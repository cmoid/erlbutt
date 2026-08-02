%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid

-module(converter).

%% converter is a utility that takes an existing log.offset file from the
%% javascript reference implementation of scuttlebutt and produces separate
%% feeds for each author in the log. Each feed is stored as a log file with the
%% same format, but in it's own directory along with a profile file and a references file.
%%
%% Blobs referenced by converted messages are copied from the JS blob store
%% (default ~/.ssb/blobs, layout sha256/<2 hex>/<62 hex>) into the local
%% blob store, hash-verified.  Pass `none` as BlobSrc to skip blob import.

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([convert/3,
         convert/4]).

-import(utils, [load_term/1]).

convert(OffsetLog, Sleep, Feeds)->
    convert(OffsetLog, Sleep, Feeds, default_blob_src()).

convert(OffsetLog, Sleep, Feeds, BlobSrc)->

    %% create initial store if needed, this info will come from config or
    %% environment at build time
    {ok, [[Home]]} = init:get_argument(home),
    File = Home ++ "/code/erlbutt/" ++ OffsetLog,

    case file:open(File, [read, binary]) of
        {ok, IoDev} ->
            convert_terms(IoDev, 0, Sleep, Feeds, BlobSrc),
            file:close(IoDev),
            {BiggestId, NumMsgs} =
                lists:foldl(fun(Elem, Acc) ->
                                    case count(Elem) > count(Acc) of
                                        true -> Elem;
                                        _Else -> Acc
                                    end
                            end, {~"FFF", 0}, get()),
            mess_auth:close(),
            ?LOG_INFO("number of unique feeds: ~p ~n",[length(get())]),
            ?LOG_INFO("largest feed belongs to: ~p ~n",
                  [{BiggestId, NumMsgs}]),
            report_blob_stats();
        {error, enoent} ->
            ?LOG_INFO("Probably bad input ~n",[]),
            done
    end.

default_blob_src() ->
    {ok, [[Home]]} = init:get_argument(home),
    Home ++ "/.ssb/blobs".

convert_terms(IoDev, Found, Sleep, Feeds, BlobSrc) ->
    case load_term(IoDev) of
        {ok, Data} ->
            store(Data, Sleep, Feeds, BlobSrc),
            SleepCnt = Found rem 5000 == 0,
            if SleepCnt ->
                    timer:sleep(Sleep),
                    io:format(".", []);
               true ->
                    true
            end,
            %% read spacer in file, at end this will cause eof but that will be picked
            %% up in the next iteration
            {ok, <<_PosInt:32/integer>>} = file:read(IoDev, 4),
            convert_terms(IoDev, Found + 1, Sleep, Feeds, BlobSrc);
        {error, eof} ->
            ?LOG_INFO("Found ~p messages ~n",[Found]),
            done;
        {error, Error} ->
            ?LOG_INFO("Error loading the ~p term: ~p ~n",[Found, Error])
    end.

count({_Key, Count}) when is_integer(Count) ->
    Count;
count(_) ->
    0.

get_feed(Author, Sleep) ->
    Pid = ssb_feed_sup:find_or_start(Author),
    Count = case get(Author) of
                undefined -> 1;
                N         -> N + 1
            end,
    put(Author, Count),
    PrintCount = Count rem 10000 == 0,
    if PrintCount ->
            timer:sleep(Sleep),
            io:format("~n", []),
            ?LOG_INFO("This author ~p has ~p records ~n", [Author, Count]);
       true ->
            true
    end,
    Pid.

store(Msg, Sleep, Feeds, BlobSrc) ->

    DecMsg =  message:decode(Msg, true),

    #message{id = MsgId,
             author = AuthId,
             validated = Valid} = DecMsg,

    Belongs = lists:member(AuthId, Feeds) orelse
        (hd(Feeds) == all),

    if Belongs ->
            FeedPid = get_feed(AuthId, Sleep),
            %% Blobs FIRST, then the message.  storing dispatches to
            %% social_msg, which wants every blob the message references,
            %% and blob_fetcher only skips a reference it can already see
            %% in the local store (has_local/1 -> blobs:has/1).  Import
            %% first and those wants are never raised; import after and
            %% every imported blob is recorded as wanted and broadcast to
            %% peers, because blobs:store_verified/2 has no way to retract
            %% a want -- only blob_fetcher's own fetch path clears one.
            %% (A restart repairs it, since load_wants/0 drops held ids,
            %% but until then the node begs for blobs it is sitting on.)
            %%
            %% A blob missing from the source store still becomes a want,
            %% which is what we want: that one really does have to come
            %% from a peer.
            copy_blobs(DecMsg, BlobSrc),
            ssb_feed:store_msg(FeedPid, DecMsg),
            %% Not needed, ssb_feed_store_msg already handles it
            %%gmess_auth:put(MsgId, AuthId),
            if Valid ->
                    nop;
               true ->
                    io:format("~n",[]),
                    ?LOG_INFO("Stored message that does not validate ~p ~n",[{MsgId, AuthId}])
            end;
            %% need to do this in two passes now, in order to look up message/auth pairs
            %% in the cache.
            %%check_for_refs(DecMsg, Sleep);
       true ->
            nop
    end.

%% Copy every blob the message references from the JS blob store into the
%% local one.  Blobs absent from the source store are counted and left to
%% blob_fetcher to request from peers later.
copy_blobs(_DecMsg, none) ->
    ok;
copy_blobs(#message{content = Content}, BlobSrc) ->
    lists:foreach(fun(Ref) -> copy_blob(Ref, BlobSrc) end,
                  blob_fetcher:extract_blob_refs(Content)).

copy_blob(Ref, BlobSrc) ->
    case blobs:has(Ref) of
        true ->
            ok;
        false ->
            case file:read_file(src_blob_path(Ref, BlobSrc)) of
                {ok, Data} ->
                    case blobs:store_verified(Ref, Data) of
                        ok ->
                            bump_blob_stat(copied);
                        {error, hash_mismatch} ->
                            ?LOG_INFO("source blob ~p does not match its hash ~n", [Ref]),
                            bump_blob_stat(mismatched)
                    end;
                {error, _} ->
                    bump_blob_stat(missing)
            end
    end.

%% JS ssb-blobs layout: <src>/sha256/<2 hex>/<62 hex>, lowercase and
%% zero-padded — unlike utils:decode_id/1, which strips leading zeros.
src_blob_path(<<"&", Rest/binary>>, BlobSrc) ->
    [B64, _] = binary:split(Rest, ~".sha256"),
    <<Dir:2/binary, File/binary>> = binary:encode_hex(base64:decode(B64), lowercase),
    filename:join([BlobSrc, "sha256", Dir, File]).

%% Stored as a map so the feed-count fold over get() ignores it
%% (count/1 only counts integer values).
bump_blob_stat(Key) ->
    Stats = case get(blob_stats) of
                undefined -> #{};
                M         -> M
            end,
    put(blob_stats, maps:update_with(Key, fun(N) -> N + 1 end, 1, Stats)).

report_blob_stats() ->
    case get(blob_stats) of
        undefined -> ?LOG_INFO("no blobs referenced ~n", []);
        Stats     -> ?LOG_INFO("blob import: ~p ~n", [Stats])
    end.

-ifdef(TEST).

%% Blobs must be imported BEFORE the message is stored, so that a blob the
%% source store can supply never becomes a want.  Storing dispatches to
%% social_msg, which wants every referenced blob unless blob_fetcher can
%% already see it locally; in the reverse order every imported blob is
%% recorded as wanted and broadcast to peers, and nothing retracts it
%% (blobs:store_verified/2 cannot, and only the fetch path calls
%% forget_wants/1).  A blob the source lacks SHOULD still be wanted.
%% A fixture, not a bare test: a failed assertion skips whatever follows
%% it, and these servers are registered names -- leaving them up makes
%% every later test in the module fail on {already_started,_} instead of
%% on its own merits.
blob_import_precedes_store_test_() ->
    {setup, fun conv_setup/0, fun conv_teardown/1,
     fun(SrcRoot) -> [?_test(blob_import_precedes_store(SrcRoot))] end}.

conv_setup() ->
    conv_stop(),
    Home = filename:join("/tmp", "conv_" ++
                             integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = ssb_store:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = blob_fetcher:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    filename:join(Home, "srcblobs").

conv_stop() ->
    [catch gen_server:stop(P)
     || P <- [ssb_feed_sup, blob_fetcher, blobs, mess_auth, ssb_store, config]],
    ok.

conv_teardown(SrcRoot) ->
    erase(blob_stats),
    conv_stop(),
    os:cmd("rm -rf " ++ filename:dirname(SrcRoot)),
    application:unset_env(ssb, ssb_home),
    ok.

blob_import_precedes_store(SrcRoot) ->
    %% one blob the JS store has, one it does not
    Payload  = <<"converter ordering payload ",
                 (binary:encode_hex(crypto:strong_rand_bytes(8)))/binary>>,
    Present  = <<"&", (base64:encode(crypto:hash(sha256, Payload)))/binary,
                 ".sha256">>,
    Absent   = <<"&", (base64:encode(crypto:hash(sha256,
                     <<"absent ", (binary:encode_hex(
                         crypto:strong_rand_bytes(8)))/binary>>)))/binary,
                 ".sha256">>,
    SrcPath  = src_blob_path(Present, SrcRoot),
    ok = filelib:ensure_dir(SrcPath),
    ok = file:write_file(SrcPath, Payload),

    %% A raw frame as convert/4 would read it.  The signature is junk, so
    %% validation fails -- store/4 stores anyway and only logs, which is
    %% what makes this cheap to build.  The author must still be a real
    %% key shape: utils:feed_dir/1 base64-decodes it to name the directory.
    Author = <<"@", (base64:encode(crypto:hash(sha256,
                                               ~"converter ordering author")))/binary,
               ".ed25519">>,
    Raw = iolist_to_binary(
            ["{\"key\":\"%ordering.sha256\",\"value\":{",
             "\"previous\":null,\"author\":\"", Author, "\",",
             "\"sequence\":1,\"timestamp\":0,\"hash\":\"sha256\",",
             "\"content\":{\"type\":\"post\",\"text\":\"pic\",\"mentions\":[",
             "{\"link\":\"", Present, "\"},{\"link\":\"", Absent, "\"}]},",
             "\"signature\":\"nope.sig.ed25519\"}}"]),

    erase(blob_stats),
    store(Raw, 0, [all], SrcRoot),

    %% imported, and therefore never wanted -- this is the ordering
    %% assertion.  assertEqual rather than assertNot: this is the one most
    %% likely to fire, and rebar3's vendored eunit_progress crashes trying
    %% to format an assertNot failure, burying the result.
    ?assert(blobs:has(Present)),
    ?assertEqual(false, lists:member(Present, blob_fetcher:wanted())),
    %% not importable, so it must still be wanted
    ?assertEqual(true, lists:member(Absent, blob_fetcher:wanted())),
    erase(Author).

%% copy_blob/2 imports a blob from a JS-layout source store into the local
%% store, verifying the hash; a missing source blob is counted, not fatal.
%% Unique payloads keep reruns independent (_build/test/blobs/ persists).
copy_blob_test() ->
    ConfigStarted = case whereis(config) of
        undefined -> {ok, _} = config:start_link("test/ssb.cfg"), true;
        _         -> false
    end,
    {ok, BlobsPid} = blobs:start_link(),

    %% random, not unique_integer — the latter restarts per VM run and can
    %% collide with a blob persisted in _build/test/blobs by an earlier run
    Payload = <<"converter blob import payload ",
                (binary:encode_hex(crypto:strong_rand_bytes(8)))/binary>>,
    Hash = crypto:hash(sha256, Payload),
    Ref  = <<"&", (base64:encode(Hash))/binary, ".sha256">>,

    %% lay the blob out like ~/.ssb/blobs does
    SrcRoot = "./_build/test/srcblobs",
    SrcPath = src_blob_path(Ref, SrcRoot),
    ok = filelib:ensure_dir(SrcPath),
    ok = file:write_file(SrcPath, Payload),

    erase(blob_stats),
    copy_blob(Ref, SrcRoot),
    ?assert(blobs:has(Ref)),
    ?assertEqual(#{copied => 1}, get(blob_stats)),

    %% a blob absent from the source store is just counted
    MissingRef = <<"&", (base64:encode(crypto:hash(sha256,
                       <<"absent ", (binary:encode_hex(crypto:strong_rand_bytes(8)))/binary>>)))/binary,
                   ".sha256">>,
    copy_blob(MissingRef, SrcRoot),
    ?assertNot(blobs:has(MissingRef)),
    ?assertEqual(#{copied => 1, missing => 1}, get(blob_stats)),

    erase(blob_stats),
    gen_server:stop(BlobsPid),
    case ConfigStarted of
        true  -> gen_server:stop(config);
        false -> ok
    end.

-endif.
