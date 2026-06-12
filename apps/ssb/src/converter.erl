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
         convert/4,
         build_refs/1]).

-import(utils, [load_term/1,
                update_refs/1]).

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
            ssb_feed:store_msg(FeedPid, DecMsg),
            %% Not needed, ssb_feed_store_msg already handles it
            %%gmess_auth:put(MsgId, AuthId),
            copy_blobs(DecMsg, BlobSrc),
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

build_refs(FeedId) ->
    Feed = ssb_feed_sup:find_or_start(FeedId),
    Fun = fun(Term, Acc) ->
                  %% possibly no need to validate this here
                  Msg = message:decode(Term, false),
                  [update_refs(Msg) | Acc]
          end,
    ssb_feed:foldl(Feed, Fun, []).

-ifdef(TEST).

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
