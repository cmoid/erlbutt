%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Blob garbage collection — the PLAN half.
%%
%% A blob is fetched because a stored message referenced it, and nothing
%% ever removes it.  When feed_gc removes the feed that wanted it, or an
%% upload is never posted, the file stays in blobs/ for good.  This works
%% out which blobs nothing references any more and how much space they
%% hold.  It removes nothing.
%%
%% Unlike feed_gc this runs on the live node, and so will the run:
%% blob files are immutable, nothing holds an index into them, and a blob
%% that turns out to be wanted after all is wanted again by blob_fetcher
%% the moment a message naming it is stored.
%%
%% WHAT IS LIVE (a blob is kept if any one holds; the first that matches
%% is the reason reported):
%%   feeds       a message in ANY stored feed names it.  Stored, not "in
%%               the replication set" — feeds outside it are feed_gc's
%%               business, and until it removes them their blobs stay.
%%               Two sources, because each misses something:
%%                 - the links index, which decrypts private messages
%%                   addressed to us but records a reference only when
%%                   it is a field's whole value;
%%                 - a scan of every message's raw bytes, which also finds
%%                   `![img](&….sha256)` inside markdown text (380 such
%%                   refs on a 1.2M-message store) but cannot see inside
%%                   private messages.
%%   quarantine  a message in a feed held in an unpurged feed_gc
%%               quarantine names it — so `gc restore` brings feeds back
%%               WITH their images.  Purging the quarantine frees them
%%               for the next blob plan.
%%   tables      named by a table that holds blob ids for its own reasons:
%%               archive segments, validation floors, glimpses, the
%%               glimpse edge, outstanding wants.
%%   recent      modified within grace_days: an upload not yet posted, a
%%               blob fetched for a message still arriving.
%%
%% REFUSALS.  The links view must be caught up — while it refolds, a
%% private reference it has not reached yet would read as garbage.
-module(blob_gc).

-include_lib("ssb/include/ssb.hrl").
-include_lib("kernel/include/file.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([plan/1,
         report_json/1]).

-define(DEFAULT_GRACE_DAYS, 7).
-define(DEFAULT_TOP, 20).
-define(DAY, 86400).
-define(BLOB_RE, "&[A-Za-z0-9+/]{43}=\\.sha256").

%%%===================================================================
%%% API
%%%===================================================================

%% Opts (all optional): grace_days, top (all | N), now (unix seconds).
plan(Opts) ->
    Grace = maps:get(grace_days, Opts, ?DEFAULT_GRACE_DAYS),
    Top   = maps:get(top, Opts, ?DEFAULT_TOP),
    Now   = maps:get(now, Opts, erlang:system_time(second)),
    try
        ok = preflight(),
        {ok, build(Grace, Top, Now)}
    catch
        throw:{refuse, Why} -> {error, Why}
    end.

%%%===================================================================
%%% Mark
%%%===================================================================

preflight() ->
    ssb_store:available() orelse throw({refuse, store_unavailable}),
    view_manager:caught_up(ssb_links)
        orelse throw({refuse, {not_caught_up, ssb_links}}),
    ok.

build(Grace, Top, Now) ->
    {ok, Re} = re:compile(?BLOB_RE),
    Feeds = scan_dirs(Re, feed_store:feed_dirs(), linked()),
    Quar  = scan_dirs(Re, quarantined_feed_dirs(), #{}),
    Tables = table_blobs(),
    {Blobs, Unrecognised} = blobs_on_disk(),
    Cutoff = Now - Grace * ?DAY,
    Classified = [{reason(Id, MTime, Feeds, Quar, Tables, Cutoff), B}
                  || {Id, _Path, _Size, MTime} = B <- Blobs],
    Sum = fun(R) -> {N, Bytes} = lists:foldl(
                                   fun({R1, {_, _, S, _}}, {N0, B0}) when R1 =:= R ->
                                           {N0 + 1, B0 + S};
                                      (_, Acc) -> Acc
                                   end, {0, 0}, Classified),
                    #{blobs => N, bytes => Bytes}
          end,
    Candidates = [B || {garbage, B} <- Classified],
    OnDisk = sets:from_list([Id || {Id, _, _, _} <- Blobs], [{version, 2}]),
    Missing = maps:size(maps:filter(fun(Id, _) -> not sets:is_element(Id, OnDisk) end,
                                    Feeds)),
    #{at           => Now,
      grace_days   => Grace,
      blobs_on_disk => length(Blobs),
      bytes_on_disk => lists:sum([S || {_, _, S, _} <- Blobs]),
      unrecognised => Unrecognised,
      kept         => #{feeds      => Sum(feeds),
                        quarantine => Sum(quarantine),
                        tables     => Sum(tables),
                        recent     => Sum(recent)},
      candidates   => Sum(garbage),
      by_size      => size_buckets(Candidates),
      referenced_missing => Missing,
      quarantines  => length(feed_quarantines()),
      top          => top(Candidates, Top)}.

reason(Id, MTime, Feeds, Quar, Tables, Cutoff) ->
    if is_map_key(Id, Feeds)   -> feeds;
       is_map_key(Id, Quar)    -> quarantine;
       is_map_key(Id, Tables)  -> tables;
       MTime >= Cutoff         -> recent;
       true                    -> garbage
    end.

%% Blob targets of the links index — including those inside private
%% messages addressed to us, which the raw scan cannot read.
linked() ->
    maps:from_keys(
      [Id || [Id] <- checked_q("SELECT DISTINCT i.id FROM links l"
                               " JOIN link_ids i ON i.num = l.to_id"
                               " WHERE l.kind = 2")],
      true).

%% Every blob id in the raw bytes of every message in Dirs (live logs and
%% archive segments), added to Acc0.
scan_dirs(Re, Dirs, Acc0) ->
    lists:foldl(
      fun(Dir, Acc) ->
              feed_store:fold_feed(fun(Msg, A) -> scan_msg(Re, Msg, A) end,
                                   Acc, Dir)
      end, Acc0, Dirs).

scan_msg(Re, Msg, Acc) ->
    case re:run(Msg, Re, [global, {capture, all, binary}]) of
        {match, Ms} -> lists:foldl(fun([Id], A) -> A#{Id => true} end, Acc, Ms);
        nomatch     -> Acc
    end.

feed_quarantines() ->
    try feed_gc:quarantines()
    catch _:_ -> []
    end.

quarantined_feed_dirs() ->
    lists:append(
      [filelib:wildcard(filename:join([?b2l(Dir), "feeds", "*", "*"]))
       || #{dir := Dir} <- feed_quarantines()]).

%% Blob ids other tables hold for their own reasons.  A table that does
%% not exist on this node simply contributes nothing.
table_blobs() ->
    maps:from_keys(
      lists:append(
        [[Id || [Id] <- checked_q(["SELECT ", Col, " FROM ", T])]
         || {T, Col} <- [{~"archive_boundaries", "blob"},
                         {~"feed_floors",        "blob"},
                         {~"glimpses",           "blob"},
                         {~"glimpse_edge",       "blob"},
                         {~"blob_want",          "id"}],
            table_exists(T)]),
      true).

table_exists(Name) ->
    checked_q(["SELECT count(*) FROM sqlite_master WHERE type='table'"
               " AND name='", Name, "'"]) =:= [[1]].

checked_q(Sql) ->
    try ssb_store:q(Sql) of
        L when is_list(L) -> L;
        Other             -> throw({refuse, {store_read, Sql, Other}})
    catch
        throw:T  -> throw(T);
        _:Reason -> throw({refuse, {store_read, Sql, Reason}})
    end.

%%%===================================================================
%%% Blobs on disk
%%%===================================================================

%% {[{Id, Path, Size, MTime}], Unrecognised}.  Anything under blobs/
%% whose name is not a blob's is counted and left alone.
blobs_on_disk() ->
    Loc = ?b2l(config:blob_loc()),
    lists:foldl(
      fun(Path, {Ok, Bad}) ->
              case {path_blob_id(Path), file:read_file_info(Path, [{time, posix}])} of
                  {{ok, Id}, {ok, #file_info{type = regular, size = S, mtime = M}}} ->
                      {[{Id, Path, S, M} | Ok], Bad};
                  _ ->
                      {Ok, Bad + 1}
              end
      end, {[], 0}, filelib:wildcard(filename:join(Loc, "*/*"))).

%% blobs:blob_path/1 names a blob by its hash in hex WITHOUT leading
%% zeros (utils:decode_id), split after two characters — the same scheme
%% as feed directories, so the same padding back to 64 digits.
path_blob_id(Path) ->
    Hex = filename:basename(filename:dirname(Path)) ++ filename:basename(Path),
    case length(Hex) =< 64 andalso
         lists:all(fun(C) -> lists:member(C, "0123456789ABCDEF") end, Hex) of
        true ->
            Padded = lists:duplicate(64 - length(Hex), $0) ++ Hex,
            Hash = binary:decode_hex(?l2b(Padded)),
            {ok, <<"&", (base64:encode(Hash))/binary, ".sha256">>};
        false ->
            error
    end.

size_buckets(Candidates) ->
    Buckets = [{~"<100KB", 100 bsl 10}, {~"<1MB", 1 bsl 20},
               {~"<5MB", 5 bsl 20}, {~">=5MB", infinity}],
    [{Label, length(In), lists:sum([S || {_, _, S, _} <- In])}
     || {Label, Max} <- Buckets,
        In <- [[B || {_, _, S, _} = B <- Candidates,
                     S < Max, S >= lower(Label, Buckets)]]].

lower(Label, Buckets) ->
    case lists:takewhile(fun({L, _}) -> L =/= Label end, Buckets) of
        []   -> 0;
        Prev -> element(2, lists:last(Prev))
    end.

top(Candidates, Top) ->
    Sorted = lists:sort(fun({_, _, A, _}, {_, _, B, _}) -> A >= B end, Candidates),
    Some = case Top of
               all -> Sorted;
               N when is_integer(N), N >= 0 -> lists:sublist(Sorted, N)
           end,
    [#{id => Id, bytes => S, mtime => M} || {Id, _, S, M} <- Some].

%%%===================================================================
%%% JSON for admin.gc.blobs.plan
%%%===================================================================

report_json(R) ->
    Tot = fun(#{blobs := N, bytes := B}) -> {[{~"blobs", N}, {~"bytes", B}]} end,
    K = maps:get(kept, R),
    {[{~"at",           maps:get(at, R)},
      {~"graceDays",    maps:get(grace_days, R)},
      {~"blobsOnDisk",  maps:get(blobs_on_disk, R)},
      {~"bytesOnDisk",  maps:get(bytes_on_disk, R)},
      {~"unrecognised", maps:get(unrecognised, R)},
      {~"kept",         {[{~"feeds",      Tot(maps:get(feeds, K))},
                          {~"quarantine", Tot(maps:get(quarantine, K))},
                          {~"tables",     Tot(maps:get(tables, K))},
                          {~"recent",     Tot(maps:get(recent, K))}]}},
      {~"candidates",   Tot(maps:get(candidates, R))},
      {~"bySize",       [{[{~"size", L}, {~"blobs", N}, {~"bytes", B}]}
                         || {L, N, B} <- maps:get(by_size, R)]},
      {~"referencedMissing", maps:get(referenced_missing, R)},
      {~"quarantines",  maps:get(quarantines, R)},
      {~"top",          [{[{~"id", Id}, {~"bytes", B}, {~"mtime", M}]}
                         || #{id := Id, bytes := B, mtime := M} <- maps:get(top, R)]}]}.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

path_round_trip_test_() ->
    [?_test(round_trip(<<0, 0:4, 3:4, (crypto:strong_rand_bytes(30))/binary>>)),
     ?_test(round_trip(<<0:4, 9:4, (crypto:strong_rand_bytes(31))/binary>>)),
     ?_test(round_trip(crypto:strong_rand_bytes(32)))].

%% The path blobs.erl would write a blob to maps back to its id.
round_trip(Hash) ->
    Id = <<"&", (base64:encode(Hash))/binary, ".sha256">>,
    Hex = integer_to_binary(binary:decode_unsigned(Hash), 16),
    <<D2:2/binary, Rest/binary>> = Hex,
    ?assertEqual({ok, Id}, path_blob_id(filename:join(["/x/blobs", ?b2l(D2), ?b2l(Rest)]))).

not_a_blob_test() ->
    ?assertEqual(error, path_blob_id("/x/blobs/AB/.DS_Store")),
    ?assertEqual(error, path_blob_id("/x/blobs/ZZ/0123")).

%% References in markdown text count, a near miss does not.
scan_msg_test() ->
    {ok, Re} = re:compile(?BLOB_RE),
    A = <<"&", (base64:encode(crypto:strong_rand_bytes(32)))/binary, ".sha256">>,
    B = <<"&", (base64:encode(crypto:strong_rand_bytes(32)))/binary, ".sha256">>,
    Msg = iolist_to_binary(["{\"text\":\"look ![x](", A, ") and [y](", B,
                            ")\",\"mentions\":[{\"link\":\"", A, "\"}],"
                            "\"bad\":\"&short=.sha256\"}"]),
    ?assertEqual(#{A => true, B => true}, scan_msg(Re, Msg, #{})).

reason_test() ->
    F = #{~"&f" => true}, Q = #{~"&q" => true}, T = #{~"&t" => true},
    R = fun(Id, M) -> reason(Id, M, F, Q, T, 100) end,
    ?assertEqual(feeds, R(~"&f", 0)),
    ?assertEqual(quarantine, R(~"&q", 0)),
    ?assertEqual(tables, R(~"&t", 0)),
    ?assertEqual(recent, R(~"&x", 100)),
    ?assertEqual(garbage, R(~"&x", 99)).

size_buckets_test() ->
    C = [{a, p, 10, 0}, {b, p, 200 bsl 10, 0}, {c, p, 2 bsl 20, 0}, {d, p, 9 bsl 20, 0}],
    ?assertEqual([{~"<100KB", 1, 10}, {~"<1MB", 1, 200 bsl 10},
                  {~"<5MB", 1, 2 bsl 20}, {~">=5MB", 1, 9 bsl 20}],
                 size_buckets(C)).

-endif.
