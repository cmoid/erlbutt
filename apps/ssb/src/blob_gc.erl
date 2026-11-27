%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Blob garbage collection: plan, run, restore (purge is feed_gc's, which
%% deletes any finished quarantine).
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
         run/1,
         restore/1,
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

%% {Blobs on disk, [{Reason, Blob}], ids stored feeds reference,
%%  unrecognised file count}
mark(Grace, Now) ->
    {ok, Re} = re:compile(?BLOB_RE),
    Feeds = scan_dirs(Re, feed_store:feed_dirs(), linked()),
    Quar  = scan_dirs(Re, quarantined_feed_dirs(), #{}),
    Tables = table_blobs(),
    {Blobs, Unrecognised} = blobs_on_disk(),
    Cutoff = Now - Grace * ?DAY,
    Classified = [{reason(Id, MTime, Feeds, Quar, Tables, Cutoff), B}
                  || {Id, _Path, _Size, MTime} = B <- Blobs],
    {Blobs, Classified, Feeds, Unrecognised}.

build(Grace, Top, Now) ->
    {Blobs, Classified, Feeds, Unrecognised} = mark(Grace, Now),
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
      quarantines  => length([Q || #{kind := feeds} = Q <- feed_quarantines()]),
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
       || #{dir := Dir, kind := feeds} <- feed_quarantines()]).

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
%%% Run
%%%===================================================================
%%
%% ONLINE.  Marks exactly as plan/1 does, then moves each candidate into
%% <data>/gc/<STAMP>/blobs/<2>/<rest> — a rename, so a peer mid-way
%% through reading one keeps its open file.
%%
%% THE WINDOW.  The mark reads every stored message, which takes the best
%% part of a minute, and messages keep arriving.  One that names a
%% candidate in that time finds the blob still present, so blob_fetcher
%% does not want it — and then it is moved.  So after moving, the links
%% index is asked again about every blob moved, and any it now references
%% goes straight back.  What that cannot catch is a reference arriving in
%% the window that only appears inside text; such a blob is in the
%% quarantine, where `gc restore` finds it.
%%
%% One run at a time (a global lock), and a run is refused while another
%% blob quarantine is unfinished.
%%
%% Opts: grace_days, dry_run, now.
run(Opts) ->
    Grace  = maps:get(grace_days, Opts, ?DEFAULT_GRACE_DAYS),
    DryRun = maps:get(dry_run, Opts, false),
    Now    = maps:get(now, Opts, erlang:system_time(second)),
    Lock = {?MODULE, self()},
    case global:set_lock(Lock, [node()], 0) of
        false ->
            {error, already_running};
        true ->
            try
                ok = preflight(),
                case [S || #{kind := blobs, step := St, stamp := S} <- feed_quarantines(),
                           St =/= done] of
                    []         -> ok;
                    Unfinished -> throw({refuse, {unfinished, Unfinished}})
                end,
                {_Blobs, Classified, _Feeds, _} = mark(Grace, Now),
                Candidates = [B || {garbage, B} <- Classified],
                Base = #{at         => Now,
                         grace_days => Grace,
                         candidates => length(Candidates),
                         bytes      => lists:sum([S || {_, _, S, _} <- Candidates])},
                case Candidates =:= [] orelse DryRun of
                    true  -> {ok, Base#{done => false}};
                    false -> {ok, maps:merge(Base, execute(Candidates, Now))}
                end
            catch
                throw:{refuse, Why} -> {error, Why}
            after
                global:del_lock(Lock, [node()])
            end
    end.

execute(Candidates, Now) ->
    Stamp = feed_gc:stamp(Now),
    Q = feed_gc:quarantine_dir(Stamp),
    filelib:is_file(Q) andalso throw({refuse, {exists, Q}}),
    M0 = #{version => 1, kind => blobs, created => Now, step => started,
           blobs => [{Id, rel(Path), S} || {Id, Path, S, _} <- Candidates]},
    ok = feed_gc:write_manifest(Q, M0),
    {Moved, Failed} =
        lists:foldr(
          fun({Id, Path, S, _}, {Ok, Bad}) ->
                  Target = filename:join([Q, "blobs", rel(Path)]),
                  ok = filelib:ensure_dir(Target),
                  case file:rename(Path, Target) of
                      ok           -> {[{Id, rel(Path), S} | Ok], Bad};
                      {error, Why} -> {Ok, [{Id, Why} | Bad]}
                  end
          end, {[], []}, Candidates),
    %% referenced while we worked: put back, and keep out of the manifest
    Linked = linked(),
    {Back, Kept} = lists:partition(fun({Id, _, _}) -> is_map_key(Id, Linked) end,
                                   Moved),
    Loc = ?b2l(config:blob_loc()),
    [ok = file:rename(filename:join([Q, "blobs", Rel]), filename:join(Loc, Rel))
     || {_, Rel, _} <- Back],
    ok = feed_gc:write_manifest(Q, M0#{blobs => Kept, failed => Failed,
                                       put_back => [Id || {Id, _, _} <- Back],
                                       step => done}),
    #{done       => true,
      stamp      => ?l2b(Stamp),
      quarantine => ?l2b(Q),
      moved      => length(Kept),
      moved_bytes => lists:sum([S || {_, _, S} <- Kept]),
      put_back   => length(Back),
      failed     => Failed}.

%% blobs/<2>/<rest> relative to the blob store, the same under Q.
rel(Path) ->
    filename:join(filename:basename(filename:dirname(Path)), filename:basename(Path)).

%% Put a blob quarantine back.  Online.  A blob that is in the store again
%% (fetched afresh since) is the same bytes — it is content-addressed —
%% so the quarantined copy is simply dropped.  The quarantine is removed
%% once empty.
restore(Stamp) ->
    try
        Q = feed_gc:quarantine_dir(Stamp),
        M = feed_gc:read_manifest(Q),
        maps:get(kind, M, feeds) =:= blobs
            orelse throw({refuse, {not_a_blob_quarantine, Stamp}}),
        Loc = ?b2l(config:blob_loc()),
        {Back, Dup} =
            lists:foldl(
              fun({_Id, Rel, _S}, {B, D}) ->
                      Src = filename:join([Q, "blobs", Rel]),
                      Dst = filename:join(Loc, Rel),
                      case {filelib:is_regular(Src), filelib:is_regular(Dst)} of
                          {false, _}    -> {B, D};
                          {true, true}  -> ok = file:delete(Src), {B, D + 1};
                          {true, false} ->
                              ok = filelib:ensure_dir(Dst),
                              ok = file:rename(Src, Dst),
                              {B + 1, D}
                      end
              end, {0, 0}, maps:get(blobs, M, [])),
        ok = file:del_dir_r(Q),
        {ok, #{stamp => iolist_to_binary(Stamp), restored => Back,
               already_present => Dup}}
    catch
        throw:{refuse, Why} -> {error, Why}
    end.

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

%% On a real store: a blob a message mentions, one named only inside its
%% text, one nothing names and one nothing names but that is new.  Only
%% the third is moved, and restore puts it back.
run_test_() ->
    {setup, fun bg_setup/0, fun bg_teardown/1,
     fun(_) -> [{timeout, 60, ?_test(run_and_restore())}] end}.

bg_setup() ->
    bg_teardown(ignore),
    Home = filename:join("/tmp", "blob_gc_" ++
                             integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = ssb_store:start_link(),
    {ok, _} = mess_auth:start_link(),
    Home.

bg_teardown(Home) ->
    [catch gen_server:stop(N)
     || N <- [ssb_links, view_manager, ingest_journal, ssb_feed_sup, blobs,
              mess_auth, ssb_store, keys, config]],
    case Home of
        ignore -> ok;
        _      -> os:cmd("rm -rf " ++ Home),
                  application:unset_env(ssb, ssb_home)
    end,
    ok.

run_and_restore() ->
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = ingest_journal:start_link(),
    {ok, _} = view_manager:start_link(),
    {ok, _} = ssb_links:start_link(),
    ok = bg_wait(250),
    [Mentioned, InText, Orphan, Fresh] =
        [blobs:store(crypto:strong_rand_bytes(1000 + I)) || I <- lists:seq(1, 4)],
    #{public := Pub, secret := Priv} = enacl:sign_keypair(),
    Id = <<"@", (base64:encode(Pub))/binary, ".ed25519">>,
    Content = {[{~"type", ~"post"},
                {~"text", <<"look ![x](", InText/binary, ")">>},
                {~"mentions", [{[{~"link", Mentioned}]}]}]},
    _ = ssb_feed:store_msg(utils:find_or_create_feed_pid(Id),
                           message:new_msg(null, 1, Content,
                                           {Id, base64:encode(Priv)})),
    ok = bg_wait(250),
    %% everything but Fresh last changed long ago
    Old = erlang:system_time(second) - 30 * ?DAY,
    {Blobs, _} = blobs_on_disk(),
    [ok = file:change_time(P, calendar:system_time_to_local_time(Old, second))
     || {B, P, _, _} <- Blobs, B =/= Fresh],
    Path = fun(B) -> hd([P || {X, P, _, _} <- Blobs, X =:= B]) end,

    ?assertMatch({ok, #{candidates := #{blobs := 1},
                        kept := #{feeds := #{blobs := 2}, recent := #{blobs := 1}},
                        top := [#{id := Orphan}]}},
                 plan(#{})),
    ?assertMatch({ok, #{candidates := 1, done := false}}, run(#{dry_run => true})),
    ?assert(filelib:is_regular(Path(Orphan))),

    {ok, R} = run(#{}),
    ?assertMatch(#{done := true, moved := 1, put_back := 0, failed := []}, R),
    #{stamp := Stamp} = R,
    ?assertNot(filelib:is_regular(Path(Orphan))),
    [?assert(filelib:is_regular(Path(B))) || B <- [Mentioned, InText, Fresh]],
    ?assertMatch([#{kind := blobs, count := 1, step := done}],
                 [Q || #{stamp := S} = Q <- feed_gc:quarantines(), S =:= Stamp]),
    ?assertMatch({ok, #{candidates := 0}}, run(#{})),
    %% a feed restore refuses a blob quarantine
    ?assertMatch({error, _}, catch feed_gc:restore(Stamp)),

    ?assertMatch({ok, #{restored := 1, already_present := 0}}, restore(Stamp)),
    ?assert(filelib:is_regular(Path(Orphan))),
    ?assertEqual([], [Q || #{stamp := S} = Q <- feed_gc:quarantines(), S =:= Stamp]),

    %% and the other way out
    {ok, #{stamp := S2}} = run(#{}),
    ?assertMatch({ok, _}, feed_gc:purge(S2)),
    ?assertNot(filelib:is_regular(Path(Orphan))).

bg_wait(0) -> error(never_caught_up);
bg_wait(N) ->
    case view_manager:caught_up(ssb_links) of
        true  -> ok;
        false -> timer:sleep(20), bg_wait(N - 1)
    end.

size_buckets_test() ->
    C = [{a, p, 10, 0}, {b, p, 200 bsl 10, 0}, {c, p, 2 bsl 20, 0}, {d, p, 9 bsl 20, 0}],
    ?assertEqual([{~"<100KB", 1, 10}, {~"<1MB", 1, 200 bsl 10},
                  {~"<5MB", 1, 2 bsl 20}, {~">=5MB", 1, 9 bsl 20}],
                 size_buckets(C)).

-endif.
