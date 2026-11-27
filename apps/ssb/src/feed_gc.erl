%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Feed garbage collection: plan, run, restore, purge.
%%
%% A node stores every feed that was ever in its replication set, and the
%% set moves: an unfollow, a friend's unfollow two hops out, a smaller
%% `replication_hops`, a room member who left.  Nothing ever removes the
%% feeds that fell out.  This module works out which ones those are and
%% what removing them would touch (plan/1, on the running node), and
%% removes them (run/1, on a stopped one) into a quarantine that
%% restore/1 can put back and purge/1 deletes.
%%
%% WHAT IS KEPT.  Exactly what ebt replicates (ebt:compute_repl_set/0:
%% self ∪ follows(self, hops) ∪ room members ∪ pins − blocks), plus self
%% and every pin unconditionally.  Sharing the computation with ebt means
%% "what we replicate" and "what we keep" cannot drift apart.
%%
%% NO CASCADE.  The follow graph out to N hops reads only the contact
%% messages of feeds within N−1 hops, all of which are kept.  Removing
%% feeds outside the set therefore cannot shrink the set, so one pass is
%% final and a second plan straight after a run finds nothing.
%%
%% GRACE.  The graph flaps — a friend unfollows and refollows, a room
%% member reconnects — and history that is removed may not be fetchable
%% again (peers drop feeds too, and peers that persist what they think we
%% hold will not resend it).  So a feed becomes *eligible* only after it
%% has been a candidate for `grace_days`.  The plan records when each
%% candidate was first seen outside the set in a small ledger,
%% `<data>/gc/candidates`; a feed that comes back into the set leaves the
%% ledger and starts over if it falls out again.  That ledger is the only
%% thing a plan writes, and `record => false` skips even that.
%%
%% REFUSALS.  The graph and pin readers answer [] when the store cannot be
%% read — right for replication, which just replicates less for a while,
%% and exactly wrong here, where an empty keep set means "everything is
%% garbage".  So the plan checks the store itself first and refuses
%% rather than guessing: store unavailable, follow graph not caught up, a
%% table unreadable, or a keep set holding nobody but ourselves while
%% replication_hops > 0.
%%
%% WHAT A RUN WILL HAVE TO TOUCH, and so what the plan counts:
%%   - the feed directory (live log, archive segments, hints)
%%   - ingest.journal refs — compacted by copy, survivors keep their order
%%   - view_checkpoint rows — must go WITH the feed: a stale checkpoint
%%     makes view_manager drop a re-followed feed's messages as "at or
%%     under the checkpoint"
%%   - feed_floors rows (local truth, not rebuilt by anything)
%%   - feed_meta rows; other view rows are rebuilt, not counted
-module(feed_gc).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([plan/1,
         run/1,
         restore/1,
         purge/1,
         quarantines/0,
         report_json/1,
         ledger_file/0]).

%% Quarantine bookkeeping, shared with blob_gc.
-export([stamp/1,
         quarantine_dir/1,
         write_manifest/2,
         read_manifest/1,
         tree_bytes/1]).

-define(DEFAULT_GRACE_DAYS, 7).
-define(DEFAULT_TOP, 20).
-define(DAY, 86400).

%%%===================================================================
%%% API
%%%===================================================================

%% Opts (all optional):
%%   grace_days  — days a feed must have been a candidate to be eligible
%%   top         — how many of the largest candidates to list; all | N
%%   record      — update the ledger (default true)
%%   now         — unix seconds, for tests
plan(Opts) ->
    Grace  = maps:get(grace_days, Opts, ?DEFAULT_GRACE_DAYS),
    Top    = maps:get(top, Opts, ?DEFAULT_TOP),
    Record = maps:get(record, Opts, true),
    Now    = maps:get(now, Opts, erlang:system_time(second)),
    try
        ok = preflight(),
        Keep = keep_set(),
        {ok, build(Keep, Grace, Top, Record, Now)}
    catch
        throw:{refuse, Why} -> {error, Why}
    end.

ledger_file() ->
    filename:join([?b2l(config:ssb_repo_loc()), "gc", "candidates"]).

%%%===================================================================
%%% Preflight and the keep set
%%%===================================================================

preflight() ->
    ssb_store:available() orelse throw({refuse, store_unavailable}),
    view_manager:caught_up(ssb_social_graph)
        orelse throw({refuse, {not_caught_up, ssb_social_graph}}),
    %% reachable at all, and with the table the graph is built from
    _ = checked_q("SELECT count(*) FROM social_edges"),
    ok.

keep_set() ->
    Self = keys:pub_key_disp(),
    Repl = ebt:compute_repl_set(),
    case config:replication_hops() > 0 andalso lists:delete(Self, Repl) =:= [] of
        true  -> throw({refuse, empty_keep_set});
        false -> ok
    end,
    %% Pins read directly, checked: feed_pins:all/0 answers [] on error.
    Pins = [F || [F] <- checked_q("SELECT feed FROM feed_pin")],
    sets:from_list([Self | Repl] ++ Pins, [{version, 2}]).

checked_q(Sql) ->
    try ssb_store:q(Sql) of
        L when is_list(L) -> L;
        Other             -> throw({refuse, {store_read, Sql, Other}})
    catch
        throw:T     -> throw(T);
        _:Reason    -> throw({refuse, {store_read, Sql, Reason}})
    end.

%%%===================================================================
%%% The plan
%%%===================================================================

build(Keep, Grace, Top, Record, Now) ->
    {Feeds, Unrecognised} = feeds_on_disk(),
    {Candidates, Kept} = lists:partition(
                           fun({Id, _Dir}) -> not sets:is_element(Id, Keep) end,
                           Feeds),
    OldLedger = read_ledger(ledger_file()),
    {Ledger, EligibleIds} = classify([Id || {Id, _} <- Candidates],
                                     OldLedger, Now, Grace),
    Recorded = Record andalso write_ledger(ledger_file(), Ledger, Now) =:= ok,

    Stats = [feed_stats(Id, Dir) || {Id, Dir} <- Candidates],
    {JournalTotal, Refs} = journal_refs([Id || {Id, _} <- Candidates]),
    Ckpts  = count_by_feed("SELECT feed, count(*) FROM view_checkpoint"
                           " GROUP BY feed"),
    Meta   = count_by_feed("SELECT feed, count(*) FROM feed_meta"
                           " GROUP BY feed"),
    Floors = count_by_feed("SELECT feed, 1 FROM feed_floors"),
    Blocked = sets:from_list(ssb_social_graph:blocks(keys:pub_key_disp()),
                             [{version, 2}]),

    Totals = fun(Ids) ->
                 #{feeds       => sets:size(Ids),
                   bytes       => sum(Ids, Stats, bytes),
                   messages    => sum(Ids, Stats, seq),
                   journal_refs => sum_map(Ids, Refs),
                   checkpoints => sum_map(Ids, Ckpts),
                   floors      => sum_map(Ids, Floors),
                   feed_meta   => sum_map(Ids, Meta),
                   blocked     => sets:size(sets:intersection(Ids, Blocked))}
             end,
    CandSet = sets:from_list([Id || {Id, _} <- Candidates], [{version, 2}]),
    EligSet = sets:from_list(EligibleIds, [{version, 2}]),

    #{at            => Now,
      hops          => config:replication_hops(),
      grace_days    => Grace,
      keep_set      => sets:size(Keep),
      feeds_on_disk => length(Feeds),
      kept_on_disk  => length(Kept),
      kept_bytes    => lists:sum([dir_bytes(D) || {_, D} <- Kept]),
      unrecognised_dirs => Unrecognised,
      journal_refs  => JournalTotal,
      candidates    => Totals(CandSet),
      eligible      => Totals(EligSet),
      ledger        => ?l2b(ledger_file()),
      recorded      => Recorded,
      top           => top(Stats, Ledger, EligSet, Grace, Now, Top)}.

%% Which candidates are eligible, and the ledger to carry forward: a
%% candidate keeps its first-seen time, a new one starts now, and a feed
%% that is no longer a candidate drops out.
classify(CandidateIds, OldLedger, Now, Grace) ->
    Ledger = maps:from_list([{Id, maps:get(Id, OldLedger, Now)}
                             || Id <- CandidateIds]),
    Eligible = [Id || Id := First <- Ledger, Now - First >= Grace * ?DAY],
    {Ledger, Eligible}.

%%%===================================================================
%%% Feeds on disk
%%%===================================================================

feeds_on_disk() ->
    lists:foldr(fun(Dir, {Ok, Bad}) ->
                        case dir_feed_id(Dir) of
                            {ok, Id} -> {[{Id, Dir} | Ok], Bad};
                            error    -> {Ok, Bad + 1}
                        end
                end, {[], 0}, feed_store:feed_dirs()).

%% The inverse of utils:feed_dir/1, which names a feed's directory by the
%% key in hex, split after two characters — hex WITHOUT leading zeros
%% (integer_to_binary(_, 16)), so a key starting 0x0… has a short name
%% and the digits are padded back to 64 before decoding.
dir_feed_id(Dir) ->
    Hex = filename:basename(filename:dirname(Dir)) ++ filename:basename(Dir),
    case length(Hex) =< 64 andalso
         lists:all(fun(C) -> lists:member(C, "0123456789ABCDEF") end, Hex) of
        true ->
            Padded = lists:duplicate(64 - length(Hex), $0) ++ Hex,
            Key = binary:decode_hex(?l2b(Padded)),
            {ok, utils:display_pub(base64:encode(Key))};
        false ->
            error
    end.

feed_stats(Id, Dir) ->
    {Seq, Ts} = case feed_store:last_frame(Dir) of
                    {ok, Msg} ->
                        try message:decode(Msg, false) of
                            #message{sequence = S, timestamp = T}
                              when is_integer(S) -> {S, T};
                            _ -> {undefined, undefined}
                        catch _:_ -> {undefined, undefined}
                        end;
                    _ -> {undefined, undefined}
                end,
    #{id => Id, bytes => dir_bytes(Dir), seq => Seq, last => Ts}.

dir_bytes(Dir) ->
    case file:list_dir(Dir) of
        {ok, Names} ->
            lists:sum([filelib:file_size(filename:join(Dir, N)) || N <- Names]);
        _ ->
            0
    end.

%%%===================================================================
%%% Journal and derived rows
%%%===================================================================

%% {all refs, #{CandidateId => refs}}.  One pass over the journal.
journal_refs(CandidateIds) ->
    Init = maps:from_keys(CandidateIds, 0),
    ingest_journal:fold_refs(
      fun({F, _Seq}, {T, M}) ->
              case M of
                  #{F := N} -> {T + 1, M#{F := N + 1}};
                  _         -> {T + 1, M}
              end
      end, {0, Init}).

count_by_feed(Sql) ->
    maps:from_list([{F, N} || [F, N] <- checked_q(Sql)]).

sum(Ids, Stats, Key) ->
    lists:sum([V || #{id := Id, Key := V} <- Stats,
                    is_integer(V), sets:is_element(Id, Ids)]).

sum_map(Ids, Map) ->
    sets:fold(fun(Id, Acc) -> Acc + maps:get(Id, Map, 0) end, 0, Ids).

top(Stats, Ledger, EligSet, Grace, Now, Top) ->
    Sorted = lists:sort(fun(#{bytes := A}, #{bytes := B}) -> A >= B end, Stats),
    Some = case Top of
               all -> Sorted;
               N when is_integer(N), N >= 0 -> lists:sublist(Sorted, N)
           end,
    [S#{name       => name(Id),
        first_seen => maps:get(Id, Ledger, Now),
        eligible   => sets:is_element(Id, EligSet),
        grace_left => max(0, Grace * ?DAY - (Now - maps:get(Id, Ledger, Now)))}
     || #{id := Id} = S <- Some].

name(Id) ->
    case ssb_feed_meta:name(Id) of
        N when is_binary(N) -> N;
        _                   -> undefined
    end.

%%%===================================================================
%%% Ledger
%%%===================================================================

%% One line per candidate: "<feed id> <first seen, unix seconds>".
%% Anything unparseable is skipped — the worst a damaged ledger can do is
%% restart some grace periods, which errs toward keeping.
read_ledger(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            maps:from_list(
              [{Id, binary_to_integer(T)}
               || Line <- binary:split(Bin, ~"\n", [global]),
                  [Id, T] <- [binary:split(Line, ~" ")],
                  is_feed_id(Id), is_digits(T)]);
        _ ->
            #{}
    end.

write_ledger(File, Ledger, Now) ->
    Tmp = File ++ ".tmp",
    Body = [io_lib:format("# feed_gc candidates, written ~b: "
                          "<feed id> <first seen outside the keep set>~n",
                          [Now])
            | [[Id, $\s, integer_to_binary(T), $\n]
               || {Id, T} <- lists:sort(maps:to_list(Ledger))]],
    try
        ok = filelib:ensure_dir(File),
        ok = file:write_file(Tmp, Body),
        ok = file:rename(Tmp, File)
    catch _:Reason ->
        ?SSB_ERROR("feed_gc: could not write ~s: ~p", [File, Reason]),
        error
    end.

is_feed_id(<<"@", _/binary>> = Id) -> binary:longest_common_suffix([Id, ~".ed25519"]) =:= 8;
is_feed_id(_) -> false.

is_digits(<<>>) -> false;
is_digits(B)    -> lists:all(fun(C) -> C >= $0 andalso C =< $9 end, ?b2l(B)).

%%%===================================================================
%%% Run
%%%===================================================================
%%
%% OFFLINE ONLY.  A running node has feed processes holding byte-offset
%% indexes into the logs, an ingest journal open for append, and view
%% checkpoints cached in ETS that it flushes on a timer — every one of
%% which would be wrong, or would undo this, if the files moved under it.
%% run/1 therefore refuses unless those servers are absent AND this OS
%% process holds the store LOCK (sbutt takes it).
%%
%% WHAT IS REMOVED is the intersection of two independent judgements:
%%   - eligible in the ledger, i.e. a plan made BY THE RUNNING NODE (which
%%     checked that the follow graph was caught up) saw it outside the set
%%     at least grace_days ago; and
%%   - outside the keep set computed now, from the same store.
%% A refollow since the plan rescues a feed; a keep set computed wrongly
%% offline can only remove what a live plan already named.
%%
%% ORDER, and what a crash at each point leaves:
%%   1. MANIFEST written            nothing changed yet
%%   2. feed dirs moved to Q        journal refs with no feed: logged once
%%                                  per feed by createLogStream; restore
%%   3. journal compacted by copy;  old journal hard-linked into Q, new
%%      renamed over it             one renamed into place atomically
%%   4. store.db, one transaction   floors, checkpoints, feed_meta,
%%                                  msg_author rows; views marked to
%%                                  rebuild.  All or nothing.
%%   5. ledger, MANIFEST step=done
%% The manifest records each step, and run/1 refuses while any quarantine
%% is unfinished, so a crash is resolved (restore) before anything else.
%%
%% VIEWS.  Every view's other rows about removed feeds (links, threads,
%% contacts, ...) would otherwise linger, pointing at messages that are
%% gone.  By default every registered view is marked to rebuild at the
%% next start (view_version set to -1: "stored version differs").  The
%% node serves throughout; views are partial until their refold ends.
%% rebuild_views => false skips that and leaves those rows stale.

run(Opts) ->
    Grace   = maps:get(grace_days, Opts, ?DEFAULT_GRACE_DAYS),
    Rebuild = maps:get(rebuild_views, Opts, true),
    DryRun  = maps:get(dry_run, Opts, false),
    Now     = maps:get(now, Opts, erlang:system_time(second)),
    try
        ok = offline_check(),
        ssb_store:available() orelse throw({refuse, store_unavailable}),
        _ = checked_q("SELECT count(*) FROM social_edges"),
        case [S || #{stamp := S, kind := feeds, step := St} <- quarantines(),
                   St =/= done] of
            []         -> ok;
            Unfinished -> throw({refuse, {unfinished, Unfinished}})
        end,
        Keep = keep_set(),
        {Feeds, _} = feeds_on_disk(),
        Ledger = read_ledger(ledger_file()),
        Outside = [F || {Id, _} = F <- Feeds, not sets:is_element(Id, Keep)],
        Doomed = [F || {Id, _} = F <- Outside,
                       eligible(maps:get(Id, Ledger, undefined), Now, Grace)],
        Base = #{at            => Now,
                 grace_days    => Grace,
                 rebuild_views => Rebuild,
                 eligible      => length(Doomed),
                 bytes         => lists:sum([dir_bytes(D) || {_, D} <- Doomed]),
                 waiting       => length(Outside) - length(Doomed),
                 back_in_set   => length([Id || Id := _ <- Ledger,
                                                sets:is_element(Id, Keep)])},
        case Doomed =:= [] orelse DryRun of
            true  -> {ok, Base#{done => false}};
            false -> {ok, maps:merge(Base, execute(Doomed, Rebuild, Now, Ledger))}
        end
    catch
        throw:{refuse, Why} -> {error, Why}
    end.

eligible(undefined, _Now, _Grace) -> false;    %% never planned: never removed
eligible(First, Now, Grace)       -> Now - First >= Grace * ?DAY.

offline_check() ->
    case [P || P <- [view_manager, ingest_journal, ssb_feed_sup, ebt],
               whereis(P) =/= undefined] of
        []      -> ok;
        Running -> throw({refuse, {node_running, Running}})
    end,
    Me = os:getpid(),
    case store_lock:holder(config:ssb_repo_loc()) of
        {live, #{os_pid := Me}} -> ok;
        Other                   -> throw({refuse, {lock_not_held, Other}})
    end.

execute(Doomed, Rebuild, Now, Ledger) ->
    Stamp = stamp(Now),
    Q = quarantine_dir(Stamp),
    filelib:is_file(Q) andalso throw({refuse, {exists, Q}}),
    Ids = [Id || {Id, _} <- Doomed],
    M0 = #{version       => 1,
           created       => Now,
           rebuild_views => Rebuild,
           feeds         => [{Id, rel(Dir)} || {Id, Dir} <- Doomed],
           floors        => floors_of(sets:from_list(Ids, [{version, 2}])),
           step          => started},
    ok = write_manifest(Q, M0),

    {Moved, Failed} = move_feeds(Doomed, Q),
    MovedIds = [Id || {Id, _} <- Moved],
    M1 = M0#{feeds => Moved, failed => Failed, step => feeds},
    ok = write_manifest(Q, M1),

    Journal = compact_journal(Q, sets:from_list(MovedIds, [{version, 2}])),
    M2 = M1#{journal => Journal, step => journal},
    ok = write_manifest(Q, M2),

    {Step, Derived} = case forget_derived(MovedIds, Rebuild) of
                          {counts, C} -> {derived, C};
                          Err         -> {derived_failed, Err}
                      end,
    ok = write_manifest(Q, M2#{derived => Derived, step => Step}),
    Step =:= derived_failed andalso throw({refuse, {derived_failed, Stamp, Derived}}),

    _ = write_ledger(ledger_file(), maps:without(MovedIds, Ledger), Now),
    ok = write_manifest(Q, M2#{derived => Derived, step => done}),
    #{done       => true,
      stamp      => ?l2b(Stamp),
      quarantine => ?l2b(Q),
      moved      => length(Moved),
      failed     => Failed,
      journal    => Journal,
      derived    => Derived}.

%% feeds/<2>/<rest> relative to the feed store, the same under Q.
rel(Dir) ->
    filename:join(filename:basename(filename:dirname(Dir)), filename:basename(Dir)).

move_feeds(Doomed, Q) ->
    lists:foldr(
      fun({Id, Dir}, {Ok, Bad}) ->
              Rel = rel(Dir),
              Target = filename:join([Q, "feeds", Rel]),
              ok = filelib:ensure_dir(Target),
              case file:rename(Dir, Target) of
                  ok ->
                      _ = file:del_dir(filename:dirname(Dir)),  %% if now empty
                      {[{Id, Rel} | Ok], Bad};
                  {error, Why} ->
                      {Ok, [{Id, Rel, Why} | Bad]}
              end
      end, {[], []}, Doomed).

compact_journal(Q, DropSet) ->
    J = ?b2l(ingest_journal:journal_file()),
    case filelib:is_regular(J) of
        false ->
            #{present => false, kept => 0, dropped => 0};
        true ->
            New = J ++ ".gc",
            {ok, Kept, Dropped} =
                ingest_journal:compact(J, New,
                                       fun(F) -> sets:is_element(F, DropSet) end),
            Saved = filename:join(Q, "ingest.journal"),
            ok = case file:make_link(J, Saved) of
                     ok -> ok;
                     {error, _} -> {ok, _} = file:copy(J, Saved), ok
                 end,
            ok = file:rename(New, J),
            #{present => true, kept => Kept, dropped => Dropped}
    end.

%% Rows a rebuild would not bring back: floors are local truth.
floors_of(DropSet) ->
    case table_exists(~"feed_floors") of
        false -> {[], []};
        true ->
            Cols = [C || [_Cid, C | _] <- checked_q("PRAGMA table_info(feed_floors)")],
            Rows = checked_q("SELECT * FROM feed_floors"),
            FeedAt = index_of(~"feed", Cols),
            {Cols, [R || R <- Rows, sets:is_element(lists:nth(FeedAt, R), DropSet)]}
    end.

index_of(X, L) -> index_of(X, L, 1).
index_of(X, [X | _], N) -> N;
index_of(X, [_ | T], N) -> index_of(X, T, N + 1).

table_exists(Name) ->
    checked_q(["SELECT count(*) FROM sqlite_master WHERE type='table' AND name='",
               Name, "'"]) =:= [[1]].

%% One transaction: every per-feed row we know about goes, and (by
%% default) every view is marked to rebuild.  -> {counts, Map} | {error, _}
forget_derived([], _Rebuild) ->
    {counts, #{}};
forget_derived(Ids, Rebuild) ->
    In = " IN (SELECT feed FROM temp.gc_doomed)",
    Deletes = [{K, ["DELETE FROM ", T, " WHERE ", Col, In]}
               || {K, T, Col} <- [{floors,      ~"feed_floors",     "feed"},
                                  {checkpoints, ~"view_checkpoint", "feed"},
                                  {feed_meta,   ~"feed_meta",       "feed"},
                                  {msg_author,  ~"msg_author",      "author"}],
                  table_exists(T)],
    ssb_store:transaction(
      fun(Db) ->
              ok = esqlite3:exec(Db, "CREATE TEMP TABLE IF NOT EXISTS gc_doomed("
                                     "feed TEXT PRIMARY KEY) WITHOUT ROWID"),
              ok = esqlite3:exec(Db, "DELETE FROM temp.gc_doomed"),
              {ok, Ins} = esqlite3:prepare(Db, "INSERT OR IGNORE INTO "
                                               "temp.gc_doomed VALUES(?1)"),
              [begin
                   ok = esqlite3:bind(Ins, [Id]),
                   '$done' = esqlite3:step(Ins),
                   ok = esqlite3:reset(Ins)
               end || Id <- Ids],
              Counts = [begin
                            ok = esqlite3:exec(Db, iolist_to_binary(Sql)),
                            {K, esqlite3:changes(Db)}
                        end || {K, Sql} <- Deletes],
              Views = case Rebuild of
                          true ->
                              ok = esqlite3:exec(Db, "UPDATE view_version"
                                                     " SET version = -1"),
                              esqlite3:changes(Db);
                          false ->
                              0
                      end,
              ok = esqlite3:exec(Db, "DROP TABLE temp.gc_doomed"),
              {counts, maps:from_list([{views_reset, Views} | Counts])}
      end).

%%%===================================================================
%%% Restore and purge
%%%===================================================================

%% Put a quarantine back: feed dirs, the journal (merged, so refs the
%% node appended since the run are kept, after the restored history),
%% floors, and the message-id index.  View checkpoints for the restored
%% feeds were deleted by the run, so every view folds them in at the next
%% start without a rebuild.
%%
%% A feed whose directory exists again — it came back into the set and
%% was fetched afresh after the run — is left as it is, and its
%% quarantined copy stays where it is; the quarantine is removed only
%% when everything in it went back.
%%
%% Offline, like run/1, and safe to repeat after a crash.
restore(Stamp) ->
    try
        ok = offline_check(),
        ssb_store:available() orelse throw({refuse, store_unavailable}),
        Q = quarantine_dir(Stamp),
        M = read_manifest(Q),
        maps:get(kind, M, feeds) =:= feeds
            orelse throw({refuse, {not_a_feed_quarantine, Stamp}}),
        FeedLoc = ?b2l(config:feed_loc()),
        {Back, Clash} =
            lists:foldl(
              fun({Id, Rel}, {B, C}) ->
                      Src = filename:join([Q, "feeds", Rel]),
                      Dst = filename:join(FeedLoc, Rel),
                      case {filelib:is_dir(Src), filelib:is_file(Dst)} of
                          {false, _}    -> {B, C};       %% back already
                          {true, true}  -> {B, [Id | C]};
                          {true, false} ->
                              ok = filelib:ensure_dir(Dst),
                              ok = file:rename(Src, Dst),
                              {[{Id, Dst} | B], C}
                      end
              end, {[], []}, maps:get(feeds, M)),
        Refs = restore_journal(Q, maps:get(journal, M, #{present => false})),
        Floors = restore_floors(maps:get(floors, M, {[], []})),
        Authors = restore_authors(Back),
        case Clash of
            [] -> ok = file:del_dir_r(Q);
            _  -> ok
        end,
        {ok, #{stamp     => iolist_to_binary(Stamp),
               restored  => length(Back),
               clashes   => Clash,
               journal   => Refs,
               floors    => Floors,
               msg_author => Authors,
               removed_quarantine => Clash =:= []}}
    catch
        throw:{refuse, Why} -> {error, Why}
    end.

%% Steps, each resumable: merge into J.restore; rename the saved journal
%% to .merged (marks the merge as consumed); rename J.restore over J;
%% delete .merged.  Re-running after a crash finishes whichever is left.
restore_journal(_Q, #{present := false}) ->
    none;
restore_journal(Q, #{kept := Kept}) ->
    J = ?b2l(ingest_journal:journal_file()),
    Saved  = filename:join(Q, "ingest.journal"),
    Merged = Saved ++ ".merged",
    New    = J ++ ".restore",
    N = case {filelib:is_regular(Saved), filelib:is_regular(Merged)} of
            {true, _} ->
                {ok, Total} = ingest_journal:merge(Saved, J, Kept, New),
                ok = file:rename(Saved, Merged),
                Total;
            {false, true} ->
                already;
            {false, false} ->
                none
        end,
    case filelib:is_regular(New) of
        true  -> ok = file:rename(New, J);
        false -> ok
    end,
    _ = file:delete(Merged),
    N.

restore_floors({_Cols, []}) ->
    0;
restore_floors({Cols, Rows}) ->
    Ps = lists:join(",", ["?" ++ integer_to_list(I) || I <- lists:seq(1, length(Cols))]),
    Sql = iolist_to_binary(["INSERT OR IGNORE INTO feed_floors(",
                            lists:join(",", Cols), ") VALUES(", Ps, ")"]),
    ok = ssb_store:insert_many(Sql, Rows),
    length(Rows).

restore_authors(Back) ->
    lists:foldl(
      fun({_Id, Dir}, N) ->
              Rows = feed_store:fold_feed(
                       fun(Msg, Acc) ->
                               try message:decode(Msg, false) of
                                   #message{id = MId, author = A} -> [[MId, A] | Acc]
                               catch _:_ -> Acc
                               end
                       end, [], Dir),
              ok = ssb_store:insert_many("INSERT OR IGNORE INTO msg_author"
                                         "(msg_id, author) VALUES(?1, ?2)", Rows),
              N + length(Rows)
      end, 0, Back).

%% Delete a finished quarantine for good.  Touches nothing the node uses,
%% so it needs no lock.
purge(Stamp) ->
    Q = quarantine_dir(Stamp),
    case catch read_manifest(Q) of
        #{step := done} ->
            Bytes = tree_bytes(Q),
            ok = file:del_dir_r(Q),
            {ok, #{stamp => iolist_to_binary(Stamp), bytes => Bytes}};
        #{step := Step} ->
            {error, {unfinished, Step}};
        {refuse, Why} ->
            {error, Why};
        Other ->
            {error, Other}
    end.

%% Every quarantine under <data>/gc, feeds' (feed_gc) and blobs'
%% (blob_gc), oldest first.  A manifest without a kind predates blob_gc
%% and is a feed quarantine.
quarantines() ->
    Root = filename:dirname(ledger_file()),
    lists:sort(
      [#{created => maps:get(created, M),
         stamp   => ?l2b(S),
         kind    => Kind,
         count   => length(maps:get(Kind, M, [])),
         step    => maps:get(step, M),
         dir     => ?l2b(filename:join(Root, S))}
       || S <- case file:list_dir(Root) of {ok, L} -> L; _ -> [] end,
          is_stamp(S),
          M <- [catch read_manifest(filename:join(Root, S))],
          is_map(M),
          Kind <- [maps:get(kind, M, feeds)]]).

tree_bytes(Dir) ->
    filelib:fold_files(Dir, "", true,
                       fun(F, N) -> N + filelib:file_size(F) end, 0).

%%%===================================================================
%%% Quarantine bookkeeping
%%%===================================================================

stamp(Now) ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:system_time_to_universal_time(Now, second),
    lists:flatten(io_lib:format("~4..0b~2..0b~2..0bT~2..0b~2..0b~2..0bZ",
                                [Y, Mo, D, H, Mi, S])).

is_stamp(S) ->
    length(S) =:= 16 andalso lists:nth(9, S) =:= $T andalso lists:last(S) =:= $Z.

quarantine_dir(Stamp) when is_binary(Stamp) ->
    quarantine_dir(?b2l(Stamp));
quarantine_dir(Stamp) ->
    is_stamp(Stamp) orelse throw({refuse, {not_a_quarantine, Stamp}}),
    filename:join(filename:dirname(ledger_file()), Stamp).

write_manifest(Q, M) ->
    File = filename:join(Q, "MANIFEST"),
    Tmp = File ++ ".tmp",
    ok = filelib:ensure_dir(File),
    ok = file:write_file(Tmp, io_lib:format("~0tp.~n", [M])),
    ok = file:rename(Tmp, File).

read_manifest(Q) ->
    case file:consult(filename:join(Q, "MANIFEST")) of
        {ok, [#{version := 1} = M]} -> M;
        Other -> throw({refuse, {bad_manifest, Q, Other}})
    end.

%%%===================================================================
%%% JSON for admin.gc.plan
%%%===================================================================

report_json(R) ->
    {[{~"at",              maps:get(at, R)},
      {~"hops",            maps:get(hops, R)},
      {~"graceDays",       maps:get(grace_days, R)},
      {~"keepSet",         maps:get(keep_set, R)},
      {~"feedsOnDisk",     maps:get(feeds_on_disk, R)},
      {~"keptOnDisk",      maps:get(kept_on_disk, R)},
      {~"keptBytes",       maps:get(kept_bytes, R)},
      {~"unrecognisedDirs", maps:get(unrecognised_dirs, R)},
      {~"journalRefs",     maps:get(journal_refs, R)},
      {~"candidates",      totals_json(maps:get(candidates, R))},
      {~"eligible",        totals_json(maps:get(eligible, R))},
      {~"ledger",          maps:get(ledger, R)},
      {~"recorded",        maps:get(recorded, R)},
      {~"top",             [feed_json(F) || F <- maps:get(top, R)]}]}.

totals_json(T) ->
    {[{~"feeds",       maps:get(feeds, T)},
      {~"bytes",       maps:get(bytes, T)},
      {~"messages",    maps:get(messages, T)},
      {~"journalRefs", maps:get(journal_refs, T)},
      {~"checkpoints", maps:get(checkpoints, T)},
      {~"floors",      maps:get(floors, T)},
      {~"feedMeta",    maps:get(feed_meta, T)},
      {~"blocked",     maps:get(blocked, T)}]}.

feed_json(F) ->
    {[{~"id",        maps:get(id, F)},
      {~"name",      null_if_undefined(maps:get(name, F))},
      {~"bytes",     maps:get(bytes, F)},
      {~"seq",       null_if_undefined(maps:get(seq, F))},
      {~"last",      null_if_undefined(maps:get(last, F))},
      {~"firstSeen", maps:get(first_seen, F)},
      {~"eligible",  maps:get(eligible, F)},
      {~"graceLeft", maps:get(grace_left, F)}]}.

null_if_undefined(undefined) -> null;
null_if_undefined(V)         -> V.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

-define(A, ~"@AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=.ed25519").
-define(B, ~"@BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBA=.ed25519").
-define(C, ~"@CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCA=.ed25519").

%% Every key, including ones whose hex name lost leading zeros, maps back
%% from the directory utils:feed_dir/1 gives it.
dir_round_trip_test_() ->
    [?_test(round_trip(<<0:8, 0:4, 1:4, (crypto:strong_rand_bytes(30))/binary>>)),
     ?_test(round_trip(<<0:4, 7:4, (crypto:strong_rand_bytes(31))/binary>>)),
     ?_test(round_trip(<<255, (crypto:strong_rand_bytes(31))/binary>>)),
     ?_test(round_trip(crypto:strong_rand_bytes(32)))].

round_trip(Key) ->
    Id = utils:display_pub(base64:encode(Key)),
    %% utils:feed_dir/1 prefixes config:feed_loc(); only the last two path
    %% components matter, so build them the same way it does.
    <<D2:2/binary, Rest/binary>> = utils:decode_id(Id),
    Dir = filename:join(["/x/feeds", ?b2l(D2), ?b2l(Rest)]),
    ?assertEqual({ok, Id}, dir_feed_id(Dir)).

dir_not_a_feed_test() ->
    ?assertEqual(error, dir_feed_id("/x/feeds/zz/notHex")),
    ?assertEqual(error, dir_feed_id("/x/feeds/AB/" ++ lists:duplicate(70, $C))).

classify_grace_test() ->
    Now = 1_000_000,
    Old = #{?A => Now - 8 * ?DAY,       %% candidate for 8 days
            ?B => Now - 1 * ?DAY,       %% for 1 day
            ?C => Now - 30 * ?DAY},     %% was a candidate, now kept again
    {Ledger, Elig} = classify([?A, ?B, <<"@new">>], Old, Now, 7),
    ?assertEqual(#{?A => Now - 8 * ?DAY, ?B => Now - ?DAY, <<"@new">> => Now},
                 Ledger),
    ?assertEqual([?A], Elig).

classify_zero_grace_test() ->
    {_, Elig} = classify([?A, ?B], #{}, 5, 0),
    ?assertEqual([?A, ?B], lists:sort(Elig)).

ledger_round_trip_test() ->
    File = filename:join(["/tmp", "feed_gc_test_" ++
                              integer_to_list(erlang:unique_integer([positive])),
                          "candidates"]),
    L = #{?A => 100, ?B => 200},
    ok = write_ledger(File, L, 300),
    ?assertEqual(L, read_ledger(File)),
    %% damaged lines are skipped, not fatal
    {ok, Bin} = file:read_file(File),
    ok = file:write_file(File, [Bin, "garbage\n@x 12\n", ?C, " notanumber\n"]),
    ?assertEqual(L, read_ledger(File)),
    ok = file:delete(File),
    ?assertEqual(#{}, read_ledger(File)),
    _ = file:del_dir(filename:dirname(File)).

stamp_test() ->
    S = stamp(1795782900),
    ?assert(is_stamp(S)),
    ?assertEqual("20261127T123500Z", S),
    ?assertNot(is_stamp("candidates")).

%% The whole cycle on a real store: a plan's ledger names a feed, run
%% moves it out and cleans up after it, a second run finds nothing, and
%% restore puts back exactly what was there — then run + purge.
run_test_() ->
    {setup, fun gc_setup/0, fun gc_teardown/1,
     fun(_) -> [{timeout, 120, ?_test(run_restore_purge())}] end}.

gc_setup() ->
    gc_teardown(ignore),
    Home = filename:join("/tmp", "feed_gc_" ++
                             integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = ssb_store:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = room_store:start_link(),
    {ok, _} = feed_pins:start_link(),
    {ok, _} = feed_floor:start_link(),
    Home.

gc_teardown(Home) ->
    catch store_lock:release(config:ssb_repo_loc()),
    [catch gen_server:stop(N)
     || N <- [ingest_journal, ssb_feed_meta, ssb_social_graph, view_manager,
              ssb_feed_sup, blobs, feed_floor, feed_pins, room_store,
              mess_auth, ssb_store, keys, config]],
    case Home of
        ignore -> ok;
        _      -> os:cmd("rm -rf " ++ Home),
                  application:unset_env(ssb, ssb_home)
    end,
    ok.

%% The node's side: feeds, journal, views — started in the test process,
%% because eunit runs the test in a different process from the setup.
gc_start_node() ->
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = ingest_journal:start_link(),
    {ok, _} = view_manager:start_link(),
    {ok, _} = ssb_social_graph:start_link(),
    {ok, _} = ssb_feed_meta:start_link(),
    ok = gc_wait(ssb_social_graph, 250),
    ok = gc_wait(ssb_feed_meta, 250).

gc_stop_node() ->
    ok = view_manager:save(),
    [ok = gen_server:stop(N) || N <- [ssb_feed_meta, ssb_social_graph,
                                      view_manager, ingest_journal,
                                      ssb_feed_sup, blobs]],
    ok.

gc_wait(Mod, 0) -> error({never_caught_up, Mod});
gc_wait(Mod, N) ->
    case view_manager:caught_up(Mod) of
        true  -> ok;
        false -> timer:sleep(20), gc_wait(Mod, N - 1)
    end.

gc_feed(N) ->
    #{public := Pub, secret := Priv} = enacl:sign_keypair(),
    Id = <<"@", (base64:encode(Pub))/binary, ".ed25519">>,
    Pid = utils:find_or_create_feed_pid(Id),
    lists:foldl(
      fun(Seq, Prev) ->
              Content = {[{~"type", ~"post"}, {~"text", ~"gc test"}]},
              Msg = message:new_msg(Prev, Seq, Content, {Id, base64:encode(Priv)}),
              _ = ssb_feed:store_msg(Pid, Msg),
              (ssb_feed:fetch_last_msg(Pid))#message.id
      end, null, lists:seq(1, N)),
    Id.

gc_refs() ->
    lists:reverse(ingest_journal:fold_refs(fun(R, A) -> [R | A] end, [])).

gc_count(Sql, Id) ->
    [[N]] = ssb_store:q(Sql, [Id]),
    N.

run_restore_purge() ->
    ok = gc_start_node(),
    Self = keys:pub_key_disp(),
    Kept = gc_feed(3),
    Gone = gc_feed(3),
    _ = ssb_store:write("INSERT INTO social_edges(source, dest, kind, state)"
                        " VALUES(?1, ?2, 0, 1)", [Self, Kept]),
    ok = ssb_store:write("INSERT INTO feed_floors(feed, floor_seq, prev_id,"
                         " blob, state) VALUES(?1, 1, '%p', '&b', 'floored')",
                         [Gone]),
    Before = gc_refs(),
    Ckpts = "SELECT count(*) FROM view_checkpoint WHERE feed=?1",
    Floors = "SELECT count(*) FROM feed_floors WHERE feed=?1",
    Authors = "SELECT count(*) FROM msg_author WHERE author=?1",
    ?assertEqual(3, gc_count(Authors, Gone)),

    %% refused while the node is up
    ?assertMatch({error, {node_running, _}}, run(#{})),
    ok = gc_stop_node(),                 %% flushes the checkpoints
    ?assert(gc_count(Ckpts, Gone) > 0),
    ?assertMatch({error, {lock_not_held, _}}, run(#{})),
    ok = store_lock:acquire(config:ssb_repo_loc()),

    %% no plan has named it: nothing goes, whatever the grace
    ?assertMatch({ok, #{eligible := 0, waiting := 1, done := false}},
                 run(#{grace_days => 0})),
    Now = erlang:system_time(second),
    ok = write_ledger(ledger_file(), #{Gone => Now - 8 * ?DAY}, Now),
    ?assertMatch({ok, #{eligible := 0, waiting := 1}}, run(#{grace_days => 9})),
    ?assertMatch({ok, #{eligible := 1, done := false}}, run(#{dry_run => true})),
    GoneDir = ?b2l(utils:feed_dir(Gone)),
    ?assert(filelib:is_dir(GoneDir)),

    {ok, R} = run(#{}),
    ?assertMatch(#{done := true, moved := 1, failed := [],
                   journal := #{kept := 3, dropped := 3},
                   derived := #{floors := 1, msg_author := 3, feed_meta := _}},
                 R),
    #{stamp := Stamp, derived := #{checkpoints := C, views_reset := V}} = R,
    ?assert(C > 0),
    ?assert(V > 0),
    ?assertNot(filelib:is_dir(GoneDir)),
    ?assert(filelib:is_dir(?b2l(utils:feed_dir(Kept)))),
    ?assertEqual([Ref || {F, _} = Ref <- Before, F =/= Gone], gc_refs()),
    ?assertEqual(0, gc_count(Ckpts, Gone)),
    ?assertEqual(0, gc_count(Floors, Gone)),
    ?assertEqual(0, gc_count(Authors, Gone)),
    ?assert(gc_count(Ckpts, Kept) > 0),
    ?assertEqual(#{}, read_ledger(ledger_file())),
    ?assertMatch([#{stamp := Stamp, kind := feeds, step := done, count := 1}],
                 quarantines()),

    %% no cascade
    ?assertMatch({ok, #{eligible := 0, waiting := 0}}, run(#{grace_days => 0})),

    {ok, Back} = restore(Stamp),
    ?assertMatch(#{restored := 1, clashes := [], floors := 1, msg_author := 3,
                   journal := 6, removed_quarantine := true}, Back),
    ?assert(filelib:is_dir(GoneDir)),
    ?assertEqual(Before, gc_refs()),
    ?assertEqual(1, gc_count(Floors, Gone)),
    ?assertEqual(3, gc_count(Authors, Gone)),
    ?assertEqual([], quarantines()),

    %% and the other way out
    ok = write_ledger(ledger_file(), #{Gone => Now - 8 * ?DAY}, Now),
    {ok, #{stamp := S2}} = run(#{rebuild_views => false}),
    ?assertMatch({ok, #{bytes := B}} when B > 0, purge(S2)),
    ?assertEqual([], quarantines()),
    ?assertNot(filelib:is_dir(GoneDir)),
    ?assertMatch({error, _}, purge(S2)),
    ok = store_lock:release(config:ssb_repo_loc()).

-endif.
