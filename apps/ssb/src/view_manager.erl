%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Lifecycle manager for ssb_view materialized views (the flume role):
%% per-view/per-feed sequence checkpoints, catch-up replay at
%% registration, full rebuild when a view's version bumps, synchronous
%% fan-out of every stored message, and change-event publication.
%%
%% Replay and rebuild fold the per-feed logs (archived .gz segments in
%% sequence order, then the live log.offset of each feed) — the real
%% store.  The global log.offset is NOT used: it predates nothing and
%% guarantees nothing (in practice it holds only a fraction of the
%% per-feed history), and it is slated for retirement.  Feeds are
%% folded independently; per-author order is all a view may rely on,
%% which is sufficient for SSB content semantics (only my messages
%% assert my follows/abouts).  Views needing arrival order across
%% feeds will use the future ingest journal (doc/plugin-architecture.md).
%%
%% Events: a view's view_entry/1 may return {events, [Event]}; each is
%% published as {view_event, ViewMod, Event} to processes that joined
%% via subscribe/1 (a pg group in the ssb_views scope, whose process is
%% started here and lives under this server).
%%
%% Checkpoints live in ssb_store, but ETS stays the working copy.  That is
%% deliberate: deliver/2 consults and advances a checkpoint once per
%% message PER VIEW, so with nine views a write-through design would put
%% nine round trips on the ingest path and on every message of every
%% catch-up fold.  Instead the ETS table is authoritative in memory, a
%% dirty set records which {view, feed} pairs have moved, and the periodic
%% flush writes only those — O(what changed), where the ets:tab2file
%% snapshot this replaced was O(views x feeds) rewritten every minute.
%%
%% The flush interval means a checkpoint can lag the view rows it
%% describes after a crash.  That asymmetry is safe and stays safe: it
%% only ever runs one way (rows ahead of checkpoint, never behind), and a
%% view then refolds messages it has already folded, which is idempotent
%% per {feed, seq}.  What the move does remove is the split-brain case —
%% checkpoints claiming coverage of a store that was deleted underneath
%% them — because both now live or die in the same file.
-module(view_manager).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

%% API
-export([start_link/0,
         register_view/1,
         rebuild/1,
         ingest/1,
         subscribe/1,
         unsubscribe/1,
         notify/2,
         checkpoint/2,
         views/0,
         views/1,
         info/0,
         caught_up/1,
         save/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CKPT, ssb_view_checkpoints).
-define(DIRTY, ssb_view_checkpoints_dirty).
-define(PG_SCOPE, ssb_views).
-define(SAVE_EVERY_MS, 60_000).

-define(SCHEMA_VERSION, 1).
-define(DDL,
        ["CREATE TABLE IF NOT EXISTS view_checkpoint("
         "  view TEXT NOT NULL,"
         "  feed TEXT NOT NULL,"
         "  seq  INTEGER NOT NULL,"
         "  PRIMARY KEY (view, feed)) WITHOUT ROWID;",
         "CREATE TABLE IF NOT EXISTS view_version("
         "  view    TEXT PRIMARY KEY,"
         "  version INTEGER NOT NULL) WITHOUT ROWID;"]).
%% Feeds folded per catch-up chunk, and the sweep count past which we stop
%% chasing a store that is growing faster than we can fold it.
-define(CATCH_UP_FEEDS, 64).
-define(CATCH_UP_MAX_PASSES, 8).

%% Registered views as [{Mod, core | app}], in delivery order: every core
%% view before every app view, each group in registration order.  Core
%% views are protocol infrastructure an app view may fold against, so they
%% must see a message first (doc/persistence.md §5).
-record(vm_state, {views = [],
                   %% #{Mod => true} for views whose catch-up fold is
                   %% still running; they receive no ingests until done.
                   catching = #{}}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Register a view module.  If its stored version matches
%% Mod:view_version() and Mod:view_load() reports surviving state, the
%% view is caught up from its checkpoints; otherwise it is reset and
%% refolded from the whole log.
%%
%% Returns as soon as the catch-up is SCHEDULED, not when it finishes —
%% the fold runs in chunks in this server's message loop so that ingest
%% and every other call keep being served while it runs.  Poll
%% caught_up/1 if you need the view complete.
register_view(Mod) when is_atom(Mod) ->
    gen_server:call(?SERVER, {register_view, Mod}, infinity).

%% Wipe a registered view's derived state and refold it from the whole
%% log — the recovery hammer for a corrupted or suspect index.  Returns
%% as soon as the refold has been scheduled; poll caught_up/1 to know
%% when it has finished.
rebuild(Mod) when is_atom(Mod) ->
    gen_server:call(?SERVER, {rebuild, Mod}, infinity).

%% Fold one just-stored message into every registered view.  Called
%% synchronously from ssb_feed:store/2 so views are current when the
%% store returns (the same contract ssb_social_graph:update/3 used to have).
%% A no-op when the manager is not running.
ingest(#message{} = Msg) ->
    try gen_server:call(?SERVER, {ingest, Msg}, infinity)
    catch exit:{noproc, _} -> ok
    end.

%% Receive {view_event, ViewMod, Event} messages for a view's changes.
subscribe(ViewMod) ->
    ok = pg:join(?PG_SCOPE, {view, ViewMod}, self()).

unsubscribe(ViewMod) ->
    ok = pg:leave(?PG_SCOPE, {view, ViewMod}, self()).

%% Send an ad-hoc Event to a view's subscribers, exactly as a view's own
%% change events are delivered.  Lets non-view producers (e.g. a periodic
%% heartbeat) drive a live_source over the same pg mechanism.  Runs in
%% the caller's process — no view_manager round-trip.
notify(ViewMod, Event) ->
    publish(ViewMod, [Event]).

%% The highest sequence of FeedId delivered to ViewMod (0 if none).
%% Reads the protected checkpoint table directly.
checkpoint(ViewMod, FeedId) ->
    try ets:lookup(?CKPT, {ViewMod, feed, FeedId}) of
        [{_, Seq}] -> Seq;
        []         -> 0
    catch error:badarg -> 0
    end.

%% Has Mod finished its catch-up fold?  Registration returns before the
%% fold does, so anything that needs the view complete (a test, a status
%% display) asks here.
caught_up(Mod) ->
    try gen_server:call(?SERVER, {caught_up, Mod}, infinity)
    catch exit:{noproc, _} -> false
    end.

%% Registered views in delivery order (core first).
views() ->
    views(any).

%% Registered views of one class — `core`, `app`, or `any`.  Lets an
%% application assert that the core views it builds on are present.
views(Class) when Class =:= core; Class =:= app; Class =:= any ->
    try gen_server:call(?SERVER, {views, Class}, infinity)
    catch exit:{noproc, _} -> []
    end.

%% [{Mod, Class, Version, FeedsCheckpointed}] for every registered view,
%% in delivery order.  An accessor rather than letting callers read the
%% checkpoint table directly — it is owned by this process and other apps
%% (admin) should not depend on its shape.
info() ->
    try gen_server:call(?SERVER, info, infinity)
    catch exit:{noproc, _} -> []
    end.

%% Flush every view's durable state and the checkpoint table to disk.
save() ->
    gen_server:call(?SERVER, save, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    %% The pg scope is started with the first manager; it survives a
    %% manager restart (normal exits do not propagate through the link).
    case pg:start_link(?PG_SCOPE) of
        {ok, _}                        -> ok;
        {error, {already_started, _}}  -> ok
    end,
    ets:new(?DIRTY, [named_table, protected, set]),
    load_ckpt(),
    erlang:send_after(?SAVE_EVERY_MS, self(), save_tick),
    {ok, #vm_state{}}.

handle_call({register_view, Mod}, _From, #vm_state{views = Views} = State) ->
    case lists:keymember(Mod, 1, Views) of
        true ->
            {reply, ok, State};
        false ->
            Class   = ssb_view:class(Mod),
            StoredV = stored_version(Mod),
            CodeV   = Mod:view_version(),
            State1 =
                case StoredV =:= CodeV andalso Mod:view_load() =:= ok of
                    true ->
                        start_catch_up(Mod, State);
                    false ->
                        ?SSB_INFO("view_manager: rebuilding ~p ~p "
                                  "(stored ~p, code ~p)",
                                  [Class, Mod, StoredV, CodeV]),
                        reset_view(Mod),
                        start_catch_up(Mod, State)
                end,
            {reply, ok, State1#vm_state{views = insert_view(Mod, Class, Views)}}
    end;

handle_call({rebuild, Mod}, _From, #vm_state{views = Views} = State) ->
    case lists:keymember(Mod, 1, Views) of
        true ->
            reset_view(Mod),
            {reply, ok, start_catch_up(Mod, State)};
        false ->
            {reply, {error, not_registered}, State}
    end;

handle_call({caught_up, Mod}, _From, #vm_state{catching = Catching} = State) ->
    {reply, not maps:is_key(Mod, Catching), State};

handle_call({views, Class}, _From, #vm_state{views = Views} = State) ->
    {reply, [M || {M, C} <- Views, Class =:= any orelse C =:= Class], State};

handle_call(info, _From, #vm_state{views = Views} = State) ->
    {reply, [{Mod, Class, stored_version(Mod), feeds_checkpointed(Mod)}
             || {Mod, Class} <- Views], State};

%% A view still catching up is deliberately skipped: its fold reads the
%% store, and store/2 writes the message before calling here, so the fold
%% will see it.  Delivering it now would be actively wrong — deliver/2
%% would advance the checkpoint to this sequence, and every earlier
%% message the fold has not reached yet would then be skipped as
%% already-covered, leaving a permanent hole in the view.
handle_call({ingest, Msg}, _From,
            #vm_state{views = Views, catching = Catching} = State) ->
    [deliver(Mod, Msg) || {Mod, _Class} <- Views,
                          not maps:is_key(Mod, Catching)],
    {reply, ok, State};

handle_call(save, _From, #vm_state{views = Views} = State) ->
    save_all(Views),
    {reply, ok, State}.

%%%===================================================================
%%% Chunked catch-up
%%%===================================================================
%%
%% catch_up used to run inside the register_view call, which meant the
%% manager was blocked for the whole fold — and because ssb_feed:store/2
%% calls ingest/1 synchronously, replication stalled with it.  Measured
%% at ~28 s for a 2.5M-message store (doc/persistence.md §8), with every
%% other view's registration queued behind it.
%%
%% Now registration returns immediately and the fold runs in the
%% manager's own message loop, a few feeds at a time, so ingest and the
%% other calls interleave with it.
%%
%% Correctness comes from two rules: a catching-up view receives no
%% ingests (see handle_call({ingest, ...})), and the fold keeps sweeping
%% until a whole pass delivers nothing.  The second rule closes the race
%% where a feed the sweep has already passed gains a message while later
%% feeds are still being folded.

start_catch_up(Mod, #vm_state{catching = Catching} = State) ->
    self() ! {catch_up, Mod, feed_store:feed_dirs(), 0, 1,
              erlang:monotonic_time(millisecond)},
    State#vm_state{catching = Catching#{Mod => true}}.

%% Fold one chunk of feeds, then either continue, sweep again, or finish.
do_catch_up(Mod, Dirs, Delivered, Pass, T0, State) ->
    {Chunk, Rest} = split_chunk(chunk_size(), Dirs),
    N = lists:foldl(fun(Dir, Acc) -> Acc + fold_one_feed(Mod, Dir) end,
                    0, Chunk),
    case Rest of
        [] -> finish_pass(Mod, Delivered + N, Pass, T0, State);
        _  -> self() ! {catch_up, Mod, Rest, Delivered + N, Pass, T0},
              State
    end.

%% A pass that delivered nothing means nothing was missed: done.  A pass
%% that delivered something may have raced with an append to a feed it
%% had already visited, so sweep again — cheap, because caught_up_feed/2
%% skips a whole feed on one tail read.
finish_pass(Mod, 0, Pass, T0, #vm_state{catching = Catching} = State) ->
    ?SSB_INFO("view_manager: ~p caught up in ~p ms (~p pass(es))",
              [Mod, erlang:monotonic_time(millisecond) - T0, Pass]),
    ok = Mod:view_save(),
    persist_ckpt(),
    State#vm_state{catching = maps:remove(Mod, Catching)};
finish_pass(Mod, N, Pass, T0, State) when Pass >= ?CATCH_UP_MAX_PASSES ->
    %% Still moving after this many sweeps: the store is being written
    %% faster than we fold.  Stop sweeping and let ingest take over —
    %% anything missed is picked up by the next run's catch-up.
    ?SSB_ERROR("view_manager: ~p still delivering (~p) after ~p passes; "
               "accepting it as caught up", [Mod, N, Pass]),
    finish_pass(Mod, 0, Pass, T0, State);
finish_pass(Mod, _N, Pass, T0, State) ->
    self() ! {catch_up, Mod, feed_store:feed_dirs(), 0, Pass + 1, T0},
    State.

fold_one_feed(Mod, Dir) ->
    case caught_up_feed(Mod, Dir) of
        true  -> 0;
        false -> feed_store:fold_feed(
                   fun(Data, Acc) -> Acc + deliver_raw(Mod, Data) end, 0, Dir)
    end.

split_chunk(N, List) ->
    case length(List) =< N of
        true  -> {List, []};
        false -> lists:split(N, List)
    end.

%% Feeds per chunk.  Configurable so a test can force the fold to span
%% several turns of the message loop, which is the only way to actually
%% exercise a store landing mid-catch-up.
chunk_size() ->
    application:get_env(ssb, view_catch_up_feeds, ?CATCH_UP_FEEDS).

%% Append Mod to its class group: core views ahead of every app view,
%% within a group in registration order.
insert_view(Mod, core, Views) ->
    {Core, App} = lists:splitwith(fun({_, C}) -> C =:= core end, Views),
    Core ++ [{Mod, core}] ++ App;
insert_view(Mod, app, Views) ->
    Views ++ [{Mod, app}].

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({catch_up, Mod, Dirs, Delivered, Pass, T0}, State) ->
    {noreply, do_catch_up(Mod, Dirs, Delivered, Pass, T0, State)};

handle_info(save_tick, #vm_state{views = Views} = State) ->
    save_all(Views),
    erlang:send_after(?SAVE_EVERY_MS, self(), save_tick),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #vm_state{views = Views}) ->
    save_all(Views),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Drop a view's derived state and its checkpoints so the catch-up that
%% follows refolds from the start of the log.  The version is stamped
%% here, not after the fold, so a crash mid-rebuild leaves the view empty
%% with no checkpoints — which reads as "rebuild me" next time, not as
%% "complete".
%% The store rows go too, and synchronously — a rebuild interrupted before
%% the next flush must not come back to checkpoints describing state that
%% view_reset/0 has already thrown away.  That is the one direction the
%% lag is not safe in, so it is the one write that does not wait.
reset_view(Mod) ->
    ok = Mod:view_reset(),
    ets:match_delete(?CKPT, {{Mod, feed, '_'}, '_'}),
    ets:match_delete(?DIRTY, {{Mod, '_'}}),
    catch ssb_store:write("DELETE FROM view_checkpoint WHERE view=?1",
                          [atom_to_binary(Mod, utf8)]),
    Version = Mod:view_version(),
    ets:insert(?CKPT, {{Mod, version}, Version}),
    store_version(Mod, Version),
    ok.

%% True when Mod's checkpoint already covers this feed's last message, so
%% the whole feed can be skipped without folding it.  Determined from a
%% single cheap tail read; conservatively false on any doubt (unreadable
%% tail, undecodable message), which just means the feed is folded as
%% before.  Safe across rebuilds: rebuild_view clears the checkpoints
%% first, so every feed reads back as not-caught-up and is refolded.
caught_up_feed(Mod, Dir) ->
    case feed_store:last_frame(Dir) of
        {ok, Msg} ->
            try message:decode(Msg, false) of
                #message{author = FeedId, sequence = Seq}
                  when is_binary(FeedId), is_integer(Seq) ->
                    checkpoint(Mod, FeedId) >= Seq;
                _ ->
                    false
            catch _:_ ->
                    false
            end;
        unknown ->
            false
    end.

deliver_raw(Mod, Data) ->
    try message:decode(Data, false) of
        #message{} = Msg -> deliver(Mod, Msg)
    catch _:_ -> 0
    end.

%% Deliver Msg to Mod if it is beyond Mod's checkpoint for the feed;
%% advance the checkpoint and publish any events the view emits.
%% Returns 1 when the message was folded, 0 when it was already covered.
deliver(Mod, #message{author = FeedId, sequence = Seq} = Msg) ->
    case checkpoint(Mod, FeedId) of
        Ckpt when Seq > Ckpt ->
            Res = try Mod:view_entry(Msg)
                  catch C:R:Stack ->
                          ?SSB_ERROR("view ~p crashed on ~p seq ~p: ~p:~p ~p",
                                     [Mod, FeedId, Seq, C, R, Stack]),
                          ok
                  end,
            ets:insert(?CKPT, {{Mod, feed, FeedId}, Seq}),
            mark_dirty(Mod, FeedId),
            case Res of
                {events, Events} -> publish(Mod, Events);
                _                -> ok
            end,
            1;
        _ ->
            0
    end.

publish(Mod, Events) ->
    Members = pg:get_members(?PG_SCOPE, {view, Mod}),
    [Pid ! {view_event, Mod, Event} || Pid <- Members, Event <- Events],
    ok.

save_all(Views) ->
    [try Mod:view_save()
     catch C:R ->
             %% Routine at shutdown: views stop before this manager
             %% (reverse start order) and snapshot themselves in their
             %% own terminate; their tables are already gone here.
             ?SSB_DEBUG("view ~p save skipped: ~p:~p", [Mod, C, R])
     end || {Mod, _Class} <- Views],
    persist_ckpt().

%% Populate the in-memory table from the store at boot.  A store that is
%% down or empty leaves the table empty, which every view reads as "no
%% checkpoint" and answers with a full refold — expensive, but correct,
%% and never silently partial.
load_ckpt() ->
    {Ckpts, Versions} =
        case catch ssb_store:declare(?MODULE, ?SCHEMA_VERSION, ?DDL) of
            ok ->
                {rows("SELECT view, feed, seq FROM view_checkpoint", []),
                 rows("SELECT view, version FROM view_version", [])};
            Err ->
                ?SSB_ERROR("view_manager: could not declare its schema (~p) — "
                           "every view will refold from the start of the log",
                           [Err]),
                {[], []}
        end,
    case Ckpts =:= [] andalso Versions =:= [] of
        true  -> import_ckpt_file();       %% may create ?CKPT itself
        false -> ok
    end,
    ensure_ckpt_table(),
    [ets:insert(?CKPT, {{to_mod(V), feed, Feed}, Seq}) || [V, Feed, Seq] <- Ckpts],
    [ets:insert(?CKPT, {{to_mod(V), version}, Ver})     || [V, Ver] <- Versions],
    ok.

ensure_ckpt_table() ->
    case ets:info(?CKPT, size) of
        undefined -> ets:new(?CKPT, [named_table, protected, set,
                                     {read_concurrency, true}]);
        _         -> ?CKPT
    end.

%% One-time import of the ets:tab2file snapshot this replaced.  Without it
%% the first boot after the port refolds every registered view over the
%% whole corpus, which on a real node is the difference between a restart
%% and an afternoon.
%%
%% Runs only when the store has nothing, so a stale file left lying about
%% can never overwrite live checkpoints.  The snapshot was taken of a
%% named table, so file2tab restores it under that name — which is why
%% this runs before ensure_ckpt_table/0 rather than after.  Everything
%% imported is written through immediately; the file is inert from then on
%% and can be deleted.
import_ckpt_file() ->
    File = <<(config:ssb_repo_loc())/binary, "views/checkpoints.tab">>,
    case catch ets:file2tab(?b2l(File)) of
        {ok, ?CKPT} ->
            Rows = ets:tab2list(?CKPT),
            [mark_dirty(Mod, Feed) || {{Mod, feed, Feed}, _} <- Rows],
            [store_version(Mod, V)  || {{Mod, version}, V} <- Rows],
            persist_ckpt(),
            ?SSB_INFO("view_manager: imported ~p checkpoints from ~s; the file "
                      "is no longer read and may be deleted", [length(Rows), File]);
        {ok, Other} ->
            ets:delete(Other),       %% not the table we snapshot: ignore it
            ok;
        _ ->
            ok                       %% no snapshot to import: a fresh node
    end.

%% Flush the checkpoints that have moved since the last flush.  Failure
%% leaves them dirty for the next tick rather than dropping them: the
%% store being briefly unavailable should cost a retry, not a refold.
persist_ckpt() ->
    case catch ets:tab2list(?DIRTY) of
        []                    -> ok;
        Dirty when is_list(Dirty) -> flush_dirty(Dirty);
        _                     -> ok        %% table gone: shutdown teardown
    end.

flush_dirty(Dirty) ->
    Rows = [[atom_to_binary(Mod, utf8), Feed, checkpoint(Mod, Feed)]
            || {{Mod, Feed}} <- Dirty],
    case catch ssb_store:insert_many(
                 "INSERT INTO view_checkpoint(view, feed, seq)"
                 " VALUES(?1, ?2, ?3)"
                 " ON CONFLICT(view, feed) DO UPDATE SET seq=excluded.seq",
                 Rows) of
        ok ->
            [ets:delete(?DIRTY, K) || {K} <- Dirty],
            ok;
        Err ->
            ?SSB_ERROR("view_manager: checkpoint flush failed (~p); "
                       "~p checkpoints held for the next flush",
                       [Err, length(Dirty)]),
            ok
    end.

mark_dirty(Mod, FeedId) ->
    ets:insert(?DIRTY, {{Mod, FeedId}}).

store_version(Mod, Version) ->
    catch ssb_store:write("INSERT INTO view_version(view, version)"
                          " VALUES(?1, ?2)"
                          " ON CONFLICT(view) DO UPDATE SET"
                          " version=excluded.version",
                          [atom_to_binary(Mod, utf8), Version]),
    ok.

rows(Sql, Params) ->
    try ssb_store:q(Sql, Params) of
        L when is_list(L) -> L;
        _                 -> []
    catch _:_ -> []
    end.

%% Rows only ever come from our own writes, so the atom set is bounded by
%% the views that have registered on this node.
to_mod(V) when is_binary(V) -> binary_to_atom(V, utf8).

stored_version(Mod) ->
    case ets:lookup(?CKPT, {Mod, version}) of
        [{_, V}] -> V;
        []       -> undefined
    end.

%% How many feeds Mod holds a checkpoint for — a cheap "how far along is
%% this view" number for the admin surface.
feeds_checkpointed(Mod) ->
    try ets:select_count(?CKPT, [{{{Mod, feed, '_'}, '_'}, [], [true]}])
    catch error:badarg -> 0
    end.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

vm_test_() ->
    {setup, fun vm_setup/0, fun vm_teardown/1,
     fun(_) ->
             [?_test(ingest_and_checkpoint()),
              ?_test(core_views_ordered_first()),
              ?_test(catch_up_is_asynchronous()),
              ?_test(no_gap_when_storing_during_catch_up()),
              ?_test(events_to_subscriber()),
              ?_test(catch_up_after_restart()),
              ?_test(rebuild_on_version_bump()),
              ?_test(rebuild_without_global_log()),
              ?_test(rebuild_folds_archives()),
              ?_test(only_changed_checkpoints_are_flushed()),
              ?_test(checkpoints_survive_a_hard_kill()),
              ?_test(imports_the_legacy_snapshot())]
     end}.

%% Fully isolated home: these tests archive the own feed and rebuild
%% from disk, and a home shared across eunit runs accumulates
%% overlapping archives (found the hard way).
vm_setup() ->
    vm_teardown(ignore),
    Home = filename:join("/tmp", "vm_" ++
                          integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = ssb_store:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = view_manager:start_link(),
    Home.

vm_teardown(Home) ->
    application:unset_env(ssb, test_view_version),
    [catch gen_server:stop(Name)
     || Name <- [view_manager, ssb_feed_sup, blobs, mess_auth, ssb_store, keys, config]],
    case Home of
        ignore -> ok;
        _ ->
            os:cmd("rm -rf " ++ Home),
            application:unset_env(ssb, ssb_home)
    end,
    ok.

%% eunit may run each test in its own process; a manager started (and
%% linked) inside an earlier test dies with it, so every test begins by
%% making sure one is running.
vm_ensure_manager() ->
    case whereis(view_manager) of
        undefined -> {ok, _} = view_manager:start_link(), ok;
        _         -> ok
    end.

vm_restart_manager() ->
    catch gen_server:stop(view_manager),
    {ok, _} = view_manager:start_link(),
    ok.

%% Catch-up is scheduled by register_view/1 and runs in the manager's own
%% message loop, so a test must wait for it before reading a view's
%% tables (which it does directly, not through the manager).
wait_caught_up(Mod) ->
    wait_caught_up(Mod, 250).

wait_caught_up(Mod, 0) ->
    error({never_caught_up, Mod});
wait_caught_up(Mod, N) ->
    case view_manager:caught_up(Mod) of
        true  -> ok;
        false -> timer:sleep(20), wait_caught_up(Mod, N - 1)
    end.

vm_make_peer() ->
    #{public := Pub, secret := Priv} = enacl:sign_keypair(),
    Id = <<"@", (base64:encode(Pub))/binary, ".ed25519">>,
    {utils:find_or_create_feed_pid(Id), Id, base64:encode(Priv)}.

vm_store_post(FeedPid, AuthId, AuthPriv, Prev, Seq) ->
    Content = {[{~"type", ~"post"}, {~"text", ~"view manager test"}]},
    Msg = message:new_msg(Prev, Seq, Content, {AuthId, AuthPriv}),
    _ = ssb_feed:store_msg(FeedPid, Msg),
    ssb_feed:fetch_last_msg(FeedPid).

ingest_and_checkpoint() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    ok = wait_caught_up(test_counter_view),
    {Pid, Id, Priv} = vm_make_peer(),
    ?assertEqual(0, checkpoint(test_counter_view, Id)),
    #message{id = M1} = vm_store_post(Pid, Id, Priv, null, 1),
    #message{}        = vm_store_post(Pid, Id, Priv, M1, 2),
    ?assertEqual([1, 2], test_counter_view:entries(Id)),
    ?assertEqual(2, checkpoint(test_counter_view, Id)).

%% A core view registered after an app view still sorts (and is
%% delivered) ahead of it — an app view may fold against core state, so
%% core has to see each message first.  test_counter_view declares no
%% view_class and must therefore default to `app`.
core_views_ordered_first() ->
    ok = test_counter_view:ensure_table(),
    ok = test_core_view:ensure_table(),
    ok = vm_ensure_manager(),
    %% a view declaring no class is an app view
    ?assertEqual(app,  ssb_view:class(test_counter_view)),
    ?assertEqual(core, ssb_view:class(test_core_view)),
    ok = register_view(test_counter_view),      %% app registers FIRST
    ok = register_view(test_core_view),         %% core registers second
    ok = wait_caught_up(test_core_view),
    ?assertEqual([test_core_view, test_counter_view], views()),
    ?assertEqual([test_core_view], views(core)),
    ?assertEqual([test_counter_view], views(app)).

%% register_view/1 must not block on the fold: the manager keeps
%% answering while catch-up runs, which is the whole point of chunking it
%% (a 2.5M-message store folded for ~28 s inside the call, stalling
%% replication with it — doc/persistence.md §8).
catch_up_is_asynchronous() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    %% the manager is responsive immediately after registering
    ?assert(lists:member(test_counter_view, views())),
    ok = wait_caught_up(test_counter_view),
    ?assert(caught_up(test_counter_view)),
    %% an unregistered view is never "catching up"
    ?assert(caught_up(never_registered_view)).

%% The hazard chunking introduces: a message stored while a view is still
%% folding must not be delivered ahead of the fold.  deliver/2 would
%% advance the checkpoint to that sequence and every earlier message the
%% fold had not yet reached would read as already-covered, leaving a
%% permanent hole.  ingest skips catching-up views for exactly this
%% reason, and the sweep picks the message up instead — so the view ends
%% with a contiguous run, no gap.
no_gap_when_storing_during_catch_up() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    ok = wait_caught_up(test_counter_view),
    {Pid, Id, Priv} = vm_make_peer(),
    %% history for one feed, plus a second so the fold spans chunks
    Ids = lists:foldl(
            fun(Seq, [Prev | _] = Acc) ->
                    #message{id = New} = vm_store_post(Pid, Id, Priv, Prev, Seq),
                    [New | Acc]
            end, [null], lists:seq(1, 12)),
    {Pid2, Id2, Priv2} = vm_make_peer(),
    #message{} = vm_store_post(Pid2, Id2, Priv2, null, 1),
    %% One feed per chunk, so the fold takes several turns of the message
    %% loop and the store below genuinely lands in the middle of it.
    application:set_env(ssb, view_catch_up_feeds, 1),
    try
        %% rebuild forces a full refold of an already-registered view
        ok = rebuild(test_counter_view),
        [Last | _] = Ids,
        #message{} = vm_store_post(Pid, Id, Priv, Last, 13),
        ok = wait_caught_up(test_counter_view),
        %% every sequence present exactly once, none skipped
        ?assertEqual(lists:seq(1, 13), test_counter_view:entries(Id)),
        ?assertEqual(13, checkpoint(test_counter_view, Id))
    after
        application:unset_env(ssb, view_catch_up_feeds)
    end.

events_to_subscriber() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    ok = wait_caught_up(test_counter_view),
    ok = subscribe(test_counter_view),
    {Pid, Id, Priv} = vm_make_peer(),
    #message{} = vm_store_post(Pid, Id, Priv, null, 1),
    receive
        {view_event, test_counter_view, {seen, Id, 1}} -> ok
    after 1000 ->
        error(no_view_event)
    end,
    ok = unsubscribe(test_counter_view).

%% Messages stored while the manager is down are delivered on
%% re-registration (catch-up from checkpoints, not a full rebuild).
catch_up_after_restart() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    ok = wait_caught_up(test_counter_view),
    {Pid, Id, Priv} = vm_make_peer(),
    #message{id = M1} = vm_store_post(Pid, Id, Priv, null, 1),
    ?assertEqual([1], test_counter_view:entries(Id)),
    ok = gen_server:stop(view_manager),         %% saves views + checkpoints
    #message{} = vm_store_post(Pid, Id, Priv, M1, 2),  %% ingest is a no-op
    ?assertEqual([1], test_counter_view:entries(Id)),
    {ok, _} = view_manager:start_link(),
    ok = register_view(test_counter_view),
    ok = wait_caught_up(test_counter_view),
    ?assertEqual([1, 2], test_counter_view:entries(Id)),
    ?assertEqual(2, checkpoint(test_counter_view, Id)).

%% Rebuild reads the per-feed logs, not the global log.offset: wiping
%% the global log must lose nothing.  (Regression: the global log holds
%% only a fraction of the per-feed history on converted nodes, which
%% left the social graph view nearly empty after its first rebuild.)
rebuild_without_global_log() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    ok = wait_caught_up(test_counter_view),
    {Pid, Id, Priv} = vm_make_peer(),
    #message{id = M1} = vm_store_post(Pid, Id, Priv, null, 1),
    #message{}        = vm_store_post(Pid, Id, Priv, M1, 2),
    %% the global log is not even written anymore — assert that, then
    %% prove the rebuild source is the per-feed store
    GlobalLog = ?b2l(<<(config:ssb_repo_loc())/binary, "log.offset">>),
    ?assertNot(filelib:is_file(GlobalLog)),
    ok = rebuild(test_counter_view),
    ok = wait_caught_up(test_counter_view),
    ?assertEqual([1, 2], test_counter_view:entries(Id)).

%% Archived segments are folded too: after archiving, a rebuild sees the
%% feed's full history with no sequence gaps.
rebuild_folds_archives() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    ok = wait_caught_up(test_counter_view),
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    ok = ssb_feed:post_content(OwnPid, ~"before archive one"),
    ok = ssb_feed:post_content(OwnPid, ~"before archive two"),
    _ = ssb_feed:archive(OwnPid),   %% gzips the live log, posts an archive msg
    ok = ssb_feed:post_content(OwnPid, ~"after archive"),
    #message{sequence = Last} = ssb_feed:fetch_last_msg(OwnPid),
    ok = rebuild(test_counter_view),
    ok = wait_caught_up(test_counter_view),
    ?assertEqual(lists:seq(1, Last), test_counter_view:entries(OwnId)).

%% A version bump forces reset + refold of the whole log.
rebuild_on_version_bump() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    ok = wait_caught_up(test_counter_view),
    {Pid, Id, Priv} = vm_make_peer(),
    #message{id = M1} = vm_store_post(Pid, Id, Priv, null, 1),
    #message{}        = vm_store_post(Pid, Id, Priv, M1, 2),
    ?assertEqual([1, 2], test_counter_view:entries(Id)),
    application:set_env(ssb, test_view_version, 2),
    ok = vm_restart_manager(),
    ok = register_view(test_counter_view),
    ok = wait_caught_up(test_counter_view),
    %% state wiped and refolded from the log, exactly once per message
    ?assertEqual([1, 2], test_counter_view:entries(Id)),
    ?assertEqual(2, checkpoint(test_counter_view, Id)).

%% The reason checkpoints are not written through: a flush costs one write
%% per checkpoint that MOVED, not one per checkpoint held.  Steady state is
%% an empty dirty set, and a single delivery dirties a single pair.
only_changed_checkpoints_are_flushed() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    ok = wait_caught_up(test_counter_view),
    {Pid, Id, Priv} = vm_make_peer(),
    #message{id = M1} = vm_store_post(Pid, Id, Priv, null, 1),
    ok = save(),
    ?assertEqual(0, ets:info(?DIRTY, size)),
    #message{} = vm_store_post(Pid, Id, Priv, M1, 2),
    ?assert(ets:member(?DIRTY, {test_counter_view, Id})),
    ok = save(),
    ?assertEqual(0, ets:info(?DIRTY, size)),
    ?assertEqual(2, checkpoint(test_counter_view, Id)).

%% Durability must not depend on terminate/2 running.  Once save/0 has
%% returned, a checkpoint survives the manager being killed outright —
%% which is what a crash or a SIGKILL actually looks like.
checkpoints_survive_a_hard_kill() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    ok = wait_caught_up(test_counter_view),
    {Pid, Id, Priv} = vm_make_peer(),
    #message{} = vm_store_post(Pid, Id, Priv, null, 1),
    ok = save(),
    VM = whereis(view_manager),
    true = unlink(VM),               %% or the kill takes this test with it
    exit(VM, kill),
    ok = wait_gone(VM, 100),
    {ok, _} = view_manager:start_link(),
    ?assertEqual(1, checkpoint(test_counter_view, Id)).

%% First boot after the port: checkpoints live in a legacy tab2file
%% snapshot and the store has none.  They must be imported rather than
%% thrown away — the alternative is refolding every view over the whole
%% corpus — and written through, so the file is dead weight afterwards.
imports_the_legacy_snapshot() ->
    ok = vm_ensure_manager(),
    {_Pid, Id, _Priv} = vm_make_peer(),
    ok = save(),                     %% empty the dirty set before wiping
    catch gen_server:stop(view_manager),
    %% a snapshot in the old format: tab2file of the named table
    File = ?b2l(<<(config:ssb_repo_loc())/binary, "views/checkpoints.tab">>),
    ok = filelib:ensure_dir(File),
    ?CKPT = ets:new(?CKPT, [named_table, public, set]),
    ets:insert(?CKPT, {{test_counter_view, feed, Id}, 7}),
    ok = ets:tab2file(?CKPT, File),
    true = ets:delete(?CKPT),
    %% both tables, or this is not a first boot: an empty view_checkpoint
    %% alongside recorded versions means a view was reset and has folded
    %% nothing yet, and importing over that would resurrect what the reset
    %% just threw away
    ok = ssb_store:exec("DELETE FROM view_checkpoint"),
    ok = ssb_store:exec("DELETE FROM view_version"),
    {ok, _} = view_manager:start_link(),
    ?assertEqual(7, checkpoint(test_counter_view, Id)),
    %% written through on import, so the file is now irrelevant
    ok = file:delete(File),
    ok = vm_restart_manager(),
    ?assertEqual(7, checkpoint(test_counter_view, Id)).

wait_gone(_Pid, 0)  -> error(still_alive);
wait_gone(Pid, N) ->
    case is_process_alive(Pid) of
        false -> ok;
        true  -> timer:sleep(10), wait_gone(Pid, N - 1)
    end.

-endif.
