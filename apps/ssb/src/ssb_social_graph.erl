%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
%%
%% Core view: the follow graph and the block graph.
%%
%% This is protocol infrastructure, not a social feature — `ebt` asks it
%% who to replicate and who to refuse, so a node carrying no social
%% application at all still needs it (doc/persistence.md §5).  It was
%% called `friends` until July 2026, a patchwork-ism sitting in the
%% foundation; the display-name cache it also carried moved out to
%% ssb_feed_meta, where arbitrary self-asserted fields live.
%%
%% The graphs live in ssb_store (doc/persistence.md §6) as one table of
%% edges, and this is the first view ported off ETS-plus-tab2file.  It
%% was chosen as the pilot for having the fewest readers and the
%% clearest shape; two things came out better than a straight swap:
%%
%%   reverse_edges/1 was a full ets:foldl over both graphs — O(feeds) to
%%   answer "who follows this person".  It is now an indexed lookup.
%%
%%   follows/2 was a hand-rolled breadth-first walk in Erlang carrying a
%%   visited set.  It is now one recursive CTE; SQLite's UNION does the
%%   cycle detection the visited set was there for.
%%
%% The view callbacks (view_entry/1 etc.) run in the view_manager
%% process, never in this server, which now owns no state at all — it
%% exists to register the view and retry if a service is not up yet.
%% Writes are durable as they happen, so view_save/0 has nothing to
%% flush; all it records is the completeness marker view_load/0 reads
%% (see ssb_store's note on why that marker is still needed).
-module(ssb_social_graph).

-behaviour(gen_server).
-behaviour(ssb_view).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/0,
         direct_follows/1,
         follows/2,
         blocks/1,
         edge/2,
         edges/1,
         reverse_edges/1]).

%% ssb_view callbacks
-export([view_version/0,
         view_class/0,
         view_load/0,
         view_reset/0,
         view_save/0,
         view_entry/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         handle_info/2, terminate/2, code_change/3]).

%% kind column: a contact message can assert following and blocking
%% independently, so they are separate edges between the same pair.
-define(FOLLOW, 0).
-define(BLOCK,  1).

%% state is the asserted value: 1 for following/blocking, 0 for the
%% retraction (an unfollow is a fact, not the absence of one), which is
%% why the row is updated rather than deleted.
-define(SCHEMA_VERSION, 1).
-define(DDL,
        ["CREATE TABLE IF NOT EXISTS social_edges("
         "  source TEXT NOT NULL,"
         "  dest   TEXT NOT NULL,"
         "  kind   INTEGER NOT NULL,"
         "  state  INTEGER NOT NULL,"
         "  PRIMARY KEY (source, dest, kind)) WITHOUT ROWID;",
         %% the index reverse_edges/1 used to lack
         "CREATE INDEX IF NOT EXISTS ix_social_dest"
         "  ON social_edges(dest, kind, state);"]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Feeds the given feed follows right now.
direct_follows(FeedPid) when is_pid(FeedPid) ->
    direct_follows(ssb_feed:whoami(FeedPid));
direct_follows(FeedId) ->
    out_edges(FeedId, ?FOLLOW).

%% Feeds the given feed blocks right now.
blocks(FeedPid) when is_pid(FeedPid) ->
    blocks(ssb_feed:whoami(FeedPid));
blocks(FeedId) ->
    out_edges(FeedId, ?BLOCK).

out_edges(FeedId, Kind) when is_binary(FeedId) ->
    [D || [D] <- rows("SELECT dest FROM social_edges"
                      " WHERE source=?1 AND kind=?2 AND state=1",
                      [FeedId, Kind])];
out_edges(_NotAFeed, _Kind) ->
    [].

%% Transitive follows out to HopCount hops, excluding the start feed.
follows(FeedPid, HopCount) when is_pid(FeedPid) ->
    follows(ssb_feed:whoami(FeedPid), HopCount);
follows(FeedId, HopCount) when is_binary(FeedId), is_integer(HopCount) ->
    %% Reachability over the follow graph, bounded by HopCount and
    %% excluding the start feed.  UNION (not UNION ALL) makes the walk
    %% terminate on a cycle, which is what the old visited set did by
    %% hand.
    lists:usort(
      [Id || [Id] <-
                 rows("WITH RECURSIVE reach(id, depth) AS ("
                      "  SELECT ?1, 0"
                      "  UNION"
                      "  SELECT e.dest, r.depth + 1"
                      "    FROM social_edges e JOIN reach r ON e.source = r.id"
                      "   WHERE e.kind = ?3 AND e.state = 1 AND r.depth < ?2)"
                      " SELECT id FROM reach WHERE id <> ?1",
                      [FeedId, HopCount, ?FOLLOW])]);
follows(_FeedId, _HopCount) ->
    [].

%% The relationship Source holds toward Dest in ssb-friends legacy
%% terms: true = following, false = blocking, null = neither.  Block
%% wins (consistent with EBT replication gating).
edge(Source, Dest) ->
    case lists:member(Dest, blocks(Source)) of
        true  -> false;
        false ->
            case lists:member(Dest, direct_follows(Source)) of
                true  -> true;
                false -> null
            end
    end.

%% All of Source's outgoing edges as #{Dest => true | false}
%% (following or blocking; block wins).
edges(Source) ->
    Follows = maps:from_list([{D, true}  || D <- direct_follows(Source)]),
    Blocks  = maps:from_list([{D, false} || D <- blocks(Source)]),
    maps:merge(Follows, Blocks).

%% All edges pointing AT Dest as #{Source => true | false}: who follows
%% or blocks Dest (block wins).  An indexed lookup now — this used to
%% fold both whole graphs.
reverse_edges(Dest) when is_binary(Dest) ->
    Rows = rows("SELECT source, kind FROM social_edges"
                " WHERE dest=?1 AND state=1", [Dest]),
    %% blocks last so they overwrite a follow between the same pair
    lists:foldl(fun([Source, Kind], Acc) ->
                        Acc#{Source => Kind =:= ?FOLLOW}
                end, #{},
                [R || [_, K] = R <- Rows, K =:= ?FOLLOW] ++
                [R || [_, K] = R <- Rows, K =:= ?BLOCK]).

%%%===================================================================
%%% ssb_view callbacks (run in the view_manager process)
%%%===================================================================

%% 2: the fold source changed from the (hole-ridden) global log.offset
%% to the per-feed logs.  Checkpoints recorded against the old source
%% claim coverage they don't have, so upgrading nodes must rebuild —
%% which is exactly what a version bump forces.
view_version() -> 2.

view_class() -> core.

view_load() ->
    case ssb_store:complete(?MODULE) of
        true  -> ok;
        false -> empty
    end.

view_reset() ->
    _ = ssb_store:clear_complete(?MODULE),
    _ = ssb_store:exec("DELETE FROM social_edges;"),
    ok.

%% Rows are already durable; the only thing to record is that this view's
%% state is complete up to the manager's checkpoints.
view_save() ->
    _ = ssb_store:mark_complete(?MODULE),
    ok.

%% Fold one stored message into the index.  A contact message can carry
%% `following` and/or `blocking`; each applies to its own graph and is
%% announced to subscribers (ebt keeps its replication set current from
%% these events).
view_entry(#message{author = Author} = Msg) ->
    FollowEvents =
        case social_msg:is_follow(Msg) of
            {C, F} when is_binary(C) ->
                apply_edge(Author, C, ?FOLLOW, F),
                [{contact, Author, C, F}];
            _ -> []
        end,
    BlockEvents =
        case social_msg:is_block(Msg) of
            {Cb, B} when is_binary(Cb) ->
                apply_edge(Author, Cb, ?BLOCK, B),
                [{block, Author, Cb, B}];
            _ -> []
        end,
    case FollowEvents ++ BlockEvents of
        []     -> ok;
        Events -> {events, Events}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = ssb_store:declare(?MODULE, ?SCHEMA_VERSION, ?DDL),
    {ok, #{}, {continue, register_view}}.

handle_continue(register_view, State) ->
    %% Registration failures are loud and transient ones retried on a
    %% timer (ssb_view:ensure_registered) — a silent skip means the
    %% follow graph stops updating.  In eunit setups without a
    %% view_manager the retries just keep logging.
    ensure_registered(State).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(ensure_registered, State) ->
    ensure_registered(State);
handle_info(_Info, State) ->
    {noreply, State}.

%% First attempt (from handle_continue) and every timer retry land
%% here; keep trying until the view registration is accepted.  This view
%% registers no plugin — its RPC surface lives in the silkpurse app.
ensure_registered(State) ->
    case ssb_view:ensure_registered(?MODULE, [view]) of
        ok    -> ok;
        retry -> erlang:send_after(2000, self(), ensure_registered)
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Snapshot before the tables die with this process.  At shutdown we
    %% stop before view_manager (reverse start order), so the manager's
    %% own final save of this view cannot succeed — this one can.
    catch view_save(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Read helper.  A query before the store is up (a bare eunit fixture, a
%% restarting store) answers empty rather than raising: a view read is on
%% the path of RPC handlers and ebt, and a missing index means "no data",
%% never a crash — the same contract the ETS lookups had.
rows(Sql, Params) ->
    try ssb_store:q(Sql, Params) of
        L when is_list(L) -> L;
        _                 -> []
    catch _:_ -> []
    end.

%% Assert one edge.  A contact message restates the whole relationship,
%% so this is an upsert on (source, dest, kind) rather than an insert.
apply_edge(Author, Contact, Kind, Bool) ->
    State = case Bool of true -> 1; _ -> 0 end,
    _ = ssb_store:write("INSERT INTO social_edges(source,dest,kind,state)"
                        " VALUES(?1,?2,?3,?4)"
                        " ON CONFLICT(source,dest,kind) DO UPDATE SET"
                        " state=excluded.state",
                        [Author, Contact, Kind, State]),
    ok.

-ifdef(TEST).

social_graph_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [fun direct_follows_empty_test/1,
      fun direct_follows_follow_test/1,
      fun direct_follows_unfollow_test/1,
      fun incremental_update_test/1,
      fun garbage_contact_test/1,
      fun follows_zero_hops_test/1,
      fun follows_one_hop_test/1,
      fun follows_two_hop_test/1,
      fun follows_no_cycle_test/1,
      fun blocks_block_test/1,
      fun blocks_unblock_test/1,
      fun blocks_independent_of_follow_test/1,
      fun edge_test/1,
      fun edges_test/1,
      fun name_updates_test/1,
      fun name_other_about_test/1,
      fun contact_event_test/1,
      fun rebuild_from_log_test/1]}.

setup() ->
    %% Isolated home per test.  Before the store, these fixtures shared
    %% whatever ssb_home resolved to and the ETS tables were wiped by
    %% teardown; now there is a store.db on disk, so a shared home means
    %% one run's edges leak into the next (and into the repo's own
    %% .ssberl).  Each test gets its own directory, removed afterwards.
    teardown(ignore),
    Home = filename:join("/tmp", "social_graph_" ++
                          integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("test/ssb.cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = ssb_store:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = view_manager:start_link(),
    {ok, _} = ssb_social_graph:start_link(),
    {ok, _} = ssb_feed_meta:start_link(),
    %% A view registers itself from its own handle_continue, and
    %% view_manager schedules the catch-up fold rather than running it in
    %% the call.  A view that is still catching up receives no ingests
    %% (they would open a checkpoint gap), so these tests must wait for
    %% the fold before storing anything and asserting on the result.
    ok = wait_caught_up(ssb_social_graph),
    ok = wait_caught_up(ssb_feed_meta),
    Home.

wait_caught_up(Mod) ->
    wait_caught_up(Mod, 250).

wait_caught_up(Mod, 0) ->
    error({never_caught_up, Mod});
wait_caught_up(Mod, N) ->
    case view_manager:caught_up(Mod) of
        true  -> ok;
        false -> timer:sleep(20), wait_caught_up(Mod, N - 1)
    end.

teardown(Home) ->
    %% reverse start order, so the views go down before the services
    %% their shutdown paths use (store, config)
    [catch gen_server:stop(N)
     || N <- [ssb_feed_meta, ssb_social_graph, view_manager, ssb_store,
              ssb_feed_sup, blobs, mess_auth,  keys, config]],
    case Home of
        ignore -> ok;
        _ ->
            os:cmd("rm -rf " ++ Home),
            application:unset_env(ssb, ssb_home)
    end,
    ok.

%% Create a fresh feed backed by a generated key pair.
%% Returns {FeedPid, FeedId, PrivKey}.
make_peer() ->
    #{public := Pub, secret := Priv} = enacl:sign_keypair(),
    Id = <<"@", (base64:encode(Pub))/binary, ".ed25519">>,
    PrivB64 = base64:encode(Priv),
    Pid = utils:find_or_create_feed_pid(Id),
    {Pid, Id, PrivB64}.

%% Store a contact message in FeedPid, signed by {AuthId, AuthPriv}.
store_contact(FeedPid, AuthId, AuthPriv, Prev, Seq, ContactId, Following) ->
    Content = {[{~"type", ~"contact"}, {~"contact", ContactId}, {~"following", Following}]},
    Msg = message:new_msg(Prev, Seq, Content, {AuthId, AuthPriv}),
    _ = ssb_feed:store_msg(FeedPid, Msg),
    ok.

%% Store an about message in FeedPid naming AboutId, signed by {AuthId, AuthPriv}.
store_about(FeedPid, AuthId, AuthPriv, Prev, Seq, AboutId, Name) ->
    Content = {[{~"type", ~"about"}, {~"about", AboutId}, {~"name", Name}]},
    Msg = message:new_msg(Prev, Seq, Content, {AuthId, AuthPriv}),
    _ = ssb_feed:store_msg(FeedPid, Msg),
    ok.

%% Store a blocking contact message in FeedPid, signed by {AuthId, AuthPriv}.
store_block(FeedPid, AuthId, AuthPriv, Prev, Seq, ContactId, Blocking) ->
    Content = {[{~"type", ~"contact"}, {~"contact", ContactId}, {~"blocking", Blocking}]},
    Msg = message:new_msg(Prev, Seq, Content, {AuthId, AuthPriv}),
    _ = ssb_feed:store_msg(FeedPid, Msg),
    ok.

direct_follows_empty_test(_) ->
    fun() ->
        {Pid, _Id, _Priv} = make_peer(),
        ?assertEqual([], ssb_social_graph:direct_follows(Pid))
    end.

direct_follows_follow_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([Id2], ssb_social_graph:direct_follows(Pid))
    end.

blocks_block_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ?assertEqual([], ssb_social_graph:blocks(Pid)),
        ok = store_block(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([Id2], ssb_social_graph:blocks(Pid))
    end.

blocks_unblock_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_block(Pid, Id, Priv, null, 1, Id2, true),
        #message{id = Msg1Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_block(Pid, Id, Priv, Msg1Id, 2, Id2, false),
        ?assertEqual([], ssb_social_graph:blocks(Pid))
    end.

%% follow and block are tracked independently, even within the same feed.
blocks_independent_of_follow_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_P2, Id2, _Pr2} = make_peer(),
        {_P3, Id3, _Pr3} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        #message{id = Msg1Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_block(Pid, Id, Priv, Msg1Id, 2, Id3, true),
        ?assertEqual([Id2], ssb_social_graph:direct_follows(Pid)),
        ?assertEqual([Id3], ssb_social_graph:blocks(Pid))
    end.

direct_follows_unfollow_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        #message{id = Msg1Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_contact(Pid, Id, Priv, Msg1Id, 2, Id2, false),
        ?assertEqual([], ssb_social_graph:direct_follows(Pid))
    end.

%% edge/2: following -> true, blocking -> false (block wins), else null.
edge_test(_) ->
    fun() ->
        {Pid, Id, Priv}      = make_peer(),
        {_P2, Followed, _}   = make_peer(),
        {_P3, Blocked, _}    = make_peer(),
        {_P4, Stranger, _}   = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Followed, true),
        #message{id = M1} = ssb_feed:fetch_last_msg(Pid),
        ok = store_block(Pid, Id, Priv, M1, 2, Blocked, true),
        ?assertEqual(true,  ssb_social_graph:edge(Id, Followed)),
        ?assertEqual(false, ssb_social_graph:edge(Id, Blocked)),
        ?assertEqual(null,  ssb_social_graph:edge(Id, Stranger))
    end.

edges_test(_) ->
    fun() ->
        {Pid, Id, Priv}    = make_peer(),
        {_P2, Followed, _} = make_peer(),
        {_P3, Blocked, _}  = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Followed, true),
        #message{id = M1} = ssb_feed:fetch_last_msg(Pid),
        ok = store_block(Pid, Id, Priv, M1, 2, Blocked, true),
        Edges = ssb_social_graph:edges(Id),
        ?assertEqual(true,  maps:get(Followed, Edges)),
        ?assertEqual(false, maps:get(Blocked, Edges)),
        ?assertEqual(2, maps:size(Edges))
    end.

%% Contacts stored after the first read are applied incrementally by the
%% view manager's synchronous ingest — reads always see the latest store.
incremental_update_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        {_Pid3, Id3, _Priv3} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([Id2], ssb_social_graph:direct_follows(Id)),
        #message{id = Msg1Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_contact(Pid, Id, Priv, Msg1Id, 2, Id3, true),
        ?assertEqual(lists:sort([Id2, Id3]),
                     lists:sort(ssb_social_graph:direct_follows(Id))),
        #message{id = Msg2Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_contact(Pid, Id, Priv, Msg2Id, 3, Id2, false),
        ?assertEqual([Id3], ssb_social_graph:direct_follows(Id))
    end.

%% Legacy planetary garbage: contact field holding a boolean is ignored.
garbage_contact_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, true, true),
        ?assertEqual([], ssb_social_graph:direct_follows(Id))
    end.

follows_zero_hops_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([], ssb_social_graph:follows(Pid, 0))
    end.

follows_one_hop_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_AlicePid, AliceId, _AlicePriv} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, AliceId, true),
        Result = ssb_social_graph:follows(Pid, 1),
        ?assert(lists:member(AliceId, Result))
    end.

follows_two_hop_test(_) ->
    fun() ->
        {OwnerPid, OwnerId, OwnerPriv} = make_peer(),
        {AlicePid, AliceId, AlicePriv} = make_peer(),
        {_BobPid, BobId, _BobPriv} = make_peer(),
        ok = store_contact(OwnerPid, OwnerId, OwnerPriv, null, 1, AliceId, true),
        ok = store_contact(AlicePid, AliceId, AlicePriv, null, 1, BobId, true),
        Result = ssb_social_graph:follows(OwnerPid, 2),
        ?assert(lists:member(AliceId, Result)),
        ?assert(lists:member(BobId, Result))
    end.

%% Mutual follows must not cause an infinite loop, and the start node
%% must not appear in the result (it is always in the initial visited set).
follows_no_cycle_test(_) ->
    fun() ->
        {OwnerPid, OwnerId, OwnerPriv} = make_peer(),
        {AlicePid, AliceId, AlicePriv} = make_peer(),
        ok = store_contact(OwnerPid, OwnerId, OwnerPriv, null, 1, AliceId, true),
        ok = store_contact(AlicePid, AliceId, AlicePriv, null, 1, OwnerId, true),
        Result = ssb_social_graph:follows(OwnerPid, 5),
        ?assert(lists:member(AliceId, Result)),
        ?assertNot(lists:member(OwnerId, Result))
    end.

%% A newer self-about wins over an older one.
name_updates_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        ?assertEqual(undefined, ssb_feed_meta:name(Id)),
        ok = store_about(Pid, Id, Priv, null, 1, Id, ~"alice"),
        ?assertEqual(~"alice", ssb_feed_meta:name(Id)),
        #message{id = Msg1Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_about(Pid, Id, Priv, Msg1Id, 2, Id, ~"alice the great"),
        ?assertEqual(~"alice the great", ssb_feed_meta:name(Id))
    end.

%% An about message naming someone else must not set that feed's name.
name_other_about_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_about(Pid, Id, Priv, null, 1, Id2, ~"impostor"),
        ?assertEqual(undefined, ssb_feed_meta:name(Id2)),
        ?assertEqual(undefined, ssb_feed_meta:name(Id))
    end.

%% Follow changes are announced to view subscribers.
contact_event_test(_) ->
    fun() ->
        ok = view_manager:subscribe(ssb_social_graph),
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        receive
            {view_event, ssb_social_graph, {contact, Id, Id2, true}} -> ok
        after 1000 ->
            error(no_contact_event)
        end,
        ok = view_manager:unsubscribe(ssb_social_graph)
    end.

%% Wiping the derived state and rebuilding refolds it from the log.
rebuild_from_log_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([Id2], ssb_social_graph:direct_follows(Id)),
        %% simulate lost derived state, then refold from the log
        ok = view_reset(),
        ?assertEqual([], ssb_social_graph:direct_follows(Id)),
        %% rebuild/1 schedules the refold rather than running it inline
        ok = view_manager:rebuild(ssb_social_graph),
        ok = wait_caught_up(ssb_social_graph),
        ?assertEqual([Id2], ssb_social_graph:direct_follows(Id))
    end.

-endif.
