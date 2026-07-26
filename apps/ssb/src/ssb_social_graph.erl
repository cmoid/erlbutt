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
%% The graphs are kept in named public ETS tables owned by this
%% gen_server and populated exclusively by view_manager (this module is
%% an ssb_view — see ssb_view.erl).  The manager guarantees the tables
%% are complete: it replays anything missed at registration, rebuilds
%% from the log when view_version/0 bumps, and folds every newly stored
%% message in synchronously — so reads are plain ETS lookups and a miss
%% simply means "no data for that feed".
%%
%% The view callbacks (view_entry/1 etc.) run in the view_manager
%% process, never in this server; they are plain functions over the
%% public tables.  Durable state is ets:tab2file snapshots under
%% <repo>/views/, restored (or created fresh) in init; view_save/0
%% stamps a completeness marker so view_load/0 can tell a restored
%% snapshot from a fresh table.
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

-define(GRAPH, ssb_follow_graph).
-define(BLOCKS, ssb_block_graph).

%% Written by view_save/0 before each snapshot; its presence after a
%% file2tab restore is how view_load/0 knows the state is complete up to
%% the manager's checkpoints.
-define(COMPLETE, '$complete').

%% Renamed from friends_*.tab with the module.  An upgrading node finds
%% no snapshot under the new names, so view_load/0 reports empty and the
%% manager rebuilds both graphs from the log — the intended behaviour for
%% a view whose storage identity changed.  The old files are inert and
%% can be deleted.
-define(TABLES,
        [{?GRAPH,  ~"social_graph_follows.tab"},
         {?BLOCKS, ~"social_graph_blocks.tab"}]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Feeds the given feed follows right now.
direct_follows(FeedPid) when is_pid(FeedPid) ->
    direct_follows(ssb_feed:whoami(FeedPid));
direct_follows(FeedId) ->
    case lookup(?GRAPH, FeedId) of
        {ok, Contacts} -> following_ids(Contacts);
        miss           -> []
    end.

%% Feeds the given feed blocks right now.
blocks(FeedPid) when is_pid(FeedPid) ->
    blocks(ssb_feed:whoami(FeedPid));
blocks(FeedId) ->
    case lookup(?BLOCKS, FeedId) of
        {ok, Blocked} -> blocking_ids(Blocked);
        miss          -> []
    end.

%% Transitive follows out to HopCount hops, excluding the start feed.
follows(FeedPid, HopCount) when is_pid(FeedPid) ->
    follows(ssb_feed:whoami(FeedPid), HopCount);
follows(FeedId, HopCount) ->
    {AllFollows, _} = follows2(FeedId, HopCount, sets:from_list([FeedId])),
    lists:usort(AllFollows).

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
%% or blocks Dest (block wins).  Scans the graphs, so O(feeds).
reverse_edges(Dest) ->
    F = reverse_fold(?GRAPH, Dest, true, #{}),
    reverse_fold(?BLOCKS, Dest, false, F).

reverse_fold(Tab, Dest, Value, Acc0) ->
    try
        ets:foldl(
          fun({Source, Map}, Acc) when is_map(Map) ->
                  case maps:get(Dest, Map, undefined) of
                      true -> Acc#{Source => Value};
                      _    -> Acc
                  end;
             (_, Acc) -> Acc          %% skip the completeness marker row
          end, Acc0, Tab)
    catch error:badarg -> Acc0        %% table absent
    end.

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
    case lists:all(fun({Tab, _}) -> has_marker(Tab) end, ?TABLES) of
        true  -> ok;
        false -> empty
    end.

view_reset() ->
    [ets:delete_all_objects(Tab) || {Tab, _} <- ?TABLES],
    ok.

view_save() ->
    [begin
         ets:insert(Tab, {?COMPLETE, true}),
         File = table_file(FileName),
         filelib:ensure_dir(File),
         ok = ets:tab2file(Tab, ?b2l(File))
     end || {Tab, FileName} <- ?TABLES],
    ok.

%% Fold one stored message into the index.  A contact message can carry
%% `following` and/or `blocking`; each applies to its own graph and is
%% announced to subscribers (ebt keeps its replication set current from
%% these events).
view_entry(#message{author = Author} = Msg) ->
    FollowEvents =
        case social_msg:is_follow(Msg) of
            {C, F} when is_binary(C) ->
                apply_edge(?GRAPH, Author, C, F),
                [{contact, Author, C, F}];
            _ -> []
        end,
    BlockEvents =
        case social_msg:is_block(Msg) of
            {Cb, B} when is_binary(Cb) ->
                apply_edge(?BLOCKS, Author, Cb, B),
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
    [restore_or_create(Tab, FileName) || {Tab, FileName} <- ?TABLES],
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

restore_or_create(Tab, FileName) ->
    %% table_file needs config; without it (bare eunit setups) start with
    %% fresh tables — view_load() then reports empty and the manager
    %% rebuilds if/when one is running.
    Restored = try ets:file2tab(?b2l(table_file(FileName)))
               catch _:_ -> {error, no_config}
               end,
    case Restored of
        {ok, Tab} -> ok;
        _         -> ets:new(Tab, [set, named_table, public])
    end.

table_file(FileName) ->
    <<(config:ssb_repo_loc())/binary, "views/", FileName/binary>>.

has_marker(Tab) ->
    try ets:lookup(Tab, ?COMPLETE) =/= []
    catch error:badarg -> false
    end.

apply_edge(Tab, Author, Contact, Bool) ->
    Cur = case ets:lookup(Tab, Author) of
              [{Author, Map}] -> Map;
              []              -> #{}
          end,
    ets:insert(Tab, {Author, Cur#{Contact => Bool}}).

lookup(Tab, Key) ->
    try ets:lookup(Tab, Key) of
        [{Key, Val}] -> {ok, Val};
        []           -> miss
    catch
        error:badarg -> miss             %% table absent: server not running
    end.

following_ids(Contacts) ->
    [Id || Id := true <- Contacts].

blocking_ids(Blocks) ->
    [Id || Id := true <- Blocks].

follows2(_FeedId, 0, Visited) ->
    {[], Visited};

follows2(FeedId, HopCount, Visited0) ->
    NewDirect = [Id || Id <- direct_follows(FeedId),
                       not sets:is_element(Id, Visited0)],
    {Deeper, Visited1} =
        lists:foldl(
          fun(Id, {Acc, Vis}) ->
                  case sets:is_element(Id, Vis) of
                      true ->
                          {Acc, Vis};
                      false ->
                          Vis2 = sets:add_element(Id, Vis),
                          {Ids, Vis3} = follows2(Id, HopCount - 1, Vis2),
                          {lists:append(Ids, Acc), Vis3}
                  end
          end, {[], Visited0}, NewDirect),
    {lists:append(NewDirect, Deeper), Visited1}.

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
    Started = lists:filtermap(
        fun({Name, StartFun}) ->
            case whereis(Name) of
                undefined ->
                    {ok, Pid} = StartFun(),
                    {true, Pid};
                _ ->
                    false
            end
        end,
        [{config,       fun() -> config:start_link("test/ssb.cfg") end},
         {keys,         fun() -> keys:start_link() end},
         {mess_auth,    fun() -> mess_auth:start_link() end},
         {blobs,        fun() -> blobs:start_link() end},
         {ssb_feed_sup, fun() -> ssb_feed_sup:start_link() end},
         {view_manager, fun() -> view_manager:start_link() end},
         {ssb_social_graph, fun() -> ssb_social_graph:start_link() end},
         {ssb_feed_meta,    fun() -> ssb_feed_meta:start_link() end}]),
    %% A view registers itself from its own handle_continue, and
    %% view_manager schedules the catch-up fold rather than running it in
    %% the call.  A view that is still catching up receives no ingests
    %% (they would open a checkpoint gap), so these tests must wait for
    %% the fold before storing anything and asserting on the result.
    ok = wait_caught_up(ssb_social_graph),
    ok = wait_caught_up(ssb_feed_meta),
    Started.

wait_caught_up(Mod) ->
    wait_caught_up(Mod, 250).

wait_caught_up(Mod, 0) ->
    error({never_caught_up, Mod});
wait_caught_up(Mod, N) ->
    case view_manager:caught_up(Mod) of
        true  -> ok;
        false -> timer:sleep(20), wait_caught_up(Mod, N - 1)
    end.

teardown(Pids) ->
    %% reverse start order, so the views go down before the
    %% services their shutdown paths use (config)
    lists:foreach(fun(Pid) -> catch gen_server:stop(Pid) end,
                  lists:reverse(Pids)).

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
