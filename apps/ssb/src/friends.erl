%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
%%
%% Maintained social index: a follow graph, a block graph and a
%% profile-name cache, kept in named public ETS tables owned by this
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
-module(friends).

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
         name/1]).

%% ssb_view callbacks
-export([view_version/0,
         view_load/0,
         view_reset/0,
         view_save/0,
         view_entry/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         handle_info/2, terminate/2, code_change/3]).

-define(GRAPH, ssb_follow_graph).
-define(NAMES, ssb_profile_names).
-define(BLOCKS, ssb_block_graph).

%% Written by view_save/0 before each snapshot; its presence after a
%% file2tab restore is how view_load/0 knows the state is complete up to
%% the manager's checkpoints.
-define(COMPLETE, '$complete').

-define(TABLES,
        [{?GRAPH,  ~"friends_graph.tab"},
         {?NAMES,  ~"friends_names.tab"},
         {?BLOCKS, ~"friends_blocks.tab"}]).

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

%% Latest self-assigned display name for FeedId, or undefined.
name(FeedId) ->
    case lookup(?NAMES, FeedId) of
        {ok, Name} -> Name;
        miss       -> undefined
    end.

%%%===================================================================
%%% ssb_view callbacks (run in the view_manager process)
%%%===================================================================

view_version() -> 1.

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
%% these events).  Self-assigned abouts update the name cache.
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
    apply_name(Msg),
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
    try view_manager:register_view(?MODULE)
    catch exit:{noproc, _} ->
            %% No view_manager (some eunit setups): the tables stay as
            %% restored/empty and nothing feeds them.
            ?SSB_INFO("friends: running without view_manager", [])
    end,
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
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

apply_name(#message{author = Author, content = {Props}} = Msg) ->
    case social_msg:is_about(Msg) of
        true ->
            case {?pgv(~"about", Props), ?pgv(~"name", Props)} of
                {Author, Name} when is_binary(Name) ->
                    ets:insert(?NAMES, {Author, Name});
                _ -> ok
            end;
        false -> ok
    end;
apply_name(_) ->
    ok.

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

friends_test_() ->
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
         {friends,      fun() -> friends:start_link() end}]),
    Started.

teardown(Pids) ->
    %% reverse start order, so view_manager/friends go down before the
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
        ?assertEqual([], friends:direct_follows(Pid))
    end.

direct_follows_follow_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([Id2], friends:direct_follows(Pid))
    end.

blocks_block_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ?assertEqual([], friends:blocks(Pid)),
        ok = store_block(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([Id2], friends:blocks(Pid))
    end.

blocks_unblock_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_block(Pid, Id, Priv, null, 1, Id2, true),
        #message{id = Msg1Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_block(Pid, Id, Priv, Msg1Id, 2, Id2, false),
        ?assertEqual([], friends:blocks(Pid))
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
        ?assertEqual([Id2], friends:direct_follows(Pid)),
        ?assertEqual([Id3], friends:blocks(Pid))
    end.

direct_follows_unfollow_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        #message{id = Msg1Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_contact(Pid, Id, Priv, Msg1Id, 2, Id2, false),
        ?assertEqual([], friends:direct_follows(Pid))
    end.

%% Contacts stored after the first read are applied incrementally by the
%% view manager's synchronous ingest — reads always see the latest store.
incremental_update_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        {_Pid3, Id3, _Priv3} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([Id2], friends:direct_follows(Id)),
        #message{id = Msg1Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_contact(Pid, Id, Priv, Msg1Id, 2, Id3, true),
        ?assertEqual(lists:sort([Id2, Id3]),
                     lists:sort(friends:direct_follows(Id))),
        #message{id = Msg2Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_contact(Pid, Id, Priv, Msg2Id, 3, Id2, false),
        ?assertEqual([Id3], friends:direct_follows(Id))
    end.

%% Legacy planetary garbage: contact field holding a boolean is ignored.
garbage_contact_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, true, true),
        ?assertEqual([], friends:direct_follows(Id))
    end.

follows_zero_hops_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([], friends:follows(Pid, 0))
    end.

follows_one_hop_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_AlicePid, AliceId, _AlicePriv} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, AliceId, true),
        Result = friends:follows(Pid, 1),
        ?assert(lists:member(AliceId, Result))
    end.

follows_two_hop_test(_) ->
    fun() ->
        {OwnerPid, OwnerId, OwnerPriv} = make_peer(),
        {AlicePid, AliceId, AlicePriv} = make_peer(),
        {_BobPid, BobId, _BobPriv} = make_peer(),
        ok = store_contact(OwnerPid, OwnerId, OwnerPriv, null, 1, AliceId, true),
        ok = store_contact(AlicePid, AliceId, AlicePriv, null, 1, BobId, true),
        Result = friends:follows(OwnerPid, 2),
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
        Result = friends:follows(OwnerPid, 5),
        ?assert(lists:member(AliceId, Result)),
        ?assertNot(lists:member(OwnerId, Result))
    end.

%% A newer self-about wins over an older one.
name_updates_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        ?assertEqual(undefined, friends:name(Id)),
        ok = store_about(Pid, Id, Priv, null, 1, Id, ~"alice"),
        ?assertEqual(~"alice", friends:name(Id)),
        #message{id = Msg1Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_about(Pid, Id, Priv, Msg1Id, 2, Id, ~"alice the great"),
        ?assertEqual(~"alice the great", friends:name(Id))
    end.

%% An about message naming someone else must not set that feed's name.
name_other_about_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_about(Pid, Id, Priv, null, 1, Id2, ~"impostor"),
        ?assertEqual(undefined, friends:name(Id2)),
        ?assertEqual(undefined, friends:name(Id))
    end.

%% Follow changes are announced to view subscribers.
contact_event_test(_) ->
    fun() ->
        ok = view_manager:subscribe(friends),
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        receive
            {view_event, friends, {contact, Id, Id2, true}} -> ok
        after 1000 ->
            error(no_contact_event)
        end,
        ok = view_manager:unsubscribe(friends)
    end.

%% Wiping the derived state and rebuilding refolds it from the log.
rebuild_from_log_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([Id2], friends:direct_follows(Id)),
        %% simulate lost derived state, then refold from the log
        ok = view_reset(),
        ?assertEqual([], friends:direct_follows(Id)),
        ok = view_manager:rebuild(friends),
        ?assertEqual([Id2], friends:direct_follows(Id))
    end.

-endif.
