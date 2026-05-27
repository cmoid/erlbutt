%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Plumtree epidemic broadcast (Leitão et al., 2007).
%%
%% Most of the ideas in here come from https://github.com/helium/plumtree,
%% a deprecated erlang implementation that was pulled out of riak. It's
%% essentially a stripped-down version of that implementation that removes all the
%% riak dependencies.
%%
%% Maintains eager and lazy peer sets per the paper:
%%   - eager peers receive full gossip payloads immediately
%%   - lazy peers receive only IHAVE announcements (message IDs)
%%
%% On duplicate receipt, the sender is pruned (moved eager→lazy).
%% On an IHAVE for an unknown message, a timer fires and we GRAFT
%% (request the full payload) from the announcing peer.
%%
%% Transport: Erlang distribution — messages are cast directly to the
%% registered name {plumtree_broadcast, RemoteNode}.
-module(plumtree_broadcast).

-behaviour(gen_server).

-export([start_link/1,
         broadcast/2,
         set_peers/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Wire message tags (atoms on the wire via Erlang distribution).
%% Each message carries FromNode so the recipient knows who sent it.
-define(PT_GOSSIP, pt_gossip).  % {pt_gossip, FromNode, MsgId, Payload}
-define(PT_IHAVE,  pt_ihave).   % {pt_ihave,  FromNode, MsgId}
-define(PT_GRAFT,  pt_graft).   % {pt_graft,  FromNode, MsgId}
-define(PT_PRUNE,  pt_prune).   % {pt_prune,  FromNode}

%% How long (ms) to wait for a gossiped message before GRAFTing the IHAVE sender.
-define(GRAFT_TIMEOUT_MS, 2000).

-define(SERVER, ?MODULE).

-record(state, {
    eager_peers :: [node()],
    lazy_peers  :: [node()],
    %% MsgIds we have fully received and delivered.
    received    :: #{binary() => true},
    %% IHAVEs we've heard about but haven't received yet.
    %% MsgId → {SenderNode, TimerRef}
    missing     :: #{binary() => {node(), reference()}},
    %% Payload store: MsgId → Payload (needed to answer GRAFTs).
    payloads    :: #{binary() => binary()},
    %% Application handler module (implements plumtree_handler behaviour).
    handler     :: module()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% Start the broadcast server with the given handler module.
start_link(Handler) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Handler], []).

%% Inject a locally-originated message into the broadcast tree.
broadcast(MsgId, Payload) ->
    gen_server:cast(?SERVER, {local_broadcast, MsgId, Payload}).

%% Replace the eager peer list at runtime (e.g. after cluster membership changes).
set_peers(EagerPeers) ->
    gen_server:call(?SERVER, {set_peers, EagerPeers}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Handler]) ->
    Eager = plumtree_membership:eager_peers(),
    Lazy  = plumtree_membership:lazy_peers(),
    {ok, #state{
        eager_peers = Eager,
        lazy_peers  = Lazy,
        received    = #{},
        missing     = #{},
        payloads    = #{},
        handler     = Handler
    }}.

handle_call({set_peers, EagerPeers}, _From, State) ->
    {reply, ok, State#state{eager_peers = EagerPeers, lazy_peers = []}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% Locally originated message: treat like gossip from self.
handle_cast({local_broadcast, MsgId, Payload}, State) ->
    NewState = gossip_received(MsgId, Payload, node(), State),
    {noreply, NewState};

%% Eager push from another node.
handle_cast({?PT_GOSSIP, FromNode, MsgId, Payload}, State) ->
    NewState = gossip_received(MsgId, Payload, FromNode, State),
    {noreply, NewState};

%% Lazy announcement from another node.
handle_cast({?PT_IHAVE, FromNode, MsgId}, State) ->
    NewState = ihave_received(MsgId, FromNode, State),
    {noreply, NewState};

%% Another node wants a message we have.
handle_cast({?PT_GRAFT, FromNode, MsgId}, State) ->
    NewState = graft_received(MsgId, FromNode, State),
    {noreply, NewState};

%% Another node tells us to stop eager-pushing to it.
handle_cast({?PT_PRUNE, FromNode}, State) ->
    NewState = prune_received(FromNode, State),
    {noreply, NewState}.

handle_info({graft_timeout, MsgId}, #state{missing = Missing} = State) ->
    NewState = case maps:get(MsgId, Missing, undefined) of
        undefined ->
            State;
        {SourceNode, _Ref} ->
            send_to(SourceNode, {?PT_GRAFT, node(), MsgId}),
            %% Remove so a future IHAVE can start a fresh graft cycle.
            State#state{missing = maps:remove(MsgId, Missing)}
    end,
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

gossip_received(MsgId, Payload, FromNode, #state{received = Recv, handler = Handler} = State) ->
    case maps:is_key(MsgId, Recv) of
        false ->
            %% New message: deliver, store, and propagate.
            Handler:deliver(MsgId, Payload),
            State1 = State#state{
                received = Recv#{MsgId => true},
                payloads = (State#state.payloads)#{MsgId => Payload}
            },
            %% Cancel any pending GRAFT timer for this message.
            State2 = cancel_missing(MsgId, State1),
            %% Ensure sender moves to eager.
            State3 = add_eager(FromNode, State2),
            %% Push eagerly (excluding sender), announce lazily.
            eager_push(MsgId, Payload, FromNode, State3),
            lazy_push(MsgId, FromNode, State3),
            State3;
        true ->
            %% Duplicate: prune the sender.
            send_to(FromNode, {?PT_PRUNE, node()}),
            move_to_lazy(FromNode, State)
    end.

ihave_received(MsgId, FromNode, #state{received = Recv, missing = Missing} = State) ->
    case maps:is_key(MsgId, Recv) of
        true ->
            %% We already have it — ignore.
            State;
        false ->
            case maps:is_key(MsgId, Missing) of
                true ->
                    %% Timer already running for this MsgId — ignore.
                    State;
                false ->
                    Ref = erlang:send_after(?GRAFT_TIMEOUT_MS, self(),
                                            {graft_timeout, MsgId}),
                    State#state{missing = Missing#{MsgId => {FromNode, Ref}}}
            end
    end.

graft_received(MsgId, FromNode, #state{payloads = Payloads, handler = Handler} = State) ->
    Payload = case maps:get(MsgId, Payloads, undefined) of
        undefined -> Handler:retrieve(MsgId);
        P         -> {ok, P}
    end,
    case Payload of
        {ok, Data} ->
            send_to(FromNode, {?PT_GOSSIP, node(), MsgId, Data}),
            add_eager(FromNode, State);
        not_found ->
            State
    end.

prune_received(FromNode, State) ->
    move_to_lazy(FromNode, State).

eager_push(MsgId, Payload, ExcludeNode, #state{eager_peers = Eager}) ->
    [send_to(N, {?PT_GOSSIP, node(), MsgId, Payload})
     || N <- Eager, N =/= ExcludeNode].

lazy_push(MsgId, ExcludeNode, #state{lazy_peers = Lazy}) ->
    [send_to(N, {?PT_IHAVE, node(), MsgId})
     || N <- Lazy, N =/= ExcludeNode].

send_to(Node, Msg) ->
    gen_server:cast({?SERVER, Node}, Msg).

add_eager(Node, #state{eager_peers = Eager, lazy_peers = Lazy} = State) ->
    case lists:member(Node, Eager) of
        true  -> State;
        false -> State#state{
                     eager_peers = [Node | Eager],
                     lazy_peers  = lists:delete(Node, Lazy)}
    end.

move_to_lazy(Node, #state{eager_peers = Eager, lazy_peers = Lazy} = State) ->
    case lists:member(Node, Lazy) of
        true  -> State;
        false -> State#state{
                     lazy_peers  = [Node | Lazy],
                     eager_peers = lists:delete(Node, Eager)}
    end.

cancel_missing(MsgId, #state{missing = Missing} = State) ->
    case maps:get(MsgId, Missing, undefined) of
        undefined ->
            State;
        {_Node, Ref} ->
            erlang:cancel_timer(Ref),
            State#state{missing = maps:remove(MsgId, Missing)}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Each test starts a fresh plumtree_broadcast gen_server with the test
%% handler and stops it after.  No real network nodes are involved;
%% send_to/2 casts to dead nodes, which Erlang drops silently.

setup() ->
    plumtree_test_handler:init_table(),
    {ok, Pid} = ?MODULE:start_link(plumtree_test_handler),
    Pid.

teardown(Pid) ->
    gen_server:stop(Pid),
    catch ets:delete(pt_test_deliveries).

%% New gossip: message stored in received, delivered to handler.
gossip_new_delivers_test() ->
    Pid = setup(),
    MsgId = <<"msg-1">>, Payload = <<"hello">>,
    gen_server:cast(Pid, {?PT_GOSSIP, 'fake@peer', MsgId, Payload}),
    timer:sleep(50),
    #state{received = Recv} = sys:get_state(Pid),
    ?assert(maps:is_key(MsgId, Recv)),
    ?assert(plumtree_test_handler:has_msg(MsgId)),
    teardown(Pid).

%% Duplicate gossip: sender moved to lazy_peers.
gossip_duplicate_prunes_test() ->
    Pid = setup(),
    Peer = 'fake@peer',
    %% First put Peer into eager by sending gossip from it.
    gen_server:cast(Pid, {?PT_GOSSIP, Peer, <<"msg-2">>, <<"p">>}),
    timer:sleep(50),
    %% Same message again from same peer → should prune.
    gen_server:cast(Pid, {?PT_GOSSIP, Peer, <<"msg-2">>, <<"p">>}),
    timer:sleep(50),
    #state{lazy_peers = Lazy} = sys:get_state(Pid),
    ?assert(lists:member(Peer, Lazy)),
    teardown(Pid).

%% IHAVE for an unknown message starts a graft timer.
ihave_starts_timer_test() ->
    Pid = setup(),
    MsgId = <<"msg-3">>,
    gen_server:cast(Pid, {?PT_IHAVE, 'fake@peer', MsgId}),
    timer:sleep(50),
    #state{missing = Missing} = sys:get_state(Pid),
    ?assert(maps:is_key(MsgId, Missing)),
    teardown(Pid).

%% IHAVE for a known message is ignored.
ihave_known_ignored_test() ->
    Pid = setup(),
    MsgId = <<"msg-4">>,
    %% Deliver via gossip first.
    gen_server:cast(Pid, {?PT_GOSSIP, 'fake@peer', MsgId, <<"p">>}),
    timer:sleep(50),
    gen_server:cast(Pid, {?PT_IHAVE, 'other@peer', MsgId}),
    timer:sleep(50),
    #state{missing = Missing} = sys:get_state(Pid),
    ?assertNot(maps:is_key(MsgId, Missing)),
    teardown(Pid).

%% Two IHAVEs for the same unknown message: only one timer entry.
ihave_dedup_test() ->
    Pid = setup(),
    MsgId = <<"msg-5">>,
    gen_server:cast(Pid, {?PT_IHAVE, 'peer1@host', MsgId}),
    gen_server:cast(Pid, {?PT_IHAVE, 'peer2@host', MsgId}),
    timer:sleep(50),
    #state{missing = Missing} = sys:get_state(Pid),
    ?assertEqual(1, maps:size(Missing)),
    teardown(Pid).

%% PRUNE from a peer in eager_peers moves it to lazy_peers.
prune_moves_to_lazy_test() ->
    Pid = setup(),
    Peer = 'fake@peer',
    %% Put Peer into eager via gossip.
    gen_server:cast(Pid, {?PT_GOSSIP, Peer, <<"msg-6">>, <<"p">>}),
    timer:sleep(50),
    #state{eager_peers = Eager1} = sys:get_state(Pid),
    ?assert(lists:member(Peer, Eager1)),
    %% Now prune.
    gen_server:cast(Pid, {?PT_PRUNE, Peer}),
    timer:sleep(50),
    #state{eager_peers = Eager2, lazy_peers = Lazy2} = sys:get_state(Pid),
    ?assertNot(lists:member(Peer, Eager2)),
    ?assert(lists:member(Peer, Lazy2)),
    teardown(Pid).

%% Local broadcast: message in received and delivered to handler.
local_broadcast_test() ->
    Pid = setup(),
    MsgId = <<"msg-7">>, Payload = <<"local">>,
    ?MODULE:broadcast(MsgId, Payload),
    timer:sleep(50),
    #state{received = Recv} = sys:get_state(Pid),
    ?assert(maps:is_key(MsgId, Recv)),
    ?assert(plumtree_test_handler:has_msg(MsgId)),
    teardown(Pid).

-endif.
