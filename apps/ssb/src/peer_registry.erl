%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Tracks active peer connections (inbound and outbound) keyed by raw public
%% key bytes.  Serialises register/2 through a gen_server to avoid TOCTOU;
%% is_connected/1 reads the ETS table directly for a cheap pre-flight check.
-module(peer_registry).

-behaviour(gen_server).

-export([start_link/0,
         register/2,
         unregister/1,
         is_connected/1,
         find/1,
         all/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("ssb/include/ssb.hrl").

-define(TAB, peer_connections).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Register Pid as the active connection for PubKey.
%% Returns ok, or {duplicate, ExistingPid} if already connected.
register(PubKey, Pid) ->
    gen_server:call(?MODULE, {register, PubKey, Pid}).

%% Deregister PubKey on connection teardown.  Safe to call with undefined.
unregister(undefined) -> ok;
unregister(PubKey) ->
    gen_server:cast(?MODULE, {unregister, PubKey}).

%% Cheap ETS read — no gen_server round-trip.
is_connected(PubKey) ->
    case ets:lookup(?TAB, PubKey) of
        [{PubKey, Pid}] -> is_process_alive(Pid);
        []              -> false
    end.

%% Return {ok, Pid} if a live connection for PubKey exists, miss otherwise.
find(PubKey) ->
    case ets:lookup(?TAB, PubKey) of
        [{PubKey, Pid}] ->
            case is_process_alive(Pid) of
                true  -> {ok, Pid};
                false -> miss
            end;
        [] ->
            miss
    end.

%% Return [{PubKey, Pid}] for all currently registered connections.
all() ->
    ets:tab2list(?TAB).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?TAB, [set, named_table, public]),
    {ok, #{}}.

handle_call({register, PubKey, Pid}, _From, State) ->
    Reply = case ets:lookup(?TAB, PubKey) of
        [{PubKey, Existing}] ->
            case is_process_alive(Existing) of
                true  -> {duplicate, Existing};
                false ->
                    ets:insert(?TAB, {PubKey, Pid}),
                    ok
            end;
        [] ->
            ets:insert(?TAB, {PubKey, Pid}),
            ok
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({unregister, PubKey}, State) ->
    ets:delete(?TAB, PubKey),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
