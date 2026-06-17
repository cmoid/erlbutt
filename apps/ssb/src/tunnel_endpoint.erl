%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Registry for the local tunnel listener: the process that accepts incoming
%% tunnelled connections relayed by a room.  When a non-room node receives a
%% tunnel.connect, rpc_processor hands the stream to whatever pid is set here.
%%
%% The listener receives:
%%   {tunnel_open, ReqNo, OwnerPid}  -- a new tunnel; reply on -ReqNo via
%%                                      ssb_peer:send_frame(OwnerPid, -ReqNo, Body)
%%   {tunnel_data, ReqNo, Body}      -- a frame on that tunnel
%%
%% Phase 2c will register an SHS-over-tunnel server here.
-module(tunnel_endpoint).

-behaviour(gen_server).

-export([start_link/0, set_listener/1, listener/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {listener = undefined}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Register (or clear, with undefined) the process that accepts tunnels.
set_listener(Pid) ->
    gen_server:call(?MODULE, {set_listener, Pid}).

%% The currently registered listener pid, or undefined.
listener() ->
    gen_server:call(?MODULE, listener).

init([]) ->
    {ok, #state{}}.

handle_call({set_listener, Pid}, _From, State) ->
    {reply, ok, State#state{listener = Pid}};

handle_call(listener, _From, #state{listener = L} = State) ->
    {reply, L, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
