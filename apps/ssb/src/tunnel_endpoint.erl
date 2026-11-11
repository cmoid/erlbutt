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

-export([start_link/0, set_listener/1, listener/0,
         set_server_notify/1, accept/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {listener = undefined,       %% test override sink
                server_notify = undefined}). %% observer for default SHS servers

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Register (or clear, with undefined) the process that accepts tunnels.
set_listener(Pid) ->
    gen_server:call(?MODULE, {set_listener, Pid}).

%% The currently registered listener pid, or undefined.
listener() ->
    gen_server:call(?MODULE, listener).

%% Register an observer pid notified when a default SHS server accepts a tunnel.
set_server_notify(Pid) ->
    gen_server:call(?MODULE, {set_server_notify, Pid}).

%% Accept an incoming tunnel.connect (called by rpc_processor on a non-room
%% node).  Returns {ok, SinkPid} naming the process that should receive this
%% stream's frames, or `none` if accepting is disabled.
%%   - If a test listener is set, route frames to it and announce the open.
%%   - Otherwise spawn a tunnel_conn server that runs SHS over the tunnel.
accept(ReqNo, OwnerPid) ->
    gen_server:call(?MODULE, {accept, ReqNo, OwnerPid}).

init([]) ->
    {ok, #state{}}.

handle_call({set_listener, Pid}, _From, State) ->
    {reply, ok, State#state{listener = Pid}};

handle_call(listener, _From, #state{listener = L} = State) ->
    {reply, L, State};

handle_call({set_server_notify, Pid}, _From, State) ->
    {reply, ok, State#state{server_notify = Pid}};

handle_call({accept, ReqNo, OwnerPid}, _From,
            #state{listener = Listener, server_notify = Notify} = State) ->
    Reply = case Listener of
        Pid when is_pid(Pid) ->
            Pid ! {tunnel_open, ReqNo, OwnerPid},
            {ok, Pid};
        _ ->
            %% Default: accept and run a full peer over the tunnel as server.
            %% Room sends to us on +ReqNo; we reply on -ReqNo.
            _ = Notify,
            Conn = ssb_peer:start_tunnel_server(OwnerPid, ReqNo, -ReqNo),
            {ok, Conn}
    end,
    {reply, Reply, State};

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
