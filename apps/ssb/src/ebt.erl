%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2025 Charles Moid
-module(ebt).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("ssb/include/ssb.hrl").

-behaviour(gen_server).
-behavior(rpc_behavior).

%% API
-export([start_link/0,
         initial_vector/0,
         exec_rpc/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% send simple vector with feed of the peer node,
%% in case this is a restore of a backup
initial_vector() ->
    PeerKey = keys:pub_key_disp(),
    {[{PeerKey, ebt_vc:encode_clock_int(true, true, 0)}]}.

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(Request, _From, State) when is_number(Request) ->
    Reply = ok,
    ?LOG_DEBUG("Callback did run: ~p ~n",[Request]),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

exec_rpc(Request) ->
    gen_server:call(?SERVER, Request).
