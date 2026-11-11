%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% A singleton timer that publishes a tick to the `silkpurse_heartbeat'
%% view-event group a little under once a second.  patchwork.heartbeat is
%% a live_source over that group (see silkpurse_patchwork); the client's
%% progress-notifier resets its "waiting" flag on every tick, so a steady
%% pulse keeps the "Scuttling..." spinner hidden once the node is up.
%%
%% Publishing is a no-op when no heartbeat streams are open (the pg group
%% is empty), so the timer costs nothing until a client subscribes.
-module(silkpurse_heartbeat).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(VIEW, silkpurse_heartbeat).
%% Under the client's 1000ms staleness timeout, with margin.
-define(INTERVAL_MS, 900).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    schedule(),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    catch view_manager:notify(?VIEW, tick),
    schedule(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

schedule() ->
    erlang:send_after(?INTERVAL_MS, self(), tick).
