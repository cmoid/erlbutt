%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% A live muxrpc source stream fed by view events: the server half of
%% {live: true}.  One process per open stream.  It subscribes to a
%% view's change events before the snapshot is sent (so nothing falls
%% in the gap), buffers while the snapshot goes out, then deduplicates
%% against the snapshot's message ids and pushes every later match as
%% a stream frame through the owning connection's ordered write path
%% (ssb_peer:send_frame).
%%
%% The plugin supplies EventFun(Event) -> {send, MsgId, Bin} | skip,
%% where Bin is the encoded message; it runs in this process.
%%
%% Lifecycle: dies with the owning connection (monitor); stop/1 when
%% the client cancels the stream.
-module(view_stream).

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

-export([start/4,
         release/2,
         stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(vs_state, {owner,          %% ssb_peer pid (ordered writes)
                   req_no,         %% negative ReqNo frames are sent on
                   view,           %% view module whose events we follow
                   event_fun,      %% fun(Event) -> {send, MsgId, Bin} | skip
                   buffer = [],    %% events buffered until release/2
                   skip,           %% set of MsgIds already sent (snapshot)
                   released = false}).

%%%===================================================================
%%% API
%%%===================================================================

start(OwnerPid, ReqNo, ViewMod, EventFun) ->
    gen_server:start(?MODULE, [OwnerPid, ReqNo, ViewMod, EventFun], []).

%% The snapshot has been sent; SentIds were in it.  Flush buffered
%% events (minus SentIds) and go live.
release(Pid, SentIds) ->
    gen_server:cast(Pid, {release, SentIds}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([OwnerPid, ReqNo, ViewMod, EventFun]) ->
    erlang:monitor(process, OwnerPid),
    ok = view_manager:subscribe(ViewMod),
    {ok, #vs_state{owner = OwnerPid,
                   req_no = ReqNo,
                   view = ViewMod,
                   event_fun = EventFun,
                   skip = sets:new()}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({release, SentIds}, #vs_state{buffer = Buffered} = State) ->
    Skip = sets:from_list(SentIds),
    State1 = State#vs_state{released = true, buffer = [], skip = Skip},
    State2 = lists:foldl(fun(Event, S) -> push(Event, S) end,
                         State1, lists:reverse(Buffered)),
    {noreply, State2};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({view_event, View, Event},
            #vs_state{view = View, released = false,
                      buffer = Buffered} = State) ->
    {noreply, State#vs_state{buffer = [Event | Buffered]}};

handle_info({view_event, View, Event},
            #vs_state{view = View} = State) ->
    {noreply, push(Event, State)};

handle_info({'DOWN', _Ref, process, Owner, _Reason},
            #vs_state{owner = Owner} = State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #vs_state{view = View}) ->
    catch view_manager:unsubscribe(View),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

push(Event, #vs_state{owner = Owner, req_no = ReqNo,
                      event_fun = EventFun, skip = Skip} = State) ->
    try EventFun(Event) of
        {send, MsgId, Bin} ->
            case sets:is_element(MsgId, Skip) of
                true ->
                    %% already sent in the snapshot; from here on the
                    %% skip set has served its purpose for this id
                    State#vs_state{skip = sets:del_element(MsgId, Skip)};
                false ->
                    ok = ssb_peer:send_frame(Owner, ReqNo, Bin),
                    State
            end;
        {send, Bin} ->
            %% no dedup: for value streams (e.g. about.socialValueStream)
            %% that re-emit a recomputed value on every change rather than
            %% pushing distinct stored messages
            ok = ssb_peer:send_frame(Owner, ReqNo, Bin),
            State;
        skip ->
            State
    catch C:R ->
            ?SSB_ERROR("view_stream: event fun crashed: ~p:~p", [C, R]),
            State
    end.
