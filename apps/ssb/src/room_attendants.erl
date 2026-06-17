%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Presence registry for a room.  Tracks which peers (by feed id) are
%% currently connected, and lets callers subscribe to join/leave events.
%%
%% Each attendant's ssb_peer process and each subscriber process is
%% monitored, so a crash or disconnect removes the entry (and, for an
%% attendant, broadcasts a `left` event) without an explicit call.
-module(room_attendants).

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

%% API
-export([start_link/0,
         join/2,
         leave/1,
         list/0,
         lookup/1,
         subscribe/1,
         unsubscribe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {attendants = #{},   %% FeedId => {Pid, MonRef}
                subscribers = #{}}). %% Pid => MonRef

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Register a connected peer (by feed id) and broadcast a `joined` event.
join(FeedId, Pid) ->
    gen_server:call(?MODULE, {join, FeedId, Pid}).

%% Remove a connected peer (by its pid) and broadcast a `left` event.
leave(Pid) ->
    gen_server:cast(?MODULE, {leave, Pid}).

%% All currently connected feed ids.
list() ->
    gen_server:call(?MODULE, list).

%% Resolve a feed id to its connected ssb_peer pid, or `miss`.
lookup(FeedId) ->
    gen_server:call(?MODULE, {lookup, FeedId}).

%% Subscribe the calling process to {room_event, joined|left, FeedId} messages.
subscribe(Pid) ->
    gen_server:call(?MODULE, {subscribe, Pid}).

unsubscribe(Pid) ->
    gen_server:cast(?MODULE, {unsubscribe, Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({join, FeedId, Pid}, _From,
            #state{attendants = Att, subscribers = Subs} = State) ->
    %% Replacing an existing entry for this feed (e.g. reconnect): drop the
    %% stale monitor first so its DOWN does not later evict the new pid.
    Att1 = case maps:find(FeedId, Att) of
        {ok, {_OldPid, OldRef}} -> erlang:demonitor(OldRef, [flush]), Att;
        error -> Att
    end,
    Ref = erlang:monitor(process, Pid),
    broadcast(joined, FeedId, Subs),
    {reply, ok, State#state{attendants = maps:put(FeedId, {Pid, Ref}, Att1)}};

handle_call(list, _From, #state{attendants = Att} = State) ->
    {reply, maps:keys(Att), State};

handle_call({lookup, FeedId}, _From, #state{attendants = Att} = State) ->
    Reply = case maps:find(FeedId, Att) of
        {ok, {Pid, _Ref}} -> {ok, Pid};
        error             -> miss
    end,
    {reply, Reply, State};

handle_call({subscribe, Pid}, _From, #state{subscribers = Subs} = State) ->
    Subs1 = case maps:is_key(Pid, Subs) of
        true  -> Subs;
        false -> maps:put(Pid, erlang:monitor(process, Pid), Subs)
    end,
    {reply, ok, State#state{subscribers = Subs1}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({leave, Pid}, State) ->
    {noreply, remove_attendant_by_pid(Pid, State)};

handle_cast({unsubscribe, Pid}, #state{subscribers = Subs} = State) ->
    case maps:take(Pid, Subs) of
        {Ref, Subs1} -> erlang:demonitor(Ref, [flush]),
                        {noreply, State#state{subscribers = Subs1}};
        error        -> {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason},
            #state{subscribers = Subs} = State) ->
    case maps:is_key(Pid, Subs) of
        true  -> {noreply, State#state{subscribers = maps:remove(Pid, Subs)}};
        false -> {noreply, remove_attendant_by_pid(Pid, State)}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Remove the attendant whose pid matches, demonitor, and broadcast `left`.
remove_attendant_by_pid(Pid, #state{attendants = Att, subscribers = Subs} = State) ->
    case [{F, R} || {F, {P, R}} <- maps:to_list(Att), P =:= Pid] of
        [{FeedId, Ref} | _] ->
            erlang:demonitor(Ref, [flush]),
            broadcast(left, FeedId, Subs),
            State#state{attendants = maps:remove(FeedId, Att)};
        [] ->
            State
    end.

broadcast(Kind, FeedId, Subs) ->
    [P ! {room_event, Kind, FeedId} || P <- maps:keys(Subs)],
    ok.
