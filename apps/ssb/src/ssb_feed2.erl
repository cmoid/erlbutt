%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2018 Dionne Associates, LLC.
-module(ssb_feed2).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ssb.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/1]).

-export([post_msg/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {feed_id}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [keys:pub_key()], []).

start_link(FeedId) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [FeedId], []).

%% Msg is the content field of a message, the assumption being that
%% last_msg has all the other fields needed
post_msg(Msg) ->
    gen_server:cast(?MODULE, {post, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([FeedId]) ->
    process_flag(trap_exit, true),
    {ok, #state{feed_id = FeedId}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({post, Msg}, State) ->
    store(Msg),
    {noreply, State}.

%% info

handle_info(_Info, State) ->
    {noreply, State}.

%%

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
store(_Msg) ->
    ok.

-ifdef(TEST).

-endif.
