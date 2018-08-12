%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2018 Dionne Associates, LLC.
-module(ssb_feed).

%% ssb_feed is a gen_server that represents a single feed. When it
%% is initialized with an id, it retrieves the latest message for the feed
%% and stores it in the state of the gen_server. As new messages are posted
%% they are appended to the feed as per the protocol and persisted in
%% the mnesia table.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ssb.hrl").

-behaviour(gen_server).

-import(msg_store,
        [persist/1,
         fetch_latest_msg/1,
         fetch_msg/1,
         fetch_msgs_author/1,
         fetch_history_feed/2]
       ).

-import(message,
        [encn_store/3]
       ).

%% API
-export([start_link/0,
         start_link/1]).

-export([post_msg/1,
         reinit/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {last_msg}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [keys:pub_key()], []).

start_link(Id) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Id], []).


%% Msg is the content field of a message, the assumption being that
%% last_msg has all the other fields needed
post_msg(Msg) ->
    gen_server:cast(?MODULE, {post, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Id]) ->
    process_flag(trap_exit, true),
    %% initialize state with last_msg from database
    {ok, #state{last_msg = fetch_latest_msg(Id)}}.

reinit() ->
    ok == gen_server:call(?MODULE, {reinit}).

handle_call({reinit}, _From, State) ->
    LatestMsg = fetch_latest_msg(keys:pub_key()),
    {reply, ok, State#state{last_msg = LatestMsg}}.

%% casts

handle_cast({post, Msg}, #state{last_msg = LastMsg} = State) ->
    %% create and persist new msg appended to feed
    NewMsg = postn_store(Msg, LastMsg),
    {noreply, State#state{last_msg = NewMsg}};

handle_cast(_OtherMsg, State) ->
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

postn_store(Msg, nil) ->
    %% First time there is no last msg
    Previous = nil,
    Sequence = 1,
    encn_store(Previous, Sequence, Msg);

postn_store(Msg, LastMsg) ->
    %% create a new msg pointing back to the last msg
    Previous = LastMsg#message.id,
    Sequence = LastMsg#message.sequence + 1,
    encn_store(Previous, Sequence, Msg).

-ifdef(TEST).

simple_test() ->
    init_apps(),
    post_msg(<<"foo">>),
    post_msg(<<"bar">>),
    timer:sleep(500),
    ?assert(true),
    clear().

retrieve_test() ->
    reinit(),
    Msg = postn_store(<<"retrieve">>, nil),
    timer:sleep(500),
    RetMsg = fetch_msg(Msg#message.id),
    ?assert(RetMsg#message.id == Msg#message.id),
    clear().

retrieve_index_test() ->
    reinit(),
    Msg = postn_store(<<"index">>, nil),
    timer:sleep(500),
    [RetMsg] = fetch_msgs_author(Msg#message.author),
    ?assert(RetMsg#message.id == Msg#message.id),
    clear().

create_feed_test() ->
    reinit(),
    post_msg(<<"foo">>),
    post_msg(<<"bar">>),
    post_msg(<<"baz">>),
    timer:sleep(500),
    Msgs = fetch_msgs_author(keys:pub_key()),
    ?assert(length(Msgs) == 3),
    LastMsg = msg_store:fetch_latest_msg(keys:pub_key()),
    ?assert(LastMsg#message.content == <<"baz">>),
    clear().

create_history_test() ->
    reinit(),
    post_msg(<<"foo">>),
    post_msg(<<"bar">>),
    post_msg(<<"baz">>),
    timer:sleep(500),
    Msgs = fetch_history_feed(keys:pub_key(), 0),
    ?assert(length(Msgs) == 3),
    Msgs2 = fetch_history_feed(keys:pub_key(), 1),
    ?assert(length(Msgs2) == 2),
    clear().

init_apps() ->
    application:ensure_all_started(ssb),
    keys:start_link(),
    start_link().

clear() ->
    mnesia:clear_table(message).


-endif.
