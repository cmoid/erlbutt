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
-export([start_link/2]).

-export([process_msg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {feed,
                meta}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(FeedId, Location) ->
    gen_server:start_link(?MODULE, [FeedId, Location], []).

%% Msg is the content field of a message, the assumption being that
%% last_msg has all the other fields needed
process_msg(FeedPid, Msg) ->
    gen_server:cast(FeedPid, {process, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([FeedId, Location]) ->
    process_flag(trap_exit, true),
    DecodeId = integer_to_binary(binary:decode_unsigned(base64:decode(FeedId)),16),
    {Feed, Meta} = init_directories(DecodeId, Location),
    {ok, #state{feed = Feed, meta = Meta}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({process, Msg}, #state{feed = Feed} = State) ->
    store(Msg, Feed),
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
store(Msg, Feed) ->
    {ok, Out} = file:open(Feed, [append]),
    DataSiz = size(Msg),
    file:write(Out,
               <<DataSiz:32, Msg/binary, DataSiz:32>>),
    FileSize = filelib:file_size(Feed),
    file:write(Out, <<FileSize:32>>),
    file:close(Out).

init_directories(AuthDir, Location) ->
    %% Author is already decoded as hex, use first two chars for directory
    <<Dir:2/binary,RestAuth/binary>> = AuthDir,
    file:make_dir(<<Location/binary,Dir/binary>>),
    FeedDir = <<Location/binary,Dir/binary,<<"/">>/binary,RestAuth/binary>>,
    file:make_dir(FeedDir),
    %% write msg to feed
    Feed = <<FeedDir/binary,<<"/">>/binary,<<"log.offset">>/binary>>,
    Meta = <<FeedDir/binary,<<"/">>/binary,<<"meta">>/binary>>,
    {Feed, Meta}.

-ifdef(TEST).

-endif.
