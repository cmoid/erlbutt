%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(config).
-include("ssb.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/1,
         feed_store_loc/0,
         network_id/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {store_loc,
                net_id}).

%%%===================================================================
%%% API
%%%===================================================================

feed_store_loc() ->
    gen_server:call(?MODULE, location, infinity).

network_id() ->
    gen_server:call(?MODULE, netid, infinity).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, ["ssb.cfg"], []).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Config]) ->
    process_flag(trap_exit, true),
    case filelib:is_file(Config) of
        true ->
            {ok, load_and_parse(Config, #state{})};
        false ->
            {ok, #state{store_loc = default_feed_store(),
                        net_id = default_net_id()}}
    end.

handle_call(location, _From, #state{store_loc = DbLoc}=State) ->
    {reply, DbLoc, State};

handle_call(netid, _From, #state{net_id = NetId}=State) ->
    {reply, NetId, State}.

%% casts

handle_cast(_Msg, State) ->
    {noreply, State}.

%% info

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
load_and_parse(CfgFile, #state{} = State) ->
    {ok, CfgTerms} = file:consult(CfgFile),
    lists:foldl(fun(CfgTerm, StateIn) ->
                        parse(CfgTerm, StateIn)
                end, State, CfgTerms).

parse({feed_store_location, Loc}, State) ->
    Store = ?l2b(Loc),
    filelib:ensure_dir(Store),
    State#state{store_loc = Store};

parse({network_id, NetId}, State) ->
    State#state{net_id = base64:decode(NetId)};

parse(_Any, State) ->
    %% ignore for now, this is an error technically
    State.

default_feed_store() ->
    DataStore = ?l2b("./feeds/"),
    filelib:ensure_dir(DataStore),
    DataStore.

default_net_id() ->
    ?DEFAULT_NETWORK_ID.
