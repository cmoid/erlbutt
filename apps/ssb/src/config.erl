%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(config).
-include_lib("ssb/include/ssb.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/1,
         ssb_repo_loc/0,
         feed_loc/0,
         blob_loc/0,
         network_id/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {ssb_home,
                repo_loc,
                feed_loc,
                blob_loc,
                net_id}).

%%%===================================================================
%%% API
%%%===================================================================

ssb_repo_loc() ->
    gen_server:call(?MODULE, repo, infinity).

feed_loc() ->
    gen_server:call(?MODULE, feeds, infinity).

blob_loc() ->
    gen_server:call(?MODULE, blobs, infinity).

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
    SSBHome = application:get_env(ssb, ssb_home, "."),
    case filelib:is_file(Config) of
        true ->
            {ok, load_and_parse(Config, #state{ssb_home = SSBHome,
                                               repo_loc = default_repo(SSBHome)})};
        false ->
            %%?LOG_DEBUG("try to load the config from ~p ~n", []),
            {ok, #state{ssb_home = SSBHome,
                        repo_loc = default_repo(SSBHome),
                        feed_loc = default_feed_store(SSBHome),
                        blob_loc = default_blob_store(SSBHome),
                        net_id = default_net_id()}}
    end.

handle_call(repo, _From, #state{repo_loc = RepLoc}=State) ->
    {reply, RepLoc, State};

handle_call(feeds, _From, #state{feed_loc = FeedLoc}=State) ->
    {reply, FeedLoc, State};

handle_call(blobs, _From, #state{blob_loc = BlobLoc}=State) ->
    {reply, BlobLoc, State};

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

parse({feed_store_location, Loc}, #state{repo_loc = RepLoc} = State) ->
    Store = ?l2b(?b2l(RepLoc) ++ Loc),
    filelib:ensure_dir(Store),
    State#state{feed_loc = Store};

parse({blob_store_location, Loc}, #state{repo_loc = RepLoc} = State) ->
    Store = ?l2b(?b2l(RepLoc) ++ Loc),
    filelib:ensure_dir(Store),
    State#state{blob_loc = Store};

parse({network_id, NetId}, State) ->
    State#state{net_id = base64:decode(NetId)};

parse(_Any, State) ->
    %% ignore for now, this is an error technically
    State.

default_repo(SSBHome) ->
    ?l2b(SSBHome ++ "/.ssberl/").

default_feed_store(SSBHome) ->
    DataStore = ?l2b(SSBHome ++ "/.ssberl/feeds/"),
    filelib:ensure_dir(DataStore),
    DataStore.

default_blob_store(SSBHome) ->
    DataStore = ?l2b(SSBHome ++ "/.ssberl/blobs/"),
    filelib:ensure_dir(DataStore),
    DataStore.

default_net_id() ->
    ?DEFAULT_NETWORK_ID.
