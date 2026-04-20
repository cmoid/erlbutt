%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2025 Charles Moid
%%
%% Supervisor for ssb_feed gen_servers.
%%
%% Each feed (identified by its @pubkey.ed25519 string) gets one
%% gen_server started on demand and restarted by this supervisor if it
%% crashes.  A named ETS table (ssb_feed_registry) maps FeedId → Pid so
%% that any process can locate an existing feed in O(1) without going
%% through this supervisor.
%%
%% Ownership of ssb_feed_registry: the table is created here in
%% start_link/0, which is called from ssb_sup's process.  ssb_sup is
%% permanent for the lifetime of the application, so the table is never
%% prematurely destroyed.  If ssb_feed_sup itself is restarted while
%% ssb_sup remains alive, we clear the stale entries (all feed children
%% have already been killed by the supervisor restart).
-module(ssb_feed_sup).

-include_lib("ssb/include/ssb.hrl").

-behaviour(supervisor).

-export([start_link/0,
         find_or_start/1]).

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    case whereis(?MODULE) of
        undefined ->
            %% Fresh start: set up registry then start the supervisor.
            ensure_registry(),
            supervisor:start_link({local, ?MODULE}, ?MODULE, []);
        Pid ->
            %% Already running (e.g. called repeatedly in unit tests).
            {ok, Pid}
    end.

%% Create the registry ETS table, or clear it if it already exists
%% (which means ssb_feed_sup crashed and is being restarted by ssb_sup;
%% all feed children are dead so stale entries must be removed).
ensure_registry() ->
    case ets:info(ssb_feed_registry) of
        undefined ->
            ets:new(ssb_feed_registry, [set, public, named_table]);
        _ ->
            ets:delete_all_objects(ssb_feed_registry)
    end.

%% Return the pid of the feed gen_server for FeedId, starting one if
%% necessary.  Returns 'bad' when FeedId fails the id check.
find_or_start(FeedId) ->
    case ets:lookup(ssb_feed_registry, FeedId) of
        [{FeedId, Pid}] ->
            case is_process_alive(Pid) of
                true  -> Pid;
                false ->
                    %% Stale entry — clean up and start fresh.
                    ets:delete(ssb_feed_registry, FeedId),
                    do_start(FeedId)
            end;
        [] ->
            do_start(FeedId)
    end.

%%%===================================================================
%%% supervisor callback
%%%===================================================================

init([]) ->
    ChildSpec = #{id       => ssb_feed,
                  start    => {ssb_feed, start_link, []},
                  restart  => transient,
                  shutdown => 5000,
                  type     => worker,
                  modules  => [ssb_feed]},
    {ok, {{simple_one_for_one, 10, 60}, [ChildSpec]}}.

%%%===================================================================
%%% Internal
%%%===================================================================

do_start(FeedId) ->
    case supervisor:start_child(?MODULE, [FeedId]) of
        {ok, Pid}                       -> Pid;
        {error, {already_started, Pid}} -> Pid;
        {error, Reason} ->
            ?LOG_ERROR("ssb_feed_sup: failed to start feed ~p: ~p~n",
                                   [FeedId, Reason]),
            error(Reason)
    end.
