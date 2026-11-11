%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
-module(plumtree_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Handler = application:get_env(plumtree, handler, ssb_feed_handler),
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    Children = [
        #{id       => plumtree_broadcast,
          start    => {plumtree_broadcast, start_link, [Handler]},
          restart  => permanent,
          shutdown => 5000,
          type     => worker,
          modules  => [plumtree_broadcast]}
    ],
    {ok, {SupFlags, Children}}.
