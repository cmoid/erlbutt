%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
-module(silkpurse_sup).

-behaviour(supervisor).

-include_lib("ssb/include/ssb.hrl").

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10},
          [?CHILD(silkpurse_backlinks, worker),
           ?CHILD(silkpurse_by_type, worker),
           ?CHILD(silkpurse_about, worker)]}}.
