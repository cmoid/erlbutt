%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(ssb_sup).

-behaviour(supervisor).

-include_lib("ssb/include/ssb.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    {ok, { {one_for_one, 5, 10},
           [?CHILD(config, worker),
            ?CHILD(heartbeat, worker),
            ?CHILD(keys, worker),
            ?CHILD(blobs, worker),
            ?CHILD(mess_auth, worker),
            ?CHILD(rpc_processor, worker),
            ?CHILD(ebt, worker)]}}.
