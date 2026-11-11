%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% No children: the control plane is a stateless plugin over services the
%% ssb app already supervises.  The supervisor exists because an OTP
%% application needs a root process, and as the place to hang any future
%% admin worker (a scheduled compaction job, say).
-module(admin_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.
