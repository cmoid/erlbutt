%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
-module(silkpurse_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Pid} = silkpurse_sup:start_link(),
    %% Stateless plugins (no process, no view) register here; modules
    %% that own state (silkpurse_backlinks) register themselves from
    %% their own init/handle_continue.
    ok = plugin_registry:register_plugin(silkpurse_db),
    ok = plugin_registry:register_plugin(silkpurse_conn),
    {ok, Pid}.

stop(_State) ->
    plugin_registry:unregister_plugin(silkpurse_db),
    plugin_registry:unregister_plugin(silkpurse_conn),
    ok.
