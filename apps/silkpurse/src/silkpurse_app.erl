%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
-module(silkpurse_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Register the stateless plugins BEFORE starting the view
    %% supervisor.  The view children restore their tables and catch up
    %% from the log on startup, which can take a while; doing that first
    %% would delay these methods (conn.*, contacts.*, …) appearing in the
    %% served manifest, so a client connecting during boot would get an
    %% incomplete API.  With no state to build, these can register
    %% immediately.
    ok = plugin_registry:register_plugin(silkpurse_db),
    ok = plugin_registry:register_plugin(silkpurse_conn),
    ok = plugin_registry:register_plugin(silkpurse_contacts),
    ok = plugin_registry:register_plugin(silkpurse_thread),
    ok = plugin_registry:register_plugin(silkpurse_patchwork),
    silkpurse_sup:start_link().

stop(_State) ->
    plugin_registry:unregister_plugin(silkpurse_db),
    plugin_registry:unregister_plugin(silkpurse_conn),
    plugin_registry:unregister_plugin(silkpurse_contacts),
    plugin_registry:unregister_plugin(silkpurse_thread),
    plugin_registry:unregister_plugin(silkpurse_patchwork),
    ok.
