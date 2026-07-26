%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
-module(admin_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% admin_rpc is stateless, so it can register immediately — there is
    %% no index to build before its methods can answer.
    ok = plugin_registry:register_plugin(admin_rpc),
    admin_sup:start_link().

stop(_State) ->
    plugin_registry:unregister_plugin(admin_rpc),
    ok.
