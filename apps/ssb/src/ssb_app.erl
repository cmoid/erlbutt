%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2018 Dionne Associates, LLC.
-module(ssb_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    {ok, _} = ranch:start_listener(erlbutt_listener, 5,
                                   ranch_tcp, [{port, 8008},
                                               {max_connections, 10}],
                                   ssb_peer, []),
    LogLevel = application:get_env(ssb, ssb_log_level, notice),
    ?notice("Log level ~p set from env ~n", [LogLevel]),
    logger:set_primary_config(level, LogLevel),
    logger:set_module_level(supervisor, error),

    ssb_sup:start_link().

stop(_State) ->
    ok.


-ifdef(TEST).

simple_test() ->
    application:ensure_all_started(ssb),
    ?assertNot(undefined == whereis(ssb_sup)).

-endif.
