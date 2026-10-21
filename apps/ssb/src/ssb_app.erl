%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(ssb_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    LogLevel = application:get_env(ssb, ssb_log_level, notice),
    logger:set_primary_config(level, LogLevel),
    logger:set_module_level(supervisor, error),
    ?LOG(LogLevel, "Log level ~p set from env ~n", [LogLevel]),

    %% Before anything opens a socket or touches the store: refuse to run
    %% against data written by a newer build.  Not binding the port is part
    %% of not running — a half-started node that answers peers while
    %% declining to read its own floors is worse than one that stopped.
    %%
    %% And before even that: one node per data directory (store_lock).
    Dir = ssb_paths:repo_dir(),
    case store_lock:acquire(Dir) of
        ok ->
            case data_version:check() of
                ok ->
                    case start_node(LogLevel) of
                        {ok, _} = Ok -> Ok;
                        Err          -> _ = store_lock:release(Dir), Err
                    end;
                {error, _} = Err ->
                    _ = store_lock:release(Dir),
                    Err
            end;
        {error, {locked, Holder}} = Err ->
            ?SSB_ERROR("REFUSING TO START: ~s is in use by another erlbutt "
                       "node (~p).  Two nodes on one store fork the "
                       "identity.  Stop that one first; if it is truly "
                       "gone, delete ~sLOCK.", [Dir, Holder, Dir]),
            Err;
        {error, Reason} = Err ->
            ?SSB_ERROR("REFUSING TO START: cannot lock ~s: ~p", [Dir, Reason]),
            Err
    end.

start_node(_LogLevel) ->
    Port = application:get_env(ssb, port, 8008),
    catch ranch:stop_listener(erlbutt_listener),
    {ok, _} = ranch:start_listener(erlbutt_listener,
                                   ranch_tcp,
                                   #{socket_opts => [{port, Port}],
                                     max_connections => 10,
                                     num_acceptors => 5},
                                   ssb_peer, []),

    ssb_sup:start_link().

stop(_State) ->
    _ = store_lock:release(ssb_paths:repo_dir()),
    ok.

-ifdef(TEST).

simple_test() ->
    %% Port 0 asks the OS for an ephemeral one.  This used to take the real
    %% listen port, so the whole suite failed with eaddrinuse whenever a
    %% node happened to be running on the same machine — which is exactly
    %% when you are most likely to be running the tests.
    application:set_env(ssb, port, 0),
    application:ensure_all_started(ssb),
    ?assertNot(undefined == whereis(ssb_sup)),
    application:stop(ssb).

-endif.
