%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Minimal plumtree_handler for tests.  Deliveries go into a named ETS
%% table owned by a keeper process so the table survives the rpc:call that
%% created it.  Tests can inspect deliveries via rpc:call from any node.
-module(plumtree_test_handler).

-behaviour(plumtree_handler).

-export([deliver/2, retrieve/1,
         init_table/0, deliveries/0, has_msg/1]).

%% Create (or reset) the delivery ETS table, owned by a persistent keeper.
init_table() ->
    %% Kill any existing keeper first.
    case whereis(pt_test_ets_keeper) of
        undefined -> ok;
        OldPid    ->
            unregister(pt_test_ets_keeper),
            exit(OldPid, kill),
            timer:sleep(20)
    end,
    catch ets:delete(pt_test_deliveries),
    Parent = self(),
    Keeper = spawn(fun() ->
        ets:new(pt_test_deliveries, [public, named_table, bag]),
        Parent ! {table_ready, self()},
        %% Block until explicitly killed — keeps the table alive.
        receive _Any -> ok end
    end),
    receive
        {table_ready, Keeper} ->
            register(pt_test_ets_keeper, Keeper),
            ok
    after 1000 ->
        error(table_init_timeout)
    end.

deliveries() ->
    ets:tab2list(pt_test_deliveries).

has_msg(MsgId) ->
    ets:member(pt_test_deliveries, MsgId).

deliver(MsgId, Payload) ->
    ets:insert(pt_test_deliveries, {MsgId, Payload}),
    ok.

retrieve(_MsgId) ->
    not_found.
