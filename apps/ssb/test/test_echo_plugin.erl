%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Test fixture: a minimal ssb_plugin used by plugin_registry eunit
%% tests and the plugin CT cases.
-module(test_echo_plugin).
-behaviour(ssb_plugin).

-export([manifest/0, handle_rpc/3]).

manifest() ->
    [{[~"echo", ~"hello"],   async,  anyone},
     {[~"echo", ~"whoAmI"],  async,  anyone},
     {[~"echo", ~"secrets"], async,  owner},
     {[~"echo", ~"count"],   source, anyone}].

handle_rpc([~"echo", ~"hello"], Args, _Caller) ->
    {reply, Args};

handle_rpc([~"echo", ~"whoAmI"], _Args, #{feed_id := FeedId, class := Class}) ->
    {reply, {[{~"feed", FeedId}, {~"class", atom_to_binary(Class)}]}};

handle_rpc([~"echo", ~"secrets"], _Args, _Caller) ->
    {reply, ~"the owner's secrets"};

handle_rpc([~"echo", ~"count"], [N], _Caller) when is_integer(N), N > 0 ->
    {source, lists:seq(1, N)};

handle_rpc([~"echo", ~"count"], _Args, _Caller) ->
    {error, ~"count takes a positive integer"}.
