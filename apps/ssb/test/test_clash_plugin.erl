%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Test fixture: claims a method already owned by a builtin, so
%% registration must be refused.
-module(test_clash_plugin).
-behaviour(ssb_plugin).

-export([manifest/0, handle_rpc/3]).

manifest() ->
    [{[~"whoami"], sync, anyone}].

handle_rpc(_Name, _Args, _Caller) ->
    {error, ~"never dispatched"}.
