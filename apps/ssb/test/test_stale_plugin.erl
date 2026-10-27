%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Test fixture: a plugin, and a view, "built against" plugin API version
%% 0, which no node has ever implemented.  plugin_registry and
%% view_manager must both refuse it.  The attribute is written by hand
%% here; a real plugin gets it from ssb.hrl.
-module(test_stale_plugin).
-behaviour(ssb_plugin).
-behaviour(ssb_view).

-ssb_plugin_api(0).

-export([manifest/0, handle_rpc/3]).
-export([view_version/0, view_load/0, view_reset/0, view_save/0,
         view_entry/1]).

manifest() ->
    [{[~"stale", ~"hello"], async, anyone}].

handle_rpc([~"stale", ~"hello"], Args, _Caller) ->
    {reply, Args}.

view_version() -> 1.
view_load()    -> empty.
view_reset()   -> ok.
view_save()    -> ok.
view_entry(_)  -> ok.
