%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Test fixture: the same minimal ssb_view as test_counter_view, but
%% declaring view_class() -> core.  Used to exercise view_manager's
%% core-before-app ordering without registering a real core view into a
%% test fixture (doc/persistence.md §5).
-module(test_core_view).
-behaviour(ssb_view).

-include_lib("ssb/include/ssb.hrl").

-export([view_version/0, view_class/0, view_load/0, view_reset/0,
         view_save/0, view_entry/1]).
-export([ensure_table/0, entries/1]).

-define(TAB, test_core_view_tab).

ensure_table() ->
    case ets:info(?TAB) of
        undefined -> ets:new(?TAB, [bag, named_table, public]);
        _         -> ?TAB
    end,
    ok.

entries(FeedId) ->
    lists:sort([Seq || {entry, F, Seq} <- ets:tab2list(?TAB), F =:= FeedId]).

view_version() -> 1.

view_class() -> core.

view_load() ->
    case ets:lookup(?TAB, marker) of
        [] -> empty;
        _  -> ok
    end.

view_reset() ->
    ets:delete_all_objects(?TAB),
    ok.

view_save() ->
    ets:insert(?TAB, {marker}),
    ok.

view_entry(#message{author = FeedId, sequence = Seq}) ->
    ets:insert(?TAB, {entry, FeedId, Seq}),
    ok.
