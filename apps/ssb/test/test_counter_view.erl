%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Test fixture: a minimal ssb_view recording every delivered message as
%% {entry, FeedId, Seq} in a public bag table.  Its version is read from
%% application env so tests can force a rebuild without recompiling.
-module(test_counter_view).
-behaviour(ssb_view).

-include_lib("ssb/include/ssb.hrl").

-export([view_version/0, view_load/0, view_reset/0, view_save/0,
         view_entry/1]).
-export([ensure_table/0, entries/1]).

-define(TAB, test_counter_view_tab).

%% Called by the test before registering; the table belongs to the test
%% process, mirroring how a real view's gen_server owns its tables.
ensure_table() ->
    case ets:info(?TAB) of
        undefined -> ets:new(?TAB, [bag, named_table, public]);
        _         -> ?TAB
    end,
    ok.

%% Deliveries recorded for FeedId, in sequence order.
entries(FeedId) ->
    lists:sort([Seq || {entry, F, Seq} <- ets:tab2list(?TAB), F =:= FeedId]).

view_version() ->
    application:get_env(ssb, test_view_version, 1).

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
    {events, [{seen, FeedId, Seq}]}.
