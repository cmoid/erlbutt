%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% The patchwork.* methods the renderer binds that do not yet have a
%% real implementation.  They are stubbed — correct muxrpc kind, benign
%% empty result — so the whole UI binds and the main feed renders; real
%% implementations (the feed rollups, likes, thread, ...) replace these
%% one at a time.  publicFeed lives in silkpurse_threads and contacts in
%% silkpurse_contacts (both real).  Registered by silkpurse_app.
-module(silkpurse_patchwork).

-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

-export([manifest/0, handle_rpc/3]).

manifest() ->
    %% Feed rollups live in silkpurse_threads now, EXCEPT privateFeed
    %% (needs private unboxing) which stays stubbed here.
    [{[~"patchwork", ~"privateFeed", ~"roots"],        source, owner},
     {[~"patchwork", ~"privateFeed", ~"latest"],       source, owner},
     %% Discovery / sidebar.
     {[~"patchwork", ~"recentFeeds"],                  source, owner},
     {[~"patchwork", ~"channels", ~"get"],             async,  owner},
     {[~"patchwork", ~"channels", ~"stream"],          source, owner},
     %% (patchwork.channels.suggest/recentStream are real — silkpurse_channels.)
     {[~"patchwork", ~"subscriptions"],                source, owner},
     %% (patchwork.suggest.profile is real — silkpurse_db;
     %%  patchwork.profile.avatar is real — silkpurse_about.)
     %% Message rendering.  (patchwork.likes.* is real — silkpurse_likes.)
     {[~"patchwork", ~"backlinks", ~"referencesStream"], source, owner},
     {[~"patchwork", ~"backlinks", ~"forksStream"],    source, owner},
     {[~"patchwork", ~"liveBacklinks", ~"stream"],     source, owner},
     {[~"patchwork", ~"liveBacklinks", ~"subscribe"],  async,  owner},
     {[~"patchwork", ~"liveBacklinks", ~"unsubscribe"], async, owner},
     %% (patchwork.thread.sorted is real — silkpurse_thread.)
     %% App shell.
     {[~"patchwork", ~"progress"],                     source, owner},
     {[~"patchwork", ~"heartbeat"],                    source, owner},
     {[~"patchwork", ~"disconnect"],                   async,  owner}].

%% Async getters return an empty object/list; source streams are empty.
%% Side-effecting async methods acknowledge with true.
handle_rpc([~"patchwork", ~"channels", ~"get"], _Args, _Caller) ->
    {reply, []};
handle_rpc([~"patchwork", ~"liveBacklinks", ~"subscribe"], _Args, _Caller) ->
    {reply, true};
handle_rpc([~"patchwork", ~"liveBacklinks", ~"unsubscribe"], _Args, _Caller) ->
    {reply, true};
handle_rpc([~"patchwork", ~"disconnect"], _Args, _Caller) ->
    {reply, true};

%% A live source that emits a tick just under once a second, driven by
%% the silkpurse_heartbeat timer through the view-event pg group.  The
%% client's progress-notifier resets its "waiting" flag on every tick,
%% so this keeps the "Scuttling..." spinner hidden once the node is up.
%% The frame body is an arbitrary JSON value (the emit time); the client
%% only cares that a frame arrived.
handle_rpc([~"patchwork", ~"heartbeat"], _Args, _Caller) ->
    EventFun = fun(_) ->
                       {send, integer_to_binary(erlang:system_time(millisecond))}
               end,
    {live_source, [], silkpurse_heartbeat, EventFun};

%% Everything else registered here is a source stub: an empty stream.
handle_rpc([~"patchwork" | _], _Args, _Caller) ->
    {source, []}.
