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
    %% Feed rollups (each roots + latest, like publicFeed).
    [{[~"patchwork", ~"participatingFeed", ~"roots"],  source, owner},
     {[~"patchwork", ~"participatingFeed", ~"latest"], source, owner},
     {[~"patchwork", ~"mentionsFeed", ~"roots"],       source, owner},
     {[~"patchwork", ~"mentionsFeed", ~"latest"],      source, owner},
     {[~"patchwork", ~"privateFeed", ~"roots"],        source, owner},
     {[~"patchwork", ~"privateFeed", ~"latest"],       source, owner},
     {[~"patchwork", ~"networkFeed", ~"roots"],        source, owner},
     {[~"patchwork", ~"networkFeed", ~"latest"],       source, owner},
     {[~"patchwork", ~"channelFeed", ~"roots"],        source, owner},
     {[~"patchwork", ~"channelFeed", ~"latest"],       source, owner},
     {[~"patchwork", ~"profile", ~"roots"],            source, owner},
     {[~"patchwork", ~"profile", ~"latest"],           source, owner},
     {[~"patchwork", ~"profile", ~"avatar"],           async,  owner},
     %% Discovery / sidebar.
     {[~"patchwork", ~"recentFeeds"],                  source, owner},
     {[~"patchwork", ~"channels", ~"get"],             async,  owner},
     {[~"patchwork", ~"channels", ~"stream"],          source, owner},
     {[~"patchwork", ~"channels", ~"suggest"],         async,  owner},
     {[~"patchwork", ~"channels", ~"recentStream"],    source, owner},
     {[~"patchwork", ~"subscriptions"],                source, owner},
     {[~"patchwork", ~"suggest", ~"profile"],          async,  owner},
     %% Message rendering.
     {[~"patchwork", ~"likes", ~"get"],                async,  owner},
     {[~"patchwork", ~"likes", ~"countStream"],        source, owner},
     {[~"patchwork", ~"likes", ~"feedLikesMsgStream"], source, owner},
     {[~"patchwork", ~"backlinks", ~"referencesStream"], source, owner},
     {[~"patchwork", ~"backlinks", ~"forksStream"],    source, owner},
     {[~"patchwork", ~"liveBacklinks", ~"stream"],     source, owner},
     {[~"patchwork", ~"liveBacklinks", ~"subscribe"],  async,  owner},
     {[~"patchwork", ~"liveBacklinks", ~"unsubscribe"], async, owner},
     {[~"patchwork", ~"thread", ~"sorted"],            source, owner},
     %% App shell.
     {[~"patchwork", ~"progress"],                     source, owner},
     {[~"patchwork", ~"heartbeat"],                    source, owner},
     {[~"patchwork", ~"disconnect"],                   async,  owner}].

%% Async getters return an empty object/list; source streams are empty.
%% Side-effecting async methods acknowledge with true.
handle_rpc([~"patchwork", ~"profile", ~"avatar"], _Args, _Caller) ->
    {reply, {[]}};
handle_rpc([~"patchwork", ~"channels", ~"get"], _Args, _Caller) ->
    {reply, []};
handle_rpc([~"patchwork", ~"channels", ~"suggest"], _Args, _Caller) ->
    {reply, []};
handle_rpc([~"patchwork", ~"suggest", ~"profile"], _Args, _Caller) ->
    {reply, []};
handle_rpc([~"patchwork", ~"likes", ~"get"], _Args, _Caller) ->
    {reply, {[]}};
handle_rpc([~"patchwork", ~"liveBacklinks", ~"subscribe"], _Args, _Caller) ->
    {reply, true};
handle_rpc([~"patchwork", ~"liveBacklinks", ~"unsubscribe"], _Args, _Caller) ->
    {reply, true};
handle_rpc([~"patchwork", ~"disconnect"], _Args, _Caller) ->
    {reply, true};

%% Everything else registered here is a source stub: an empty stream.
handle_rpc([~"patchwork" | _], _Args, _Caller) ->
    {source, []}.
