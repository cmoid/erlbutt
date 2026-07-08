%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Stateless db-surface plugins for UI clients: methods that answer
%% straight from the per-feed store with no derived index.  Registered
%% by silkpurse_app at startup.
-module(silkpurse_db).

-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

-export([manifest/0, handle_rpc/3]).

manifest() ->
    [{[~"getLatest"], async, owner}].

%% getLatest(feedId) -> {id, sequence, ts} of the feed's newest message
%% (the shape ssb-db clients expect).
handle_rpc([~"getLatest"], [FeedId], _Caller) when is_binary(FeedId) ->
    try utils:find_or_create_feed_pid(FeedId) of
        Pid ->
            case ssb_feed:fetch_last_msg(Pid) of
                #message{id = Id, sequence = Seq, timestamp = Ts} ->
                    {reply, {[{~"id", Id},
                              {~"sequence", Seq},
                              {~"ts", Ts}]}};
                _ ->
                    {error, ~"no messages"}
            end
    catch _:_ ->
            {error, ~"bad feed id"}
    end;
handle_rpc([~"getLatest"], _Args, _Caller) ->
    {error, ~"getLatest takes a feed id"}.
