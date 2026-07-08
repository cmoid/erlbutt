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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([manifest/0, handle_rpc/3]).

manifest() ->
    [{[~"getLatest"], async, owner},
     {[~"friends", ~"get"], async, owner}].

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
    {error, ~"getLatest takes a feed id"};

%% friends.get(opts) -> the follow/block relationship in ssb-friends
%% legacy terms (true following, false blocking, null neither):
%%   {source, dest} -> a single value
%%   {source}       -> {dest: value} for all of source's edges
handle_rpc([~"friends", ~"get"], [{Opts}], _Caller) ->
    Source = ?pgv(~"source", Opts),
    Dest   = ?pgv(~"dest", Opts),
    case {Source, Dest} of
        {S, D} when is_binary(S), is_binary(D) ->
            {reply, friends:edge(S, D)};
        {S, undefined} when is_binary(S) ->
            {reply, {maps:to_list(friends:edges(S))}};
        _ ->
            {error, ~"friends.get needs a source"}
    end;
handle_rpc([~"friends", ~"get"], _Args, _Caller) ->
    {error, ~"friends.get takes an options object"}.

-ifdef(TEST).

friends_get_args_test() ->
    %% only shape handling here; graph semantics live in friends.erl
    ?assertMatch({error, _},
                 handle_rpc([~"friends", ~"get"], [~"notanobj"], #{})),
    ?assertMatch({error, _},
                 handle_rpc([~"friends", ~"get"], [{[]}], #{})).

-endif.
