%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Static membership: reads initial peer list from app env.
%% Splits into eager (first half) and lazy (rest) for the first pass.
-module(plumtree_membership).

-export([peers/0, eager_peers/0, lazy_peers/0]).

%% Return all configured peer nodes.
peers() ->
    application:get_env(plumtree, initial_peers, []).

%% Eager peers: all peers (small deployments will be all-eager initially;
%% the broadcast algorithm prunes them to lazy as duplicates arrive).
eager_peers() ->
    peers().

lazy_peers() ->
    [].
