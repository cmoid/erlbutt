%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Message-type predicates for SSB *application conventions* — the half
%% of social_msg that replication does not need (doc/persistence.md §5).
%%
%% `contact` and `about` stayed in apps/ssb because ebt cannot build a
%% replication set without them.  `root`/`branch` are different: only
%% something that reads a conversation cares, and a non-social
%% application on erlbutt should not inherit the notion of a reply.
%%
%% This could not move until the foundation stopped needing it.
%% utils:update_refs/1 called is_branch/1 to build the per-feed
%% references file; that file is gone, replaced by the ssb_links core
%% view, which extracts references by shape and knows no message type at
%% all — so nothing below apps/ssb_conv reads a `branch` any more.
-module(ssb_conv_msg).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([is_branch/1]).

%% {Root, [BranchId]} for a message that participates in a tangle, or
%% false.  A message we do not hold (an ssb_feed:fetch_msg/2 miss) has no
%% branches — tangle walks routinely reference ids that never replicated
%% to us.
is_branch(not_found) ->
    false;

is_branch(#message{content = Content}) when is_binary(Content) ->
    false;

is_branch(#message{content = {Content}}) ->
    Root = ?pgv(~"root", Content),
    Branch = ?pgv(~"branch", Content),
    build_branch(Root, Branch);

is_branch(_) ->
    false.

build_branch(undefined, _Branch) ->
    false;
build_branch(_Root, undefined) ->
    false;
build_branch(false, _Branch) ->
    false;
build_branch(_Root, false) ->
    false;
build_branch(Root, Branch) when is_list(Branch) ->
    {Root, Branch};
build_branch(Root, Branch) ->
    {Root, [Branch]}.

-ifdef(TEST).

%% A single branch id is normalised to a list, so callers have one shape.
is_branch_single_test() ->
    Msg = #message{content = {[{~"type",   ~"post"},
                               {~"root",   ~"%root.sha256"},
                               {~"branch", ~"%b1.sha256"}]}},
    ?assertEqual({~"%root.sha256", [~"%b1.sha256"]}, is_branch(Msg)).

is_branch_list_test() ->
    Msg = #message{content = {[{~"root",   ~"%root.sha256"},
                               {~"branch", [~"%b1.sha256", ~"%b2.sha256"]}]}},
    ?assertEqual({~"%root.sha256", [~"%b1.sha256", ~"%b2.sha256"]},
                 is_branch(Msg)).

%% Neither half alone makes a tangle message.
is_branch_incomplete_test() ->
    ?assertEqual(false, is_branch(#message{content = {[{~"root", ~"%r.sha256"}]}})),
    ?assertEqual(false, is_branch(#message{content = {[{~"branch", ~"%b.sha256"}]}})),
    ?assertEqual(false, is_branch(#message{content = {[{~"type", ~"post"}]}})).

%% Private content is opaque, and an id we do not hold is not an error.
is_branch_unreadable_test() ->
    ?assertEqual(false, is_branch(#message{content = ~"boxed.box"})),
    ?assertEqual(false, is_branch(not_found)),
    ?assertEqual(false, is_branch(undefined)).

%% Against real captured messages (moved here with the function from
%% social_msg): a single branch and a multi-branch tangle message.
is_single_branch_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "single-branch.full",
    {ok, FilBin} = file:read_file(F),
    {_Root, Branches} = is_branch(message:decode(FilBin, true)),
    ?assert(is_list(Branches) andalso length(Branches) == 1).

is_multi_branch_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "multi-branch.full",
    {ok, FilBin} = file:read_file(F),
    {_Root, Branches} = is_branch(message:decode(FilBin, true)),
    ?assert(is_list(Branches) andalso length(Branches) == 2).

-endif.
