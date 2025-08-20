%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(social_msg).
-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-import(message, [decode/2]).

%% API
-export([
         is_follow/1,
         is_about/1,
         is_reply/1,
         is_branch/1]).

is_follow(#message{content = Val}) when is_binary(Val) ->
    nope;

is_follow(#message{content = {ContentProps}}) ->
    Type = ?pgv(~"type", ContentProps),
    case Type of
        ~"contact" ->
            check_contact(?pgv(~"contact",ContentProps),
                          ?pgv(~"following", ContentProps));
        _Else ->
            nope
    end.

is_about(#message{content = Content}) when is_binary(Content) ->
    false;

is_about(#message{content = {ContentProps}}) ->
    Type = ?pgv(~"type", ContentProps),
    case Type of
        undefined ->
            false;
        Type ->
            Type == ~"about"
    end.

is_branch(#message{content = Content}) when is_binary(Content) ->
    false;

is_branch(#message{content = {Content}}) ->
    Root = ?pgv(~"root", Content),
    Branch = ?pgv(~"branch", Content),
    build_branch(Root, Branch).

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

is_reply(#message{content = Content}) when is_binary(Content) ->
    false;

is_reply(#message{content = {Content}}) ->
    build_reply(?pgv(~"reply", Content)).

build_reply(undefined) ->
    false;

build_reply(Reply) ->
    Reply.

check_contact(undefined, _Following) ->
    nope;
check_contact(<<>>, _Following) ->
    nope;
check_contact(Contact, true) ->
    {Contact, true};
check_contact(Contact, false) ->
    {Contact, false};
check_contact(_Contact, _Following) ->
    nope.


-ifdef(TEST).


is_about_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "about.full",
    {ok, FilBin} = file:read_file(F),

    ?assert(social_msg:is_about(decode(FilBin, true))).

is_reply_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "reply.full",
    {ok, FilBin} = file:read_file(F),

    ?assert(social_msg:is_reply(decode(FilBin, true)) /= false).

is_multi_reply_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "mult-reply.full",
    {ok, FilBin} = file:read_file(F),
    Msg = decode(FilBin, true),
    {ListPairs} = social_msg:is_reply(Msg),
    ?assert(length(ListPairs) == 2).

is_not_reply_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "not_reply.full",
    {ok, FilBin} = file:read_file(F),

    ?assert(not social_msg:is_reply(decode(FilBin, true))).

is_single_branch_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "single-branch.full",
    {ok, FilBin} = file:read_file(F),
    {_Root, Branches} = social_msg:is_branch(decode(FilBin, true)),
    ?assert(is_list(Branches) andalso length(Branches) == 1).

is_multi_branch_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "multi-branch.full",
    {ok, FilBin} = file:read_file(F),
    {_Root, Branches} = social_msg:is_branch(decode(FilBin, true)),
    ?assert(is_list(Branches) andalso length(Branches) == 2).

follow_true_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "follow_true.full",
    {ok, FilBin} = file:read_file(F),
    {_FeedId, Bool} = social_msg:is_follow(decode(FilBin, true)),

    ?assert(Bool).

follow_false_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "follow_false.full",
    {ok, FilBin} = file:read_file(F),
    {_FeedId, Bool} = social_msg:is_follow(decode(FilBin, true)),

    ?assert(not Bool).

-endif.
