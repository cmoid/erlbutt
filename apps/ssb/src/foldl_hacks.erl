%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(foldl_hacks).
-include("ssb.hrl").
-export([branch_root/1,
         reply_root/1]).

branch_root(FeedId) ->
    {ok, Pid} = ssb_feed:start_link(FeedId),

    Fun = fun(Msg, {N, B, R, RY, NT}) ->
                  RootReply = check_branch_root(Msg),
                  case RootReply of
                      nope -> {N + 1, B, R, RY, NT};
                      both -> {N, B+1, R, RY, NT};
                      root_only -> {N, B, R + 1, RY, NT};
                      branch_only -> {N, B, R, RY = 1, NT};
                      neither -> {N, B, R, RY, NT + 1}
                  end
          end,

    ssb_feed:foldl(Pid, Fun, {0, 0, 0, 0, 0}).

reply_root(FeedId) ->
    {ok, Pid} = ssb_feed:start_link(FeedId),

    Fun = fun(Msg, {N, B, R, RY, NT}) ->
                  RootReply = check_reply_root(Msg),
                  case RootReply of
                      nope -> {N + 1, B, R, RY, NT};
                      both -> {N, B+1, R, RY, NT};
                      root_only -> {N, B, R + 1, RY, NT};
                      reply_only -> {N, B, R, RY = 1, NT};
                      neither -> {N, B, R, RY, NT + 1}
                  end
          end,
    ssb_feed:foldl(Pid, Fun, {0, 0, 0, 0, 0}).

check_branch_root(Msg) ->
    {DecProps} = utils:nat_decode(Msg),
    {Value} = ?pgv(<<"value">>, DecProps),
    Val = ?pgv(<<"content">>, Value),
    if is_binary(Val) ->
            nope;
       true ->
            {Content} = Val,
            Type = ?pgv(<<"type">>, Content),
            case Type of
                <<"post">> ->
                    Root = ?pgv(<<"root">>, Content),
                    RootExists = Root /= undefined,
                    Branch = ?pgv(<<"branch">>, Content),
                    BranchExists = Branch /= undefined,
                    BothExist = (RootExists
                                 andalso
                                 BranchExists),
                    case BothExist of
                        true ->
                            both;
                        false ->
                            if RootExists ->
                                    root_only;
                               true ->
                                    if BranchExists ->
                                            branch_only;
                                       true ->
                                            neither
                                    end
                            end
                    end;
                _Else ->
                    nope
            end
    end.

check_reply_root(Msg) ->
    {DecProps} = utils:nat_decode(Msg),
    {Value} = ?pgv(<<"value">>, DecProps),
    Val = ?pgv(<<"content">>, Value),
    if is_binary(Val) ->
            nope;
       true ->
            {Content} = Val,
            Type = ?pgv(<<"type">>, Content),
            case Type of
                <<"post">> ->
                    Root = ?pgv(<<"root">>, Content),
                    RootExists = Root /= undefined,
                    Reply = ?pgv(<<"reply">>, Content),
                    ReplyExists = Reply /= undefined,
                    BothExist = (RootExists
                                 andalso
                                 ReplyExists),
                    case BothExist of
                        true ->
                            both;
                        false ->
                            if RootExists ->
                                    root_only;
                               true ->
                                    if ReplyExists ->
                                            reply_only;
                                       true ->
                                            neither
                                    end
                            end
                    end;
                _Else ->
                    nope
            end
    end.
