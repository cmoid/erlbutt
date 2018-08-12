%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(friends).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([direct_follows/1,
         follows/2]).

direct_follows(FeedPid) ->
    Fun = fun(Data, Acc) ->
                  Msg = message:decode(Data, false),
                  Follow = social_msg:is_follow(Msg),
                  case Follow of
                      nope -> Acc;
                      %% this next clause handles legacy garbage from planetary feeds
                      {true, true} -> Acc;
                      {Id, true} -> [Id | Acc];
                      {Id, false} -> lists:delete(Id, Acc)
                  end
          end,
    ssb_feed:foldl(FeedPid, Fun, []).

follows(FeedPid, HopCount) ->
    put(visited, [ssb_feed:whoami(FeedPid)]),
    FinalFeeds = follows2(FeedPid, HopCount),
    put(visited, []),
    lists:foldl(fun(E, Acc) when is_list(Acc) ->
                        Cf = lists:member(E, Acc),
                        if Cf ->
                                Acc;
                           true ->
                                [E | Acc]
                        end
                end, [], FinalFeeds).

follows2(_FeedPid, 0) ->
    [];

follows2(FeedPid, HopCount) ->
    DirectFollow = direct_follows(FeedPid),
    NewDirectFollow = lists:filter(fun(E) ->
                                           not lists:member(E, get(visited))
                                   end, DirectFollow),

    lists:append(lists:foldl(recurse_follow(HopCount),
                             [], NewDirectFollow),
                 NewDirectFollow).

recurse_follow(HopCount) ->
    fun(Id, Acc) ->
            AlreadySeen = lists:member(Id, get(visited)),
            if AlreadySeen ->
                    Acc;
               true ->
                    Pid = utils:find_or_create_feed_pid(Id),
                    case Pid of
                        bad -> Acc;
                        OkPid ->
                            Visited = get(visited),
                            put(visited, [Id | Visited]),
                            DF = follows2(OkPid, HopCount - 1),
                            lists:flatten(lists:append(lists:filter(
                                                         fun(Nid) ->
                                                                 not lists:member(Nid, Acc)
                                                         end, DF),
                                                       Acc))
                    end
            end
    end.
