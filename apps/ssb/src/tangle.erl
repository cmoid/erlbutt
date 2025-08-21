%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(tangle).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("ssb/include/ssb.hrl").

-export([get_tangle/1,
         parents/2,
         ancestors/2,
         children/2,
         descendants/2,
         get_msgs/1
         ]).

get_tangle(TangleId) ->
    %% retrieve tangle root author
    Auth = mess_auth:get(TangleId),
    FeedPid = utils:find_or_create_feed_pid(Auth),
    Targets = ssb_feed:references(FeedPid, TangleId, TangleId),
    Fun = fun([M, A]) ->
                  find_paths(M, A, TangleId)
          end,
    {TangleId, Auth, lists:map(Fun, Targets)}.

get_msgs({TangleId, Auth, Nodes}) ->
    [get_msg(TangleId, Auth) | get_msgs1(Nodes, [])].

get_msgs1(Nodes, Msgs) ->
    Fun = fun({Id, Auth, Rest}) ->
                  get_msgs1(Rest, [get_msg(Id, Auth) | Msgs]);
             ({Id, Auth}) ->
                  lists:reverse([get_msg(Id, Auth) | Msgs])
          end,
    TmpRes = lists:flatten(lists:map(Fun, Nodes)),
    ResTerm = lists:foldl(fun(E, Acc) when is_list(Acc) ->
                        case lists:member(E, Acc) of
                            true ->
                                Acc;
                            _Else ->
                                [E | Acc]
                        end;
                       (_, Acc) ->
                            Acc
                    end, [], TmpRes),
    case is_list(ResTerm) of
        true ->
            lists:reverse(ResTerm);
        false ->
            []
    end.

get_msg(Id, Auth) ->
    Feed = utils:find_or_create_feed_pid(Auth),
    Msg = ssb_feed:fetch_msg(Feed, Id),
    {Content} = Msg#message.content,
    ?pgv(~"text", Content).

children(MsgId, TangleId) ->
    %% retrieve tangle root author
    Auth = mess_auth:get(MsgId),
    FeedPid = utils:find_or_create_feed_pid(Auth),
    {MsgId, ssb_feed:references(FeedPid, MsgId, TangleId)}.

descendants(MsgId, TangleId) ->
    %% retrieve tangle root author
    Auth = mess_auth:get(MsgId),
    FeedPid = utils:find_or_create_feed_pid(Auth),
    Targets = ssb_feed:references(FeedPid, MsgId, TangleId),
    Fun = fun([M, A]) ->
                  find_paths(M, A, TangleId)
          end,
    {MsgId, lists:map(Fun, Targets)}.

parents(MsgId, TangleId) ->
    %% retrieve message author
    Auth = mess_auth:get(MsgId),
    FeedPid = utils:find_or_create_feed_pid(Auth),
    Msg = ssb_feed:fetch_msg(FeedPid, MsgId),
    Branches = social_msg:is_branch(Msg),
    case Branches of
        false ->
            none;
        {TangleId, BranchList} ->
            {MsgId, lists:map(fun(P) -> [P, mess_auth:get(P)] end,
                      BranchList)};
        _Else ->
            none
    end.

ancestors(MsgId, TangleId) ->
    %% retrieve message author
    Auth = mess_auth:get(MsgId),
    FeedPid = utils:find_or_create_feed_pid(Auth),
    Msg = ssb_feed:fetch_msg(FeedPid, MsgId),
    Branches = social_msg:is_branch(Msg),
    case Branches of
        false ->
            none;
        {TangleId, BranchList} ->
            {MsgId, lists:map(fun(P) -> find_par_paths(P, mess_auth:get(P), TangleId) end,
                      BranchList)};
        _Else ->
            none
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

find_paths(MsgId, AuthId, RootId) ->
    Pid = utils:find_or_create_feed_pid(AuthId),
    Targets = ssb_feed:references(Pid, MsgId, RootId),
    Fun = fun([M, A]) ->
                  find_paths(M, A, RootId)
          end,
    case Targets of
        [] ->
            {MsgId, AuthId};
        done ->
            {MsgId, AuthId};
        _Else ->
            {MsgId, AuthId, lists:map(Fun, Targets)}
    end.

find_par_paths(MsgId, AuthId, RootId) ->
    Pid = utils:find_or_create_feed_pid(AuthId),
    Msg = ssb_feed:fetch_msg(Pid, MsgId),
    Branches = social_msg:is_branch(Msg),
    case Branches of
        false ->
            {MsgId, AuthId};
        {RootId, BranchList} ->
            {MsgId, AuthId,
             lists:map(fun(P) ->
                               find_par_paths(P, mess_auth:get(P),
                                              RootId) end,
                       BranchList)};
        _Else ->
            {MsgId, AuthId}
    end.

-ifdef(TEST).

basic_test() ->
    {Auth, Priv, Feed} = init(),
    #message{id = Id} = make_msg_one(Auth, Priv, Feed),
    #message{content = {Content}} = ssb_feed:fetch_msg(Feed, Id),
    ?assert(~"bar" == ?pgv(~"foo", Content)).

tangle1_test() ->
    {Auth, Priv, Feed} = init(),
    #message{id = Id} = make_msg_one(Auth, Priv, Feed),
    #message{id = Id2} = make_msg(2, Id, Id, Id, Auth, Priv, Feed),


    ?assert({Id, Auth, [{Id2, Auth}]} == tangle:get_tangle(Id)).

tangle2_test() ->
    {Auth, Priv, Feed} = init(),
    #message{id = Id} = make_msg_one(Auth, Priv, Feed),
    #message{id = Id2} = make_msg(2, Id, Id, Id, Auth, Priv, Feed),
    #message{id = Id3} = make_msg(3, Id2, Id, Id2, Auth, Priv, Feed),

    ?assert({Id, Auth, [{Id2, Auth,
                   [{Id3, Auth}]}]} == tangle:get_tangle(Id)).

tangle3_test() ->
    {Auth, Priv, Feed} = init(),
    #message{id = Id} = make_msg_one(Auth, Priv, Feed),
    #message{id = Id2} = make_msg(2, Id, Id, Id, Auth, Priv, Feed),
    #message{id = Id3} = make_msg(3, Id2, Id, Id2, Auth, Priv, Feed),

    ?assert({Id, Auth, [{Id2, Auth,
                   [{Id3, Auth}]}]} == tangle:get_tangle(Id)),

    %% Now create another feed
    {Auth2, Priv2, Feed2} = create_id(),
    #message{id = Id4} = make_msg(4, Id2, Id, Id2, Auth2, Priv2, Feed2),


    ?assert({Id, Auth, [{Id2, Auth,
                   [{Id4, Auth2},
                    {Id3, Auth}]}]} == tangle:get_tangle(Id)).

tangle4_test() ->
    {Auth, Priv, Feed} = init(),
    #message{id = Id} = make_msg_one(Auth, Priv, Feed),
    #message{id = Id2} = make_msg(2, Id, Id, Id, Auth, Priv, Feed),
    #message{id = Id3} = make_msg(3, Id2, Id, Id2, Auth, Priv, Feed),

    ?assert({Id, Auth, [{Id2, Auth,
                   [{Id3, Auth}]}]} == tangle:get_tangle(Id)),

    %% Now create another feed
    {Auth2, Priv2, Feed2} = create_id(),
    #message{id = Id4} = make_msg(4, Id2, Id, Id2, Auth2, Priv2, Feed2),
    #message{id = Id5} = make_msg(5, Id4, Id, [Id4, Id3], Auth2, Priv2, Feed2),


    ?assert({Id, Auth, [{Id2, Auth,
                   [{Id4, Auth2, [{Id5, Auth2}]},
                    {Id3, Auth, [{Id5, Auth2}]}]}]} == tangle:get_tangle(Id)).

init() ->
    config:start_link("test/ssb.cfg"),
    keys:start_link(),
    mess_auth:start_link(),
    create_id().

create_id() ->
    {Pub, Priv} = utils:create_key_pair(),
    Auth = utils:display_pub(Pub),
    {ok, Feed} = ssb_feed:start_link(Auth),
    {Auth, Priv, Feed}.

make_msg_one(Auth, Priv, Feed) ->
    Msg = message:new_msg(nil, 1, {[{~"foo", ~"bar"}]}, {Auth, Priv}),
    ssb_feed:store_msg(Feed, Msg),
    Msg.

make_msg(N, Prev, Root, BranchList, Auth, Priv, Feed) ->
    Msg = message:new_msg(Prev, N, {[{~"type", ~"post"},
                                    {~"test", ~"bar"},
                                    {~"root", Root},
                                    {~"branch", BranchList}]}, {Auth, Priv}),

    ssb_feed:store_msg(Feed, Msg),
    Msg.

-endif.
