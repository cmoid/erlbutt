%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(friends).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("ssb/include/ssb.hrl").
-endif.

-export([direct_follows/1,
         follows/2,
         update/3]).

%% Placeholder — incremental social graph update on contact message arrival.
%% Full implementation (ETS/DETS backed graph) to follow.
update(_Author, _Contact, _Following) ->
    ok.

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
    ssb_feed:fold_contacts(FeedPid, Fun, []).

follows(FeedPid, HopCount) ->
    Self = ssb_feed:whoami(FeedPid),
    Visited0 = sets:from_list([Self]),
    {AllFollows, _} = follows2(FeedPid, HopCount, Visited0),
    lists:usort(AllFollows).

follows2(_FeedPid, 0, Visited) ->
    {[], Visited};

follows2(FeedPid, HopCount, Visited0) ->
    DirectFollow = direct_follows(FeedPid),
    NewDirect = [Id || Id <- DirectFollow, not sets:is_element(Id, Visited0)],
    {Deeper, Visited1} =
        lists:foldl(
          fun(Id, {Acc, Vis}) ->
                  case sets:is_element(Id, Vis) of
                      true ->
                          {Acc, Vis};
                      false ->
                          Vis2 = sets:add_element(Id, Vis),
                          Pid = utils:find_or_create_feed_pid(Id),
                          case Pid of
                              bad -> {Acc, Vis2};
                              OkPid ->
                                  {Ids, Vis3} = follows2(OkPid, HopCount - 1, Vis2),
                                  {lists:append(Ids, Acc), Vis3}
                          end
                  end
          end, {[], Visited0}, NewDirect),
    {lists:append(NewDirect, Deeper), Visited1}.

-ifdef(TEST).

friends_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [fun direct_follows_empty_test/1,
      fun direct_follows_follow_test/1,
      fun direct_follows_unfollow_test/1,
      fun follows_zero_hops_test/1,
      fun follows_one_hop_test/1,
      fun follows_two_hop_test/1,
      fun follows_no_cycle_test/1]}.

setup() ->
    Started = lists:filtermap(
        fun({Name, StartFun}) ->
            case whereis(Name) of
                undefined ->
                    {ok, Pid} = StartFun(),
                    {true, Pid};
                _ ->
                    false
            end
        end,
        [{config,       fun() -> config:start_link("test/ssb.cfg") end},
         {keys,         fun() -> keys:start_link() end},
         {mess_auth,    fun() -> mess_auth:start_link() end},
         {blobs,        fun() -> blobs:start_link() end},
         {ssb_feed_sup, fun() -> ssb_feed_sup:start_link() end}]),
    Started.

teardown(Pids) ->
    lists:foreach(fun(Pid) -> gen_server:stop(Pid) end, Pids).

%% Create a fresh feed backed by a generated key pair.
%% Returns {FeedPid, FeedId, PrivKey}.
make_peer() ->
    #{public := Pub, secret := Priv} = enacl:sign_keypair(),
    Id = <<"@", (base64:encode(Pub))/binary, ".ed25519">>,
    PrivB64 = base64:encode(Priv),
    Pid = utils:find_or_create_feed_pid(Id),
    {Pid, Id, PrivB64}.

%% Store a contact message in FeedPid, signed by {AuthId, AuthPriv}.
store_contact(FeedPid, AuthId, AuthPriv, Prev, Seq, ContactId, Following) ->
    Content = {[{~"type", ~"contact"}, {~"contact", ContactId}, {~"following", Following}]},
    Msg = message:new_msg(Prev, Seq, Content, {AuthId, AuthPriv}),
    ssb_feed:store_msg(FeedPid, Msg).

direct_follows_empty_test(_) ->
    fun() ->
        {Pid, _Id, _Priv} = make_peer(),
        ?assertEqual([], friends:direct_follows(Pid))
    end.

direct_follows_follow_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([Id2], friends:direct_follows(Pid))
    end.

direct_follows_unfollow_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        #message{id = Msg1Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_contact(Pid, Id, Priv, Msg1Id, 2, Id2, false),
        ?assertEqual([], friends:direct_follows(Pid))
    end.

follows_zero_hops_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([], friends:follows(Pid, 0))
    end.

follows_one_hop_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_AlicePid, AliceId, _AlicePriv} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, AliceId, true),
        Result = friends:follows(Pid, 1),
        ?assert(lists:member(AliceId, Result))
    end.

follows_two_hop_test(_) ->
    fun() ->
        {OwnerPid, OwnerId, OwnerPriv} = make_peer(),
        {AlicePid, AliceId, AlicePriv} = make_peer(),
        {_BobPid, BobId, _BobPriv} = make_peer(),
        ok = store_contact(OwnerPid, OwnerId, OwnerPriv, null, 1, AliceId, true),
        ok = store_contact(AlicePid, AliceId, AlicePriv, null, 1, BobId, true),
        Result = friends:follows(OwnerPid, 2),
        ?assert(lists:member(AliceId, Result)),
        ?assert(lists:member(BobId, Result))
    end.

%% Mutual follows must not cause an infinite loop, and the start node
%% must not appear in the result (it is always in the initial visited set).
follows_no_cycle_test(_) ->
    fun() ->
        {OwnerPid, OwnerId, OwnerPriv} = make_peer(),
        {AlicePid, AliceId, AlicePriv} = make_peer(),
        ok = store_contact(OwnerPid, OwnerId, OwnerPriv, null, 1, AliceId, true),
        ok = store_contact(AlicePid, AliceId, AlicePriv, null, 1, OwnerId, true),
        Result = friends:follows(OwnerPid, 5),
        ?assert(lists:member(AliceId, Result)),
        ?assertNot(lists:member(OwnerId, Result))
    end.

-endif.
