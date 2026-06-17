%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Room membership via the (modified) pub-invite mechanism.
%%
%% node_a is a RESTRICTED room.  node_b redeems a room invite using the same
%% pub-invite flow; on the room side invite.use grants membership (room_store)
%% instead of posting a follow.  Afterwards room.metadata reports node_b as a
%% member.
-module(erlbutt_room_invite_SUITE).

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([room_invite_membership_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ssb/include/ssb.hrl").

-define(PORT_A, 18130).
-define(PORT_B, 18131).

all() -> [room_invite_membership_test].

init_per_suite(Config) ->
    ssb_ct_helper:ensure_distributed(),
    DataDir = ?config(priv_dir, Config),
    DirA = filename:join(DataDir, "a"),
    DirB = filename:join(DataDir, "b"),
    ok = filelib:ensure_dir(DirA ++ "/"),
    ok = filelib:ensure_dir(DirB ++ "/"),
    PAs = ssb_ct_helper:pa_args(),
    {ok, PeerA, NodeA} =
        ssb_ct_helper:start_ssb_node(rinv_a, DirA, ?PORT_A, PAs,
                                     [{room, true},
                                      {room_name, ~"restricted room"},
                                      {room_privacy, restricted}]),
    {ok, PeerB, NodeB} = ssb_ct_helper:start_ssb_node(rinv_b, DirB, ?PORT_B, PAs),
    [{node_a, NodeA}, {peer_a, PeerA},
     {node_b, NodeB}, {peer_b, PeerB} | Config].

end_per_suite(Config) ->
    peer:stop(?config(peer_a, Config)),
    peer:stop(?config(peer_b, Config)),
    Config.

init_per_testcase(_Case, Config) -> Config.
end_per_testcase(_Case, Config)  -> Config.

%%% Tests ---------------------------------------------------------------

room_invite_membership_test(Config) ->
    NodeA = ?config(node_a, Config),   %% restricted room
    NodeB = ?config(node_b, Config),   %% prospective member
    BId   = rpc:call(NodeB, keys, pub_key_disp, []),

    %% Before redeeming, node_b is not a member and metadata says so.
    ?assertNot(rpc:call(NodeA, room_store, is_member, [BId])),
    ?assertEqual(false, membership_via_metadata(NodeA, NodeB)),

    %% Room creates an invite; node_b redeems it (same pub-invite flow).
    {ok, Code} = rpc:call(NodeA, invite, create, ["localhost", ?PORT_A]),
    {ok, _}    = rpc:call(NodeB, invite, redeem, [Code]),
    timer:sleep(300),

    %% invite.use granted membership instead of a follow.
    ?assert(rpc:call(NodeA, room_store, is_member, [BId])),
    ?assertEqual(true, membership_via_metadata(NodeA, NodeB)).

%%% Helpers -------------------------------------------------------------

%% node_b connects to the room with its real identity and reads its membership
%% status from room.metadata.
membership_via_metadata(NodeA, NodeB) ->
    ACurvePk = rpc:call(NodeA, base64, decode,
                        [rpc:call(NodeA, keys, pub_key, [])]),
    {ok, PeerPid} = rpc:call(NodeB, ssb_peer, start,
                             ["localhost", ?PORT_A, ACurvePk]),
    timer:sleep(300),
    {ok, Body} = rpc:call(NodeB, ssb_peer, rpc_call,
                          [PeerPid, [?room, ?metadata], ~"async"]),
    {Props} = rpc:call(NodeB, utils, nat_decode, [Body]),
    rpc:call(NodeB, gen_server, stop, [PeerPid]),
    timer:sleep(100),
    proplists:get_value(~"membership", Props).
