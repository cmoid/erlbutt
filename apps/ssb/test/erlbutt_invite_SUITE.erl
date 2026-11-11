%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Two-node invite redemption tests.
%%
%% node_a acts as the pub (creates the invite code).
%% node_b acts as the new user (redeems the invite).
%% After redemption:
%%   - node_a should have posted a contact/follow message for node_b's feed.
%%   - node_b should have posted a follow message and a pub message.
-module(erlbutt_invite_SUITE).

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([invite_redeem_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ssb/include/ssb.hrl").

-define(PORT_A, 18110).
-define(PORT_B, 18111).

%%% CT callbacks --------------------------------------------------------

all() -> [invite_redeem_test].

init_per_suite(Config) ->
    ssb_ct_helper:ensure_distributed(),
    DataDir = ?config(priv_dir, Config),
    DirA = filename:join(DataDir, "node_a"),
    DirB = filename:join(DataDir, "node_b"),
    ok = filelib:ensure_dir(DirA ++ "/"),
    ok = filelib:ensure_dir(DirB ++ "/"),
    PAs = ssb_ct_helper:pa_args(),
    {ok, PeerA, NodeA} = ssb_ct_helper:start_ssb_node(node_inv_a, DirA, ?PORT_A, PAs),
    {ok, PeerB, NodeB} = ssb_ct_helper:start_ssb_node(node_inv_b, DirB, ?PORT_B, PAs),
    [{node_a, NodeA}, {peer_a, PeerA},
     {node_b, NodeB}, {peer_b, PeerB}
     | Config].

end_per_suite(Config) ->
    peer:stop(?config(peer_a, Config)),
    peer:stop(?config(peer_b, Config)),
    Config.

init_per_testcase(_Case, Config) -> Config.
end_per_testcase(_Case, Config)  -> Config.

%%% Tests ---------------------------------------------------------------

invite_redeem_test(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),

    %% Node A creates an invite code pointing to itself
    {ok, InviteCode} = rpc:call(NodeA, invite, create, ["localhost", ?PORT_A]),
    ?assert(is_binary(InviteCode)),

    %% Node B redeems the invite
    {ok, _Response} = rpc:call(NodeB, invite, redeem, [InviteCode]),

    %% Allow async follow/pub posting on node_b to complete
    timer:sleep(500),

    %% Node A should have posted a follow message for node_b's feed
    AId = rpc:call(NodeA, keys, pub_key_disp, []),
    AFeedPid = rpc:call(NodeA, utils, find_or_create_feed_pid, [AId]),
    #message{sequence = ASeq} =
        rpc:call(NodeA, ssb_feed, fetch_last_msg, [AFeedPid]),
    ?assert(ASeq >= 1),

    %% Node B should have posted at least 2 messages (follow + pub)
    BId = rpc:call(NodeB, keys, pub_key_disp, []),
    BFeedPid = rpc:call(NodeB, utils, find_or_create_feed_pid, [BId]),
    #message{sequence = BSeq} =
        rpc:call(NodeB, ssb_feed, fetch_last_msg, [BFeedPid]),
    ?assert(BSeq >= 2).

