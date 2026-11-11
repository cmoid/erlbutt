%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Two-node Plumtree broadcast tests.
%%
%% Two isolated BEAM nodes (pt_node_a, pt_node_b) are started via the OTP
%% `peer` module.  Each runs the plumtree application with plumtree_test_handler
%% so deliveries can be inspected via rpc:call without requiring a full SSB stack.
%% The nodes discover each other via Erlang distribution — no TCP transport needed.
-module(plumtree_SUITE).

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([broadcast_reaches_peer_test/1,
         bidirectional_broadcast_test/1,
         duplicate_triggers_prune_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SETTLE_MS, 300).

all() ->
    [broadcast_reaches_peer_test,
     bidirectional_broadcast_test,
     duplicate_triggers_prune_test].

init_per_suite(Config) ->
    ssb_ct_helper:ensure_distributed(),

    DataDir = ?config(priv_dir, Config),
    DirA    = filename:join(DataDir, "pt_node_a"),
    DirB    = filename:join(DataDir, "pt_node_b"),
    ok = filelib:ensure_dir(DirA ++ "/"),
    ok = filelib:ensure_dir(DirB ++ "/"),

    PAs = ssb_ct_helper:pa_args(),

    {ok, PeerA, NodeA} = start_plumtree_node(pt_node_a, DirA, 18208, PAs),
    {ok, PeerB, NodeB} = start_plumtree_node(pt_node_b, DirB, 18209, PAs),

    %% Wire the two nodes together as each other's eager peers.
    ok = rpc:call(NodeA, plumtree_broadcast, set_peers, [[NodeB]]),
    ok = rpc:call(NodeB, plumtree_broadcast, set_peers, [[NodeA]]),

    [{node_a, NodeA}, {peer_a, PeerA},
     {node_b, NodeB}, {peer_b, PeerB} | Config].

end_per_suite(Config) ->
    peer:stop(?config(peer_a, Config)),
    peer:stop(?config(peer_b, Config)),
    Config.

init_per_testcase(_Case, Config) ->
    %% Reset the delivery table on both nodes before each test.
    rpc:call(?config(node_a, Config), plumtree_test_handler, init_table, []),
    rpc:call(?config(node_b, Config), plumtree_test_handler, init_table, []),
    Config.

end_per_testcase(_Case, Config) ->
    Config.

%%% Tests ---------------------------------------------------------------

%% A broadcasts a message; B must receive and deliver it.
broadcast_reaches_peer_test(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),

    MsgId   = <<"pt-msg-1">>,
    Payload = <<"hello from node_a">>,
    rpc:call(NodeA, plumtree_broadcast, broadcast, [MsgId, Payload]),
    timer:sleep(?SETTLE_MS),

    ?assert(rpc:call(NodeB, plumtree_test_handler, has_msg, [MsgId])).

%% A broadcasts to B, then B broadcasts a different message to A.
bidirectional_broadcast_test(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),

    MsgA = <<"pt-msg-a">>,  PayloadA = <<"from a">>,
    MsgB = <<"pt-msg-b">>,  PayloadB = <<"from b">>,

    rpc:call(NodeA, plumtree_broadcast, broadcast, [MsgA, PayloadA]),
    rpc:call(NodeB, plumtree_broadcast, broadcast, [MsgB, PayloadB]),
    timer:sleep(?SETTLE_MS),

    ?assert(rpc:call(NodeB, plumtree_test_handler, has_msg, [MsgA])),
    ?assert(rpc:call(NodeA, plumtree_test_handler, has_msg, [MsgB])).

%% When both nodes broadcast the same message, each must deliver it exactly
%% once (the duplicate is silently discarded, not re-delivered).
duplicate_triggers_prune_test(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),

    MsgId   = <<"pt-msg-dup">>,
    Payload = <<"same payload">>,

    %% Both nodes originate the same message simultaneously.
    rpc:call(NodeA, plumtree_broadcast, broadcast, [MsgId, Payload]),
    rpc:call(NodeB, plumtree_broadcast, broadcast, [MsgId, Payload]),
    timer:sleep(?SETTLE_MS),

    %% Each node must have delivered the message (at least once — the ETS
    %% table uses `bag` so duplicates would show as multiple entries).
    ?assert(rpc:call(NodeA, plumtree_test_handler, has_msg, [MsgId])),
    ?assert(rpc:call(NodeB, plumtree_test_handler, has_msg, [MsgId])),

    %% Exactly one delivery per node — no duplicates passed to the handler.
    ?assertEqual(1, length(rpc:call(NodeA, plumtree_test_handler, deliveries, []))),
    ?assertEqual(1, length(rpc:call(NodeB, plumtree_test_handler, deliveries, []))).

%%% Helpers -------------------------------------------------------------

start_plumtree_node(Name, DataDir, Port, PAs) ->
    {ok, Peer, Node} = peer:start(#{name => Name, args => PAs}),
    rpc:call(Node, code, add_paths,
             [[filename:join([ssb_ct_helper:build_dir(), "lib", "enacl", "priv"])]]),
    rpc:call(Node, application, set_env, [ssb, ssb_home, DataDir]),
    rpc:call(Node, application, set_env, [ssb, port, Port]),
    %% Use the test handler so we don't need a full SSB stack for delivery.
    rpc:call(Node, application, set_env, [plumtree, handler, plumtree_test_handler]),
    {ok, _} = rpc:call(Node, application, ensure_all_started, [ssb]),
    {ok, _} = rpc:call(Node, application, ensure_all_started, [plumtree]),
    {ok, Peer, Node}.
