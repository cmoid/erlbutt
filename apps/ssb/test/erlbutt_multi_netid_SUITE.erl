%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% CT suite for multiple-network-ID support.
%%
%% Two isolated peer nodes (A as server, B as client) exercise the
%% SHS retry loop, LRU cache recording, and cache-driven ID ordering.
%%
%% `stale_cache_triggers_fallback` deliberately injects a wrong cached
%% network ID so the retry loop must fall back to the correct one.
%% That first attempt fails and triggers a 1-second backoff — the test
%% takes ~1 s by design.
-module(erlbutt_multi_netid_SUITE).

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([fresh_connect_populates_cache/1,
         stale_cache_triggers_fallback/1,
         cache_hit_reuses_winner/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ssb/include/ssb.hrl").

-define(PORT_A, 18113).
-define(PORT_B, 18114).

%%% CT callbacks ------------------------------------------------------------

all() ->
    [fresh_connect_populates_cache,
     stale_cache_triggers_fallback,
     cache_hit_reuses_winner].

init_per_suite(Config) ->
    ssb_ct_helper:ensure_distributed(),
    DataDir = ?config(priv_dir, Config),
    DirA    = filename:join(DataDir, "netid_node_a"),
    DirB    = filename:join(DataDir, "netid_node_b"),
    ok = filelib:ensure_dir(DirA ++ "/"),
    ok = filelib:ensure_dir(DirB ++ "/"),
    PAs = ssb_ct_helper:pa_args(),
    {ok, PeerA, NodeA} = ssb_ct_helper:start_ssb_node(netid_node_a, DirA, ?PORT_A, PAs),
    {ok, PeerB, NodeB} = ssb_ct_helper:start_ssb_node(netid_node_b, DirB, ?PORT_B, PAs),
    APubKey  = rpc:call(NodeA, keys, pub_key, []),
    ACurvePk = rpc:call(NodeA, base64, decode, [APubKey]),
    [{node_a, NodeA}, {peer_a, PeerA},
     {node_b, NodeB}, {peer_b, PeerB},
     {a_curve_pk, ACurvePk}
     | Config].

end_per_suite(Config) ->
    peer:stop(?config(peer_a, Config)),
    peer:stop(?config(peer_b, Config)),
    Config.

%% Clear B's cache before each test so cases are independent.
init_per_testcase(_Case, Config) ->
    NodeB = ?config(node_b, Config),
    rpc:call(NodeB, ets, delete_all_objects, [network_id_cache]),
    Config.

end_per_testcase(_Case, Config) -> Config.

%%% Tests -------------------------------------------------------------------

%% No cached entry for A: connection succeeds and the cache on B is
%% populated with the winning network ID.
fresh_connect_populates_cache(Config) ->
    NodeA    = ?config(node_a, Config),
    NodeB    = ?config(node_b, Config),
    ACurvePk = ?config(a_curve_pk, Config),

    miss = rpc:call(NodeB, network_id_cache, lookup, [ACurvePk]),

    {ok, PeerPid} = rpc:call(NodeB, ssb_peer, start,
                              ["localhost", ?PORT_A, ACurvePk]),
    ?assert(rpc:call(NodeB, erlang, is_process_alive, [PeerPid])),

    WinNetId = rpc:call(NodeA, config, network_id, []),
    ?assertEqual({ok, WinNetId},
                 rpc:call(NodeB, network_id_cache, lookup, [ACurvePk])),

    rpc:call(NodeB, gen_server, stop, [PeerPid]).

%% Inject a wrong network ID into B's cache for A's pub key.  The retry
%% loop tries the cached (wrong) ID first — A rejects it and drops the
%% connection — then falls back to the correct one from the config list.
%% After success the cache is updated with the winning ID.
stale_cache_triggers_fallback(Config) ->
    NodeA    = ?config(node_a, Config),
    NodeB    = ?config(node_b, Config),
    ACurvePk = ?config(a_curve_pk, Config),

    WrongId = crypto:strong_rand_bytes(32),
    rpc:call(NodeB, network_id_cache, record, [ACurvePk, WrongId]),
    timer:sleep(20),
    {ok, WrongId} = rpc:call(NodeB, network_id_cache, lookup, [ACurvePk]),

    %% The first attempt (WrongId) fails; the loop backs off 1 s then
    %% retries with the correct network ID from the config list.
    {ok, PeerPid} = rpc:call(NodeB, ssb_peer, start,
                              ["localhost", ?PORT_A, ACurvePk]),
    ?assert(rpc:call(NodeB, erlang, is_process_alive, [PeerPid])),

    WinNetId = rpc:call(NodeA, config, network_id, []),
    ?assertEqual({ok, WinNetId},
                 rpc:call(NodeB, network_id_cache, lookup, [ACurvePk])),

    rpc:call(NodeB, gen_server, stop, [PeerPid]).

%% Seed the cache on B with the correct winning ID before connecting.
%% The retry loop picks it up via ordered_net_ids and connects first try,
%% with no fallback needed.
cache_hit_reuses_winner(Config) ->
    NodeA    = ?config(node_a, Config),
    NodeB    = ?config(node_b, Config),
    ACurvePk = ?config(a_curve_pk, Config),

    WinNetId = rpc:call(NodeA, config, network_id, []),
    rpc:call(NodeB, network_id_cache, record, [ACurvePk, WinNetId]),
    timer:sleep(20),

    {ok, PeerPid} = rpc:call(NodeB, ssb_peer, start,
                              ["localhost", ?PORT_A, ACurvePk]),
    ?assert(rpc:call(NodeB, erlang, is_process_alive, [PeerPid])),

    ?assertEqual({ok, WinNetId},
                 rpc:call(NodeB, network_id_cache, lookup, [ACurvePk])),

    rpc:call(NodeB, gen_server, stop, [PeerPid]).

