%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Two-node peer replication tests.
%%
%% Two isolated BEAM nodes (node_a, node_b) are started via the OTP
%% `peer` module, each running its own ssb application with a separate
%% key pair, data directory, and TCP port.  node_b connects to node_a
%% over loopback and exercises the full SHS → boxstream → EBT path,
%% allowing true cross-peer replication to be verified.
-module(erlbutt_two_node_SUITE).

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([two_node_handshake_test/1,
         two_node_ebt_replication_test/1,
         two_node_blob_wants_test/1,
         two_node_blob_fetch_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ssb/include/ssb.hrl").

-define(PORT_A, 18108).
-define(PORT_B, 18109).
-define(EBT_SETTLE_MS, 500).

%%% CT callbacks --------------------------------------------------------

all() ->
    [two_node_handshake_test,
     two_node_ebt_replication_test,
     two_node_blob_wants_test,
     two_node_blob_fetch_test].

init_per_suite(Config) ->
    %% peer:start/1 requires the calling node to be distributed.
    case node() of
        nonode@nohost -> net_kernel:start([erlbutt_ct, shortnames]);
        _             -> ok
    end,

    DataDir = ?config(priv_dir, Config),
    DirA    = filename:join(DataDir, "node_a"),
    DirB    = filename:join(DataDir, "node_b"),
    ok = filelib:ensure_dir(DirA ++ "/"),
    ok = filelib:ensure_dir(DirB ++ "/"),

    PAs = pa_args(),
    {ok, PeerA, NodeA} = start_ssb_node(node_a, DirA, ?PORT_A, PAs),
    {ok, PeerB, NodeB} = start_ssb_node(node_b, DirB, ?PORT_B, PAs),

    [{node_a, NodeA}, {peer_a, PeerA},
     {node_b, NodeB}, {peer_b, PeerB},
     {dir_a,  DirA},  {dir_b, DirB}
     | Config].

end_per_suite(Config) ->
    peer:stop(?config(peer_a, Config)),
    peer:stop(?config(peer_b, Config)),
    Config.

init_per_testcase(_Case, Config) -> Config.
end_per_testcase(_Case, Config)  -> Config.

%%% Tests ---------------------------------------------------------------

%% node_b initiates SHS with node_a.  The peer process on node_b
%% stays alive only if the full handshake succeeded.
two_node_handshake_test(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),
    APubKey  = rpc:call(NodeA, keys, pub_key, []),
    ACurvePk = rpc:call(NodeA, base64, decode, [APubKey]),
    {ok, PeerPid} = rpc:call(NodeB, ssb_peer, start,
                              ["localhost", ?PORT_A, ACurvePk]),
    ?assert(rpc:call(NodeB, erlang, is_process_alive, [PeerPid])),
    rpc:call(NodeB, gen_server, stop, [PeerPid]).

%% Post a message on node_a, have node_b connect and run EBT,
%% then verify the message appears in node_b's storage for node_a's feed.
two_node_ebt_replication_test(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),

    %% Post a message on A
    AId     = rpc:call(NodeA, keys, pub_key_disp, []),
    AFeedPid = rpc:call(NodeA, utils, find_or_create_feed_pid, [AId]),
    ok = rpc:call(NodeA, ssb_feed, post_content,
                  [AFeedPid, ~"hello from node_a"]),
    #message{sequence = Seq} =
        rpc:call(NodeA, ssb_feed, fetch_last_msg, [AFeedPid]),
    ?assert(Seq >= 1),

    %% B connects to A — EBT runs automatically after the handshake
    APubKey  = rpc:call(NodeA, keys, pub_key, []),
    ACurvePk = rpc:call(NodeA, base64, decode, [APubKey]),
    {ok, PeerPid} = rpc:call(NodeB, ssb_peer, start,
                              ["localhost", ?PORT_A, ACurvePk]),
    timer:sleep(300),
    rpc:call(NodeB, ssb_peer, request_ebt, [PeerPid]),
    timer:sleep(300),
    ?assert(rpc:call(NodeB, erlang, is_process_alive, [PeerPid])),

    %% Give EBT time to stream the message to B
    timer:sleep(?EBT_SETTLE_MS),

    %% Verify B has A's feed and the message is stored there
    BFeedPid = rpc:call(NodeB, utils, find_or_create_feed_pid, [AId]),
    #message{sequence = RepSeq} =
        rpc:call(NodeB, ssb_feed, fetch_last_msg, [BFeedPid]),
    ?assert(RepSeq == Seq),

    rpc:call(NodeB, gen_server, stop, [PeerPid]).

%% Store a blob on node_a, connect from node_b, send a want, and verify
%% that node_b receives the have response with the correct blob size.
two_node_blob_wants_test(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),

    %% Store a blob on A
    BlobData = ~"erlbutt two-node blob wants test payload",
    BlobId   = rpc:call(NodeA, blobs, store, [BlobData]),
    ExpectedSize = byte_size(BlobData),
    ?assert(rpc:call(NodeA, blobs, has, [BlobId]) =:= true),

    %% B connects to A (full SHS + EBT)
    APubKey  = rpc:call(NodeA, keys, pub_key, []),
    ACurvePk = rpc:call(NodeA, base64, decode, [APubKey]),
    {ok, PeerPid} = rpc:call(NodeB, ssb_peer, start,
                              ["localhost", ?PORT_A, ACurvePk]),

    %% B sends want to A — returns {ok, [{BlobId, Size}]} directly
    {ok, Haves} = rpc:call(NodeB, ssb_peer, request_blob_wants, [PeerPid, [BlobId]]),
    ?assert(lists:member({BlobId, ExpectedSize}, Haves)),

    rpc:call(NodeB, gen_server, stop, [PeerPid]).

%% Connect from node_b to node_a, request a blob via blobs.get, and verify
%% that node_b receives the complete blob data.
two_node_blob_fetch_test(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),

    %% Store a blob on A
    BlobData = ~"erlbutt two-node blob fetch test payload",
    BlobId   = rpc:call(NodeA, blobs, store, [BlobData]),
    ?assert(rpc:call(NodeA, blobs, has, [BlobId]) =:= true),

    %% B connects to A (full SHS + EBT)
    APubKey  = rpc:call(NodeA, keys, pub_key, []),
    ACurvePk = rpc:call(NodeA, base64, decode, [APubKey]),
    {ok, PeerPid} = rpc:call(NodeB, ssb_peer, start,
                              ["localhost", ?PORT_A, ACurvePk]),

    %% B requests the blob from A — returns {ok, Data} directly
    {ok, BlobData} = rpc:call(NodeB, ssb_peer, fetch_blob, [PeerPid, BlobId]),

    rpc:call(NodeB, gen_server, stop, [PeerPid]).

%%% Helpers -------------------------------------------------------------

%% Start a peer BEAM node, load code paths, configure and start ssb.
start_ssb_node(Name, DataDir, Port, PAs) ->
    {ok, Peer, Node} = peer:start(#{
        name => Name,
        args => PAs
    }),
    %% Load enacl NIF — must be done before starting ssb
    rpc:call(Node, code, add_paths,
             [[filename:join([build_dir(), "lib", "enacl", "priv"])]]),
    rpc:call(Node, application, set_env, [ssb, ssb_home, DataDir]),
    rpc:call(Node, application, set_env, [ssb, port, Port]),
    {ok, _} = rpc:call(Node, application, ensure_all_started, [ssb]),
    {ok, Peer, Node}.

%% Build -pa flag strings for all ebin and test dirs in the test build.
%% Test dirs are needed so CT suite modules (like this one) are available
%% on remote nodes spawned via peer.
pa_args() ->
    LibDir = filename:join(build_dir(), "lib"),
    {ok, Libs} = file:list_dir(LibDir),
    lists:flatmap(
        fun(Lib) ->
            EbinDir = filename:join([LibDir, Lib, "ebin"]),
            TestDir = filename:join([LibDir, Lib, "test"]),
            Ebin = case filelib:is_dir(EbinDir) of
                true  -> ["-pa", EbinDir];
                false -> []
            end,
            Test = case filelib:is_dir(TestDir) of
                true  -> ["-pa", TestDir];
                false -> []
            end,
            Ebin ++ Test
        end, Libs).

build_dir() ->
    %% code:lib_dir(ssb) = _build/test/lib/ssb; two levels up = _build/test
    filename:join(code:lib_dir(ssb), "../..").
