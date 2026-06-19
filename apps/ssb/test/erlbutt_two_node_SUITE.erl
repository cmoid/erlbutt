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
         two_node_ebt_no_duplicate_stream_test/1,
         two_node_ebt_multiple_feeds_test/1,
         two_node_ebt_post_connect_replication_test/1,
         two_node_blob_wants_test/1,
         two_node_multiple_blob_wants_test/1,
         two_node_blob_fetch_test/1,
         two_node_auto_blob_fetch_test/1]).

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
     two_node_ebt_no_duplicate_stream_test,
     two_node_ebt_multiple_feeds_test,
     two_node_ebt_post_connect_replication_test,
     two_node_blob_wants_test,
     two_node_multiple_blob_wants_test,
     two_node_blob_fetch_test,
     two_node_auto_blob_fetch_test].

init_per_suite(Config) ->
    %% peer:start/1 requires the calling node to be distributed.
    ssb_ct_helper:ensure_distributed(),

    DataDir = ?config(priv_dir, Config),
    DirA    = filename:join(DataDir, "node_a"),
    DirB    = filename:join(DataDir, "node_b"),
    ok = filelib:ensure_dir(DirA ++ "/"),
    ok = filelib:ensure_dir(DirB ++ "/"),

    PAs = ssb_ct_helper:pa_args(),
    {ok, PeerA, NodeA} = ssb_ct_helper:start_ssb_node(node_a, DirA, ?PORT_A, PAs),
    {ok, PeerB, NodeB} = ssb_ct_helper:start_ssb_node(node_b, DirB, ?PORT_B, PAs),

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
    ok = follow_feed(NodeB, AId),
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

%% Calling request_ebt/1 twice on the same peer must not open a second
%% EBT duplex stream.  After the first call ebt_active is true; the second
%% call is a no-op.  The connection must remain alive throughout.
two_node_ebt_no_duplicate_stream_test(Config) ->
    NodeB    = ?config(node_b, Config),
    NodeA    = ?config(node_a, Config),
    APubKey  = rpc:call(NodeA, keys, pub_key, []),
    ACurvePk = rpc:call(NodeA, base64, decode, [APubKey]),
    {ok, PeerPid} = rpc:call(NodeB, ssb_peer, start,
                              ["localhost", ?PORT_A, ACurvePk]),

    rpc:call(NodeB, ssb_peer, request_ebt, [PeerPid]),
    timer:sleep(100),

    %% ebt_active should now be set.
    State1 = rpc:call(NodeB, sys, get_state, [PeerPid]),
    ?assert(State1#sbox_state.ebt_active),

    %% Second request_ebt is a no-op — connection must still be alive.
    rpc:call(NodeB, ssb_peer, request_ebt, [PeerPid]),
    timer:sleep(100),
    ?assert(rpc:call(NodeB, erlang, is_process_alive, [PeerPid])),

    State2 = rpc:call(NodeB, sys, get_state, [PeerPid]),
    ?assert(State2#sbox_state.ebt_active),

    rpc:call(NodeB, gen_server, stop, [PeerPid]).

%% Post messages from two distinct feeds on node_a, have node_b connect and
%% run EBT, then verify node_b replicates both feeds.  This exercises the
%% full-clock path: node_b's initial clock must include all known feeds so
%% node_a knows what to push.
two_node_ebt_multiple_feeds_test(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),

    %% Post to A's own feed.
    AId      = rpc:call(NodeA, keys, pub_key_disp, []),
    AFeedPid = rpc:call(NodeA, utils, find_or_create_feed_pid, [AId]),
    ok = rpc:call(NodeA, ssb_feed, post_content,
                  [AFeedPid, {[{~"type", ~"post"}, {~"text", ~"feed-a msg"}]}]),

    %% A second feed that A follows, so it is within A's replication horizon
    %% and appears in A's clock.
    {PubX, _} = rpc:call(NodeA, utils, create_key_pair, []),
    FeedX     = rpc:call(NodeA, utils, display_pub, [PubX]),
    ok = follow_feed(NodeA, FeedX),
    _ = rpc:call(NodeA, utils, find_or_create_feed_pid, [FeedX]),

    %% A's last sequence now includes the contact message posted above.
    #message{sequence = SeqA} =
        rpc:call(NodeA, ssb_feed, fetch_last_msg, [AFeedPid]),

    %% B follows A and runs EBT.
    ok = follow_feed(NodeB, AId),
    APubKey  = rpc:call(NodeA, keys, pub_key, []),
    ACurvePk = rpc:call(NodeA, base64, decode, [APubKey]),
    {ok, PeerPid} = rpc:call(NodeB, ssb_peer, start,
                              ["localhost", ?PORT_A, ACurvePk]),
    timer:sleep(300),
    rpc:call(NodeB, ssb_peer, request_ebt, [PeerPid]),
    timer:sleep(?EBT_SETTLE_MS),

    %% B must have A's own feed at the correct sequence.
    BFeedPidA = rpc:call(NodeB, utils, find_or_create_feed_pid, [AId]),
    #message{sequence = RepSeqA} =
        rpc:call(NodeB, ssb_feed, fetch_last_msg, [BFeedPidA]),
    ?assert(RepSeqA =:= SeqA),

    %% B must know about feed X (registered in A's clock even at seq 0).
    _ = rpc:call(NodeA, utils, find_or_create_feed_pid, [FeedX]),
    {Clock} = rpc:call(NodeA, ebt, full_clock, []),
    ?assert(lists:keymember(FeedX, 1, Clock)),

    rpc:call(NodeB, gen_server, stop, [PeerPid]).

%% Post a message on node_a AFTER the initial EBT clock exchange, then
%% trigger anti-entropy manually and verify node_b receives the new message.
two_node_ebt_post_connect_replication_test(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),

    %% B follows A so A's feed is within B's replication horizon.
    AId = rpc:call(NodeA, keys, pub_key_disp, []),
    ok = follow_feed(NodeB, AId),

    %% B connects to A and runs EBT (initial sync).
    APubKey  = rpc:call(NodeA, keys, pub_key, []),
    ACurvePk = rpc:call(NodeA, base64, decode, [APubKey]),
    {ok, PeerPid} = rpc:call(NodeB, ssb_peer, start,
                              ["localhost", ?PORT_A, ACurvePk]),
    timer:sleep(300),
    rpc:call(NodeB, ssb_peer, request_ebt, [PeerPid]),
    timer:sleep(?EBT_SETTLE_MS),

    %% Post a new message on A after the initial exchange.
    AId      = rpc:call(NodeA, keys, pub_key_disp, []),
    AFeedPid = rpc:call(NodeA, utils, find_or_create_feed_pid, [AId]),
    ok = rpc:call(NodeA, ssb_feed, post_content,
                  [AFeedPid, {[{~"type", ~"post"}, {~"text", ~"post-connect msg"}]}]),
    #message{sequence = NewSeq} =
        rpc:call(NodeA, ssb_feed, fetch_last_msg, [AFeedPid]),

    %% Trigger anti-entropy on B's peer immediately rather than waiting 30s.
    PeerPid ! ebt_anti_entropy,
    timer:sleep(?EBT_SETTLE_MS),

    %% B must have the new message.
    BFeedPid = rpc:call(NodeB, utils, find_or_create_feed_pid, [AId]),
    #message{sequence = RepSeq} =
        rpc:call(NodeB, ssb_feed, fetch_last_msg, [BFeedPid]),
    ?assert(RepSeq =:= NewSeq),

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

    %% B connects to A
    APubKey  = rpc:call(NodeA, keys, pub_key, []),
    ACurvePk = rpc:call(NodeA, base64, decode, [APubKey]),
    {ok, PeerPid} = rpc:call(NodeB, ssb_peer, start,
                              ["localhost", ?PORT_A, ACurvePk]),

    %% B sends want to A; haves arrive asynchronously to the test process.
    Self = self(),
    rpc:call(NodeB, ssb_peer, request_blob_wants, [PeerPid, [BlobId], Self]),
    receive
        {have, BlobId, Size} ->
            ?assert(Size =:= ExpectedSize)
    after 5000 ->
        ct:fail(no_have_received)
    end,

    rpc:call(NodeB, gen_server, stop, [PeerPid]).

%% Store a blob on node_a, connect from node_b, send a want, and verify
%% that node_b receives the have response with the correct blob size.
two_node_multiple_blob_wants_test(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),

    %% Store blobs on A
    BlobData = ~"erlbutt two-node blob wants test payload",
    BlobId   = rpc:call(NodeA, blobs, store, [BlobData]),
    ExpectedSize = byte_size(BlobData),
    ?assert(rpc:call(NodeA, blobs, has, [BlobId]) =:= true),

    BlobData2 = ~"erlbutt two-node blob wants test payload which is diff",
    BlobId2 = rpc:call(NodeA, blobs, store, [BlobData2]),
    ExpectedSize2 = byte_size(BlobData2),
    ?assert(rpc:call(NodeA, blobs, has, [BlobId2]) =:= true),

    %% B connects to A
    APubKey  = rpc:call(NodeA, keys, pub_key, []),
    ACurvePk = rpc:call(NodeA, base64, decode, [APubKey]),
    {ok, PeerPid} = rpc:call(NodeB, ssb_peer, start,
                              ["localhost", ?PORT_A, ACurvePk]),

    %% B sends want to A; haves arrive asynchronously to the test process.
    Self = self(),
    rpc:call(NodeB, ssb_peer, request_blob_wants, [PeerPid, [BlobId, BlobId2], Self]),

    Haves = rpc:call(NodeB, ssb_peer, drain_haves, [1000]),
    ?assert(lists:all(fun({_Id, Sz}) ->
        Sz =:= ExpectedSize orelse Sz =:= ExpectedSize2
    end, Haves)),

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

%% End-to-end automatic blob replication: node_a publishes a post that
%% mentions a blob it holds; node_b replicates the feed via EBT, and its
%% blob_fetcher notices the missing blob, wants it from node_a, and fetches
%% it without any explicit request.
two_node_auto_blob_fetch_test(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),

    %% A holds a blob and posts a message that mentions it.
    BlobData = ~"erlbutt automatic blob replication payload",
    BlobId   = rpc:call(NodeA, blobs, store, [BlobData]),
    AId      = rpc:call(NodeA, keys, pub_key_disp, []),
    AFeedPid = rpc:call(NodeA, utils, find_or_create_feed_pid, [AId]),
    ok = rpc:call(NodeA, ssb_feed, post_content,
                  [AFeedPid, {[{~"type", ~"post"},
                                {~"text", ~"have a blob"},
                                {~"mentions", [{[{~"link", BlobId}]}]}]}]),

    %% B connects and replicates the feed via EBT.
    APubKey  = rpc:call(NodeA, keys, pub_key, []),
    ACurvePk = rpc:call(NodeA, base64, decode, [APubKey]),
    {ok, PeerPid} = rpc:call(NodeB, ssb_peer, start,
                              ["localhost", ?PORT_A, ACurvePk]),
    timer:sleep(300),
    ok = follow_feed(NodeB, AId),
    rpc:call(NodeB, ssb_peer, request_ebt, [PeerPid]),

    %% Storing the post on B triggers want → have → fetch on its own.
    ?assert(wait_until(fun() ->
        rpc:call(NodeB, blobs, has, [BlobId]) =:= true
    end, 40)),
    ?assertEqual({ok, BlobData}, rpc:call(NodeB, blobs, fetch, [BlobId])),

    rpc:call(NodeB, gen_server, stop, [PeerPid]).

%%% Helpers -------------------------------------------------------------

%% Make Node follow TargetId (post a contact on Node's own feed) and refresh
%% the EBT replication set, so TargetId falls within Node's replication horizon.
follow_feed(Node, TargetId) ->
    SelfId  = rpc:call(Node, keys, pub_key_disp, []),
    SelfPid = rpc:call(Node, utils, find_or_create_feed_pid, [SelfId]),
    Content = {[{~"type", ~"contact"}, {~"contact", TargetId}, {~"following", true}]},
    ok = rpc:call(Node, ssb_feed, post_content, [SelfPid, Content]),
    ok = rpc:call(Node, ebt, refresh_repl_set, []).

wait_until(_F, 0) -> false;
wait_until(F, N) ->
    case F() of
        true  -> true;
        false -> timer:sleep(250), wait_until(F, N - 1)
    end.
