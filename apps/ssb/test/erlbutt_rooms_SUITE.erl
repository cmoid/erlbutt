%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Rooms tests.
%%
%% node_a is configured as a room (relay), node_b connects to it as a
%% client.  Phase 1 verifies room identity: tunnel.isRoom and room.metadata.
-module(erlbutt_rooms_SUITE).

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([is_room_true_test/1,
         room_metadata_test/1,
         presence_join_leave_test/1,
         tunnel_relay_test/1,
         tunnel_handshake_test/1,
         tunnel_blob_replication_test/1,
         tunnel_feed_replication_test/1]).

%% Spawned on the target node as its tunnel listener; echoes each frame back.
-export([echo_loop/0]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ssb/include/ssb.hrl").

-define(PORT_A, 18118).
-define(PORT_B, 18119).
-define(PORT_C, 18120).
-define(ROOM_NAME, <<"test room">>).

%%% CT callbacks --------------------------------------------------------

all() ->
    [is_room_true_test,
     room_metadata_test,
     presence_join_leave_test,
     tunnel_relay_test,
     tunnel_handshake_test,
     tunnel_blob_replication_test,
     tunnel_feed_replication_test].

init_per_suite(Config) ->
    ssb_ct_helper:ensure_distributed(),

    DataDir = ?config(priv_dir, Config),
    DirA    = filename:join(DataDir, "node_a"),
    DirB    = filename:join(DataDir, "node_b"),
    ok = filelib:ensure_dir(DirA ++ "/"),
    ok = filelib:ensure_dir(DirB ++ "/"),

    PAs = ssb_ct_helper:pa_args(),
    %% Unique peer node names (room_*) so this suite never collides with other
    %% suites that use node_a/node_b when CT runs them in the same session.
    %% room_a is a room.
    {ok, PeerA, NodeA} =
        ssb_ct_helper:start_ssb_node(room_a, DirA, ?PORT_A, PAs,
                                     [{room, true}, {room_name, ?ROOM_NAME}]),
    {ok, PeerB, NodeB} = ssb_ct_helper:start_ssb_node(room_b, DirB, ?PORT_B, PAs),
    DirC = filename:join(DataDir, "node_c"),
    ok = filelib:ensure_dir(DirC ++ "/"),
    {ok, PeerC, NodeC} = ssb_ct_helper:start_ssb_node(room_c, DirC, ?PORT_C, PAs),

    [{node_a, NodeA}, {peer_a, PeerA},
     {node_b, NodeB}, {peer_b, PeerB},
     {node_c, NodeC}, {peer_c, PeerC} | Config].

end_per_suite(Config) ->
    peer:stop(?config(peer_a, Config)),
    peer:stop(?config(peer_b, Config)),
    peer:stop(?config(peer_c, Config)),
    Config.

init_per_testcase(_Case, Config) -> Config.
end_per_testcase(_Case, Config)  -> Config.

%%% Tests ---------------------------------------------------------------

%% node_b connects to the room node_a and asks tunnel.isRoom — a room
%% answers true.
is_room_true_test(Config) ->
    {NodeB, PeerPid} = connect_to_room(Config),
    {ok, Body} = rpc:call(NodeB, ssb_peer, rpc_call,
                          [PeerPid, [?tunnel, ?isRoom], ~"async"]),
    ?assertEqual(true, rpc:call(NodeB, utils, nat_decode, [Body])),
    rpc:call(NodeB, gen_server, stop, [PeerPid]).

%% room.metadata returns the configured room name.
room_metadata_test(Config) ->
    {NodeB, PeerPid} = connect_to_room(Config),
    {ok, Body} = rpc:call(NodeB, ssb_peer, rpc_call,
                          [PeerPid, [?room, ?metadata], ~"async"]),
    {Props} = rpc:call(NodeB, utils, nat_decode, [Body]),
    ?assertEqual(?ROOM_NAME, proplists:get_value(~"name", Props)),
    ?assertEqual(true, proplists:get_value(~"membership", Props)),
    rpc:call(NodeB, gen_server, stop, [PeerPid]).

%% node_b observes the room's presence stream; node_c joins then leaves,
%% and node_b must receive a `joined` followed by a `left` event for C.
presence_join_leave_test(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),
    NodeC = ?config(node_c, Config),
    ACurvePk = room_curve_pk(NodeA),

    %% B connects and opens the presence stream.
    PeerB = connect_alive(NodeB, ACurvePk, 5),
    {ok, Ref} = rpc:call(NodeB, ssb_peer, open_source,
                         [PeerB, [?room, ?attendants], [], self()]),

    %% First frame is the presence snapshot.
    StateProps = recv_frame(NodeB, Ref),
    ?assertEqual(~"state", proplists:get_value(~"type", StateProps)),

    %% C connects → joined event carrying C's feed id.
    CFeedId = <<"@", (rpc:call(NodeC, keys, pub_key, []))/binary, ".ed25519">>,
    PeerC = connect_alive(NodeC, ACurvePk, 5),
    JoinProps = recv_frame(NodeB, Ref),
    ?assertEqual(~"joined", proplists:get_value(~"type", JoinProps)),
    ?assertEqual(CFeedId, proplists:get_value(~"id", JoinProps)),

    %% C disconnects → left event for the same feed id.
    rpc:call(NodeC, gen_server, stop, [PeerC]),
    LeftProps = recv_frame(NodeB, Ref),
    ?assertEqual(~"left", proplists:get_value(~"type", LeftProps)),
    ?assertEqual(CFeedId, proplists:get_value(~"id", LeftProps)),

    rpc:call(NodeB, gen_server, stop, [PeerB]).

%% node_b opens a tunnel through the room to node_c, which echoes.  A byte
%% payload written by B must arrive at C, be echoed, and come back to B —
%% proving the room relays frames in both directions.
tunnel_relay_test(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),
    NodeC = ?config(node_c, Config),
    APub = rpc:call(NodeA, keys, pub_key, []),
    ACurvePk = rpc:call(NodeA, base64, decode, [APub]),
    BFeedId = <<"@", (rpc:call(NodeB, keys, pub_key, []))/binary, ".ed25519">>,
    CFeedId = <<"@", (rpc:call(NodeC, keys, pub_key, []))/binary, ".ed25519">>,
    RoomFeedId = <<"@", APub/binary, ".ed25519">>,

    %% Both endpoints connect to the room.
    PeerB = connect_alive(NodeB, ACurvePk, 5),
    PeerC = connect_alive(NodeC, ACurvePk, 5),

    %% node_c accepts tunnels with an echo listener.
    EchoPid = spawn(NodeC, ?MODULE, echo_loop, []),
    ok = rpc:call(NodeC, tunnel_endpoint, set_listener, [EchoPid]),

    %% Wait until the room registers node_c's presence before dialing it,
    %% otherwise the relay would miss the target and return an error frame.
    ok = wait_attendant(NodeA, CFeedId, 20),

    %% node_b dials node_c through the room.
    Args = [{[{~"origin", BFeedId},
              {~"target", CFeedId},
              {~"portal", RoomFeedId}]}],
    {ok, ReqNo} = rpc:call(NodeB, ssb_peer, open_duplex,
                           [PeerB, [?tunnel, ?connect], Args, self()]),
    timer:sleep(300),

    %% A byte payload from B is echoed back through the room.
    rpc:call(NodeB, ssb_peer, send_frame, [PeerB, ReqNo, ~"hello"]),
    receive
        {tunnel_data, _Neg, Resp} ->
            ?assertEqual(~"echo:hello", Resp)
    after 5000 ->
        ct:fail(no_tunnel_echo)
    end,

    rpc:call(NodeB, gen_server, stop, [PeerB]),
    rpc:call(NodeC, gen_server, stop, [PeerC]).

%% A full peer runs over the tunnel: node_b dials node_c through the room,
%% they complete the inner SHS, and a whoami RPC returns node_c's id over the
%% encrypted channel — proving real muxrpc works through the room.
tunnel_handshake_test(Config) ->
    {NodeB, NodeC, PeerB, PeerC, TPid, _CFeedId} = open_tunnel_to_c(Config),
    CId = rpc:call(NodeC, keys, pub_key_disp, []),
    {ok, Body} = rpc:call(NodeB, ssb_peer, rpc_call, [TPid, [?whoami], ~"async"]),
    {Props} = rpc:call(NodeB, utils, nat_decode, [Body]),
    ?assertEqual(CId, proplists:get_value(~"id", Props)),
    stop_tunnel(NodeB, NodeC, PeerB, PeerC, TPid).

%% A blob held by node_c is fetched by node_b through the room over the tunnel.
tunnel_blob_replication_test(Config) ->
    NodeC0 = ?config(node_c, Config),
    BlobData = crypto:strong_rand_bytes(2048),
    BlobId   = rpc:call(NodeC0, blobs, store, [BlobData]),
    ?assert(rpc:call(NodeC0, blobs, has, [BlobId]) =:= true),

    {NodeB, NodeC, PeerB, PeerC, TPid, _CFeedId} = open_tunnel_to_c(Config),
    ?assertEqual({ok, BlobData},
                 rpc:call(NodeB, ssb_peer, fetch_blob, [TPid, BlobId])),
    stop_tunnel(NodeB, NodeC, PeerB, PeerC, TPid).

%% A feed published by node_c replicates to node_b through the room: node_b
%% pulls it via createHistoryStream over the tunnel and sees the message.
tunnel_feed_replication_test(Config) ->
    NodeC0 = ?config(node_c, Config),
    CId    = rpc:call(NodeC0, keys, pub_key_disp, []),
    CFeed  = rpc:call(NodeC0, utils, find_or_create_feed_pid, [CId]),
    Text   = ~"hello through the room",
    ok = rpc:call(NodeC0, ssb_feed, post_content,
                  [CFeed, {[{~"type", ~"post"}, {~"text", Text}]}]),

    {NodeB, NodeC, PeerB, PeerC, TPid, _CFeedId} = open_tunnel_to_c(Config),
    Args = [{[{~"id", CId}, {~"seq", 0}, {~"keys", true}]}],
    {ok, Msgs} = rpc:call(NodeB, ssb_peer, rpc_stream_call,
                          [TPid, [?createhistorystream], Args]),
    Texts = [text_of(NodeB, M) || M <- Msgs],
    ?assert(lists:member(Text, Texts)),
    stop_tunnel(NodeB, NodeC, PeerB, PeerC, TPid).

%% Establish a tunnelled full peer from node_b to node_c through room node_a.
%% Returns {NodeB, NodeC, PeerB, TunnelPeerPid, CFeedId}.
open_tunnel_to_c(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),
    NodeC = ?config(node_c, Config),
    ACurvePk = room_curve_pk(NodeA),
    CPub     = rpc:call(NodeC, keys, pub_key, []),
    CCurvePk = rpc:call(NodeC, base64, decode, [CPub]),
    CFeedId  = <<"@", CPub/binary, ".ed25519">>,
    BFeedId  = <<"@", (rpc:call(NodeB, keys, pub_key, []))/binary, ".ed25519">>,
    RoomFeedId = <<"@", (rpc:call(NodeA, keys, pub_key, []))/binary, ".ed25519">>,

    PeerB = connect_alive(NodeB, ACurvePk, 5),
    PeerC = connect_alive(NodeC, ACurvePk, 5),
    %% node_c accepts with the built-in SHS server (clear any echo override).
    ok = rpc:call(NodeC, tunnel_endpoint, set_listener, [undefined]),
    ok = wait_attendant(NodeA, CFeedId, 20),

    Args = [{[{~"origin", BFeedId},
              {~"target", CFeedId},
              {~"portal", RoomFeedId}]}],
    {ok, TPid} = rpc:call(NodeB, ssb_peer, tunnel_connect, [PeerB, CCurvePk, Args]),
    timer:sleep(500),
    {NodeB, NodeC, PeerB, PeerC, TPid, CFeedId}.

stop_tunnel(NodeB, NodeC, PeerB, PeerC, TPid) ->
    catch rpc:call(NodeB, gen_server, stop, [TPid]),
    catch rpc:call(NodeB, gen_server, stop, [PeerB]),
    catch rpc:call(NodeC, gen_server, stop, [PeerC]),
    timer:sleep(200),
    ok.

%% Extract the post text from a createHistoryStream frame.  With keys:true each
%% frame is a {key, value:{...}} envelope; content lives under value.  Returns
%% undefined for non-post/encrypted frames rather than crashing.
text_of(Node, MsgBin) ->
    Props = case rpc:call(Node, utils, nat_decode, [MsgBin]) of
        {P} -> P;
        _   -> []
    end,
    Value = case proplists:get_value(~"value", Props) of
        {V} -> V;
        _   -> Props
    end,
    case proplists:get_value(~"content", Value) of
        {Content} -> proplists:get_value(~"text", Content);
        _         -> undefined
    end.

%% Tunnel listener: track each tunnel's owner peer, echo every frame back
%% on the same stream (-ReqNo).
echo_loop() -> echo_loop(#{}).
echo_loop(Owners) ->
    receive
        {tunnel_open, ReqNo, OwnerPid} ->
            echo_loop(Owners#{ReqNo => OwnerPid});
        {tunnel_data, ReqNo, Body} ->
            case maps:find(ReqNo, Owners) of
                {ok, OwnerPid} ->
                    ssb_peer:send_frame(OwnerPid, -ReqNo, <<"echo:", Body/binary>>);
                error -> ok
            end,
            echo_loop(Owners)
    end.

%%% Helpers -------------------------------------------------------------

recv_frame(Node, Ref) ->
    receive
        {stream_data, Ref, Body} ->
            {Props} = rpc:call(Node, utils, nat_decode, [Body]),
            Props
    after 5000 ->
        ct:fail(no_attendants_frame)
    end.

wait_attendant(_NodeA, FeedId, 0) ->
    ct:fail({attendant_not_registered, FeedId});
wait_attendant(NodeA, FeedId, N) ->
    case rpc:call(NodeA, room_attendants, lookup, [FeedId]) of
        {ok, _Pid} -> ok;
        _          -> timer:sleep(100), wait_attendant(NodeA, FeedId, N - 1)
    end.

connect_to_room(Config) ->
    NodeA = ?config(node_a, Config),
    NodeB = ?config(node_b, Config),
    ACurvePk = room_curve_pk(NodeA),
    {NodeB, connect_alive(NodeB, ACurvePk, 5)}.

room_curve_pk(NodeA) ->
    rpc:call(NodeA, base64, decode, [rpc:call(NodeA, keys, pub_key, [])]).

%% Connect Node to the room, retrying if the freshly-started peer dies before
%% it settles (these multi-node connects are timing-sensitive).
connect_alive(_Node, _ACurvePk, 0) ->
    ct:fail(could_not_connect_to_room);
connect_alive(Node, ACurvePk, N) ->
    %% A prior peer not yet fully torn down makes start return `ignore`
    %% (duplicate guard); wait and retry rather than crash.
    case rpc:call(Node, ssb_peer, start, ["localhost", ?PORT_A, ACurvePk]) of
        {ok, PeerPid} ->
            timer:sleep(300),
            case rpc:call(Node, erlang, is_process_alive, [PeerPid]) of
                true  -> PeerPid;
                false -> timer:sleep(200), connect_alive(Node, ACurvePk, N - 1)
            end;
        _ ->
            timer:sleep(300),
            connect_alive(Node, ACurvePk, N - 1)
    end.
