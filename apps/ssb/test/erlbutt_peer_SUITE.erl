%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Loopback peer communication tests.
%%
-module(erlbutt_peer_SUITE).

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([handshake_test/1,
         post_and_fetch_test/1,
         ebt_path_test/1,
         plugin_rpc_test/1,
         manifest_rpc_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ssb/include/ssb.hrl").

-define(TEST_PORT, 18008).

%%% CT callbacks --------------------------------------------------------

all() ->
    [handshake_test,
     post_and_fetch_test,
     ebt_path_test,
     plugin_rpc_test,
     manifest_rpc_test].

init_per_suite(Config) ->
    DataDir = ?config(priv_dir, Config),
    write_test_cfg(DataDir),
    %% Stop ssb if another suite already started it on a different port,
    %% then restart with our isolated config and an ephemeral test port.
    application:stop(ssb),
    application:set_env(ssb, ssb_home, DataDir),
    application:set_env(ssb, port,     ?TEST_PORT),
    application:ensure_all_started(ssb),
    Config.

end_per_suite(Config) ->
    application:stop(ssb),
    Config.

init_per_testcase(_Case, Config) -> Config.
end_per_testcase(_Case, Config)  -> Config.

%%% Tests ---------------------------------------------------------------

%% Full SHS client+server handshake over loopback.
%% ssb_peer:start_link/2 only returns {ok, Pid} after the handshake
%% completes on the client side, so a live process means it succeeded.
handshake_test(_Config) ->
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    ?assert(is_process_alive(Peer)),
    gen_server:stop(Peer).

%% Post a message to our own feed and read it back.
%% Exercises ssb_feed write + fetch without a network round-trip.
post_and_fetch_test(_Config) ->
    FeedPid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    ok = ssb_feed:post_content(FeedPid, ~"hello from CT"),
    #message{} = ssb_feed:fetch_last_msg(FeedPid).

%% Connect to ourselves and verify the EBT code path runs without
%% crashing.  ebt:initial_vector/0 is called during the handshake;
%% we check that it includes our feed with a sequence >= 1 (the
%% message posted in post_and_fetch_test).
%%
%% Note: this test depends on post_and_fetch_test having run first.
%% For true cross-peer replication verification, use multi-node tests.
ebt_path_test(_Config) ->
    {[{OurId, EncodedSeq}]} = ebt:initial_vector(),
    ?assert(OurId == keys:pub_key_disp()),
    {true, true, Seq} = ebt_vc:decode_clock_int(EncodedSeq),
    ?assert(Seq >= 1),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    ?assert(is_process_alive(Peer)),
    gen_server:stop(Peer).

%% Register a plugin and call its methods over a real loopback
%% connection.  The loopback client authenticates with the node's own
%% keypair, so its permission class is owner.
plugin_rpc_test(_Config) ->
    ok = plugin_registry:register_plugin(test_echo_plugin),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),

    %% async method: args come back verbatim
    {ok, Body} = ssb_peer:rpc_call(Peer, [~"echo", ~"hello"], ~"async",
                                   [~"hi there"]),
    ?assertEqual([~"hi there"], utils:nat_decode(Body)),

    %% the plugin sees the authenticated caller identity and class
    {ok, Body2} = ssb_peer:rpc_call(Peer, [~"echo", ~"whoAmI"], ~"async"),
    {Props} = utils:nat_decode(Body2),
    ?assertEqual(keys:pub_key_disp(), proplists:get_value(~"feed", Props)),
    ?assertEqual(~"owner", proplists:get_value(~"class", Props)),

    %% owner-only method is callable by the owner
    {ok, Body3} = ssb_peer:rpc_call(Peer, [~"echo", ~"secrets"], ~"async"),
    ?assertEqual(~"the owner's secrets", utils:nat_decode(Body3)),

    %% source method: one frame per item, then a clean end-of-stream
    {ok, Frames} = ssb_peer:rpc_stream_call(Peer, [~"echo", ~"count"], [3]),
    ?assertEqual([1, 2, 3], [utils:nat_decode(F) || F <- Frames]),

    ok = plugin_registry:unregister_plugin(test_echo_plugin),
    gen_server:stop(Peer).

%% The manifest method serves the node's RPC surface (builtins included),
%% filtered by the caller's class.
manifest_rpc_test(_Config) ->
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, Body} = ssb_peer:rpc_call(Peer, [~"manifest"], ~"sync"),
    {Props} = utils:nat_decode(Body),
    ?assertEqual(~"source", proplists:get_value(~"createHistoryStream", Props)),
    ?assertEqual(~"sync",   proplists:get_value(~"manifest", Props)),
    {Blobs} = proplists:get_value(~"blobs", Props),
    ?assertEqual(~"async", proplists:get_value(~"has", Blobs)),
    %% the loopback caller is the owner, so owner-only methods are visible
    ?assertEqual(~"async", proplists:get_value(~"publish", Props)),
    gen_server:stop(Peer).

%%% Helpers -------------------------------------------------------------

server_pk() ->
    base64:decode(keys:pub_key()).

%% Write a minimal ssb.cfg using absolute paths inside CT's priv_dir
%% so feeds and blobs are fully isolated from ~/.ssberl.
write_test_cfg(DataDir) ->
    CfgFile = filename:join(DataDir, "ssb.cfg"),
    FeedDir = filename:join(DataDir, "feeds/"),
    BlobDir = filename:join(DataDir, "blobs/"),
    ok = filelib:ensure_dir(FeedDir),
    ok = filelib:ensure_dir(BlobDir),
    Terms = io_lib:format(
        "{feed_store_location, \"~s\"}.~n"
        "{blob_store_location, \"~s\"}.~n"
        "{network_id, \"1KHLiKZvAvjbY1ziZEHMXawbCEIM6qwjCDm3VYnaR/s=\"}.~n",
        [FeedDir, BlobDir]),
    ok = file:write_file(CfgFile, Terms),
    CfgFile.
