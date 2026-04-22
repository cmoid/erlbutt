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
         ebt_path_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ssb/include/ssb.hrl").

-define(TEST_PORT, 18008).

%%% CT callbacks --------------------------------------------------------

all() ->
    [handshake_test,
     post_and_fetch_test,
     ebt_path_test].

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
