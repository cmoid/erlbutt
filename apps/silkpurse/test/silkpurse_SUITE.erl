%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% End-to-end tests for the silkpurse client-serving app: plugins and
%% views registered from a separate OTP app, called over a real
%% loopback connection (owner class).
-module(silkpurse_SUITE).

-export([all/0,
         init_per_suite/1,
         end_per_suite/1]).

-export([backlinks_read_test/1,
         get_latest_test/1,
         manifest_includes_silkpurse_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ssb/include/ssb.hrl").

-define(TEST_PORT, 18208).

all() ->
    [backlinks_read_test,
     get_latest_test,
     manifest_includes_silkpurse_test].

init_per_suite(Config) ->
    DataDir = ?config(priv_dir, Config),
    write_test_cfg(DataDir),
    application:stop(silkpurse),
    application:stop(ssb),
    application:set_env(ssb, ssb_home, DataDir),
    application:set_env(ssb, port,     ?TEST_PORT),
    {ok, _} = application:ensure_all_started(silkpurse),
    Config.

end_per_suite(Config) ->
    application:stop(silkpurse),
    application:stop(ssb),
    Config.

%%% Tests ---------------------------------------------------------------

%% Post a root and a reply, then read the reply back over the wire via
%% backlinks.read with the flumeview-query argument the JS client sends.
backlinks_read_test(_Config) ->
    OwnPid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"},
                                         {~"text", ~"ct root"}]}),
    #message{id = RootId} = ssb_feed:fetch_last_msg(OwnPid),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"},
                                         {~"text", ~"ct reply"},
                                         {~"root", RootId}]}),
    #message{id = ReplyId} = ssb_feed:fetch_last_msg(OwnPid),

    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    Args = [{[{~"query", [{[{~"$filter", {[{~"dest", RootId}]}}]}]}]}],
    {ok, Frames} = ssb_peer:rpc_stream_call(Peer, [~"backlinks", ~"read"],
                                            Args),
    Ids = [begin #message{id = Id} = message:decode(F, false), Id end
           || F <- Frames],
    ?assert(lists:member(ReplyId, Ids)),
    gen_server:stop(Peer).

get_latest_test(_Config) ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    ok = ssb_feed:post_content(OwnPid, ~"latest marker"),
    #message{id = LastId, sequence = LastSeq} = ssb_feed:fetch_last_msg(OwnPid),

    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, Body} = ssb_peer:rpc_call(Peer, [~"getLatest"], ~"async", [OwnId]),
    {Props} = utils:nat_decode(Body),
    ?assertEqual(LastId,  proplists:get_value(~"id", Props)),
    ?assertEqual(LastSeq, proplists:get_value(~"sequence", Props)),
    gen_server:stop(Peer).

%% The silkpurse app's methods appear in the manifest erlbutt serves.
manifest_includes_silkpurse_test(_Config) ->
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, Body} = ssb_peer:rpc_call(Peer, [~"manifest"], ~"sync"),
    {Props} = utils:nat_decode(Body),
    {Backlinks} = proplists:get_value(~"backlinks", Props),
    ?assertEqual(~"source", proplists:get_value(~"read", Backlinks)),
    ?assertEqual(~"async",  proplists:get_value(~"getLatest", Props)),
    gen_server:stop(Peer).

%%% Helpers -------------------------------------------------------------

server_pk() ->
    base64:decode(keys:pub_key()).

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
