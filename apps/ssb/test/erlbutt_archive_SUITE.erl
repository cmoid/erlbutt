%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Feed archiving tests.
%%
%% A single isolated node is started per suite.  Two test cases cover:
%%   auto_archive_test  — posting N messages where N = archive_length triggers
%%                        automatic archiving; the genesis post and blob are verified.
%%   manual_archive_test — archive/1 can be called directly regardless of
%%                         message count; same postconditions apply.
-module(erlbutt_archive_SUITE).

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([auto_archive_test/1,
         manual_archive_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ssb/include/ssb.hrl").

-define(PORT, 18112).

%%% CT callbacks --------------------------------------------------------

all() -> [auto_archive_test, manual_archive_test].

init_per_suite(Config) ->
    ssb_ct_helper:ensure_distributed(),
    DataDir = ?config(priv_dir, Config),
    Dir = filename:join(DataDir, "node"),
    ok = filelib:ensure_dir(Dir ++ "/"),
    PAs = ssb_ct_helper:pa_args(),
    {ok, Peer, Node} = ssb_ct_helper:start_ssb_node(node_arc, Dir, ?PORT, PAs),
    [{node, Node}, {peer, Peer} | Config].

end_per_suite(Config) ->
    peer:stop(?config(peer, Config)),
    Config.

init_per_testcase(_Case, Config) -> Config.
end_per_testcase(_Case, Config)  -> Config.

%%% Tests ---------------------------------------------------------------

%% Post archive_length messages — the last post triggers auto-archive.
%% Verify the archive genesis is the last message and its blob is stored.
auto_archive_test(Config) ->
    Node    = ?config(node, Config),
    Id      = rpc:call(Node, keys, pub_key_disp, []),
    FeedPid = rpc:call(Node, utils, find_or_create_feed_pid, [Id]),

    ok = rpc:call(Node, config, set_archive_length, [3]),

    ok = rpc:call(Node, ssb_feed, post_content, [FeedPid, ~"msg 1"]),
    ok = rpc:call(Node, ssb_feed, post_content, [FeedPid, ~"msg 2"]),
    ok = rpc:call(Node, ssb_feed, post_content, [FeedPid, ~"msg 3"]),

    #message{sequence = GenesisSeq,
             previous = null,
             content  = {ContentProps}} =
        rpc:call(Node, ssb_feed, fetch_last_msg, [FeedPid]),

    ?assert(proplists:get_value(~"type", ContentProps) =:= ~"archive"),
    BlobId = proplists:get_value(~"archive", ContentProps),
    ?assert(rpc:call(Node, blobs, has, [BlobId]) =:= true),
    ?assert(proplists:get_value(~"to_sequence", ContentProps) =:= GenesisSeq - 1),
    ?assert(proplists:get_value(~"from_sequence", ContentProps) =:= 1),

    %% Posting continues normally after the archive
    ok = rpc:call(Node, ssb_feed, post_content, [FeedPid, ~"after archive"]),
    #message{sequence = AfterSeq} = rpc:call(Node, ssb_feed, fetch_last_msg, [FeedPid]),
    ?assert(AfterSeq =:= GenesisSeq + 1).

%% Disable auto-archive, post 2 messages, then call archive/1 directly.
%% Verify the same genesis + blob postconditions hold.
manual_archive_test(Config) ->
    Node    = ?config(node, Config),
    Id      = rpc:call(Node, keys, pub_key_disp, []),
    FeedPid = rpc:call(Node, utils, find_or_create_feed_pid, [Id]),

    ok = rpc:call(Node, config, set_archive_length, [undefined]),

    ok = rpc:call(Node, ssb_feed, post_content, [FeedPid, ~"manual 1"]),
    ok = rpc:call(Node, ssb_feed, post_content, [FeedPid, ~"manual 2"]),
    #message{sequence = PreSeq} = rpc:call(Node, ssb_feed, fetch_last_msg, [FeedPid]),

    {ok, BlobId} = rpc:call(Node, ssb_feed, archive, [FeedPid]),
    ?assert(rpc:call(Node, blobs, has, [BlobId]) =:= true),

    #message{sequence = GenesisSeq,
             previous = null,
             content  = {ContentProps}} =
        rpc:call(Node, ssb_feed, fetch_last_msg, [FeedPid]),

    ?assert(GenesisSeq =:= PreSeq + 1),
    ?assert(proplists:get_value(~"type", ContentProps) =:= ~"archive"),
    ?assert(proplists:get_value(~"to_sequence", ContentProps) =:= PreSeq).

