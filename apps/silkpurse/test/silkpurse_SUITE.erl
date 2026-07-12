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
         messages_by_type_test/1,
         live_backlinks_test/1,
         about_social_value_test/1,
         live_about_test/1,
         friends_get_test/1,
         suggest_profile_test/1,
         profile_avatar_test/1,
         heartbeat_test/1,
         blobs_ls_test/1,
         channels_test/1,
         private_get_test/1,
         private_feed_test/1,
         backlinks_streams_test/1,
         recent_feeds_test/1,
         subscriptions_test/1,
         contacts_state_stream_test/1,
         likes_test/1,
         thread_sorted_test/1,
         public_feed_roots_test/1,
         public_feed_latest_test/1,
         profile_roots_test/1,
         mentions_feed_test/1,
         manifest_includes_silkpurse_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ssb/include/ssb.hrl").

-define(TEST_PORT, 18208).

all() ->
    [backlinks_read_test,
     get_latest_test,
     messages_by_type_test,
     live_backlinks_test,
     about_social_value_test,
     live_about_test,
     friends_get_test,
     suggest_profile_test,
     profile_avatar_test,
     heartbeat_test,
     blobs_ls_test,
     channels_test,
     private_get_test,
     private_feed_test,
     backlinks_streams_test,
     recent_feeds_test,
     subscriptions_test,
     contacts_state_stream_test,
     likes_test,
     thread_sorted_test,
     public_feed_roots_test,
     public_feed_latest_test,
     profile_roots_test,
     mentions_feed_test,
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

%% Type-scoped scan over the wire: post a contact, read it back via
%% messagesByType while a differently-typed message stays out.
messages_by_type_test(_Config) ->
    OwnPid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    Target = fresh_feed_id(),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"contact"},
                                         {~"contact", Target},
                                         {~"following", true}]}),
    #message{id = ContactId} = ssb_feed:fetch_last_msg(OwnPid),

    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, Frames} = ssb_peer:rpc_stream_call(Peer, [~"messagesByType"],
                                            [~"contact"]),
    Ids = [begin #message{id = Id} = message:decode(F, false), Id end
           || F <- Frames],
    ?assert(lists:member(ContactId, Ids)),
    %% the ct-root/ct-reply posts from backlinks_read_test are type post
    {ok, PostFrames} = ssb_peer:rpc_stream_call(Peer, [~"messagesByType"],
                                                [~"post"]),
    ?assertNot(lists:member(ContactId,
                            [begin #message{id = I} = message:decode(F, false), I end
                             || F <- PostFrames])),
    gen_server:stop(Peer).

%% {live: true}: open the stream first, post the reply afterwards, and
%% receive it as a pushed frame — the view-event -> view_stream ->
%% send_frame path end to end.
live_backlinks_test(_Config) ->
    OwnPid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"},
                                         {~"text", ~"live root"}]}),
    #message{id = RootId} = ssb_feed:fetch_last_msg(OwnPid),

    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    Args = [{[{~"query", [{[{~"$filter", {[{~"dest", RootId}]}}]}]},
              {~"live", true}, {~"old", false}]}],
    {ok, _Ref} = ssb_peer:open_source(Peer, [~"backlinks", ~"read"],
                                      Args, self()),

    %% nothing yet: old=false and no replies exist
    receive {stream_data, _, Early} -> error({unexpected_early, Early})
    after 300 -> ok
    end,

    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"},
                                         {~"text", ~"live reply"},
                                         {~"root", RootId}]}),
    #message{id = ReplyId} = ssb_feed:fetch_last_msg(OwnPid),
    receive
        {stream_data, _, Frame} ->
            #message{id = GotId} = message:decode(Frame, false),
            ?assertEqual(ReplyId, GotId)
    after 3000 ->
        error(no_live_frame)
    end,
    gen_server:stop(Peer).

%% thread.sorted streams the root, its replies, and a sync sentinel.
thread_sorted_test(_Config) ->
    OwnPid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"}, {~"text", ~"thread root"}]}),
    #message{id = RootId} = ssb_feed:fetch_last_msg(OwnPid),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"}, {~"text", ~"a reply"},
                                         {~"root", RootId}]}),
    #message{id = ReplyId} = ssb_feed:fetch_last_msg(OwnPid),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    %% old-only (non-live) so the stream ends and we can collect it
    Args = [{[{~"dest", RootId}, {~"live", false}, {~"old", true},
              {~"types", [~"post"]}]}],
    {ok, Frames} = ssb_peer:rpc_stream_call(
                     Peer, [~"patchwork", ~"thread", ~"sorted"], Args),
    Decoded = [utils:nat_decode(F) || F <- Frames],
    Keys = [proplists:get_value(~"key", P) || {P} <- Decoded,
                                              proplists:is_defined(~"key", P)],
    %% the root is NOT streamed — the renderer supplies it via get and
    %% prepends it; emitting it here too duplicates it in the thread
    ?assertNot(lists:member(RootId, Keys)),
    ?assert(lists:member(ReplyId, Keys)),  %% reply present
    ?assert(lists:any(fun({P}) -> proplists:get_value(~"sync", P) =:= true;
                         (_) -> false end, Decoded)),   %% sync sentinel
    gen_server:stop(Peer).

%% likes.get returns the likers of a message, and countStream its live
%% count, after a vote is posted over the wire.
likes_test(_Config) ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"}, {~"text", ~"like me"}]}),
    #message{id = Target} = ssb_feed:fetch_last_msg(OwnPid),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"vote"},
                                         {~"vote", {[{~"link", Target},
                                                     {~"value", 1}]}}]}),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, GetBody} = ssb_peer:rpc_call(Peer, [~"patchwork", ~"likes", ~"get"],
                                      ~"async", [{[{~"dest", Target}]}]),
    ?assert(lists:member(OwnId, utils:nat_decode(GetBody))),
    %% countStream is a live stream (never ends); read its first frame
    {ok, _Ref} = ssb_peer:open_source(
                   Peer, [~"patchwork", ~"likes", ~"countStream"],
                   [{[{~"dest", Target}]}], self()),
    receive
        {stream_data, _, CountFrame} ->
            ?assertEqual(1, utils:nat_decode(CountFrame))
    after 3000 ->
        error(no_count_frame)
    end,
    gen_server:stop(Peer).

%% get({id, private:true}) unboxes a private message addressed to us:
%% the content comes back decrypted with a private:true marker.
private_get_test(_Config) ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    ok = ssb_feed:post_private(OwnPid,
                               {[{~"type", ~"post"}, {~"text", ~"a secret"},
                                 {~"recps", [OwnId]}]}, [OwnId]),
    #message{id = MsgId} = ssb_feed:fetch_last_msg(OwnPid),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    %% without private:true the content stays boxed
    {ok, Boxed} = ssb_peer:rpc_call(Peer, [~"get"], ~"async", [MsgId]),
    {BoxedVal} = utils:nat_decode(Boxed),
    ?assert(is_binary(proplists:get_value(~"content", BoxedVal))),
    %% with private:true it is unboxed in place
    {ok, Body} = ssb_peer:rpc_call(Peer, [~"get"], ~"async",
                                   [{[{~"id", MsgId}, {~"private", true}]}]),
    {Value} = utils:nat_decode(Body),
    ?assertEqual(true, proplists:get_value(~"private", Value)),
    {Content} = proplists:get_value(~"content", Value),
    ?assertEqual(~"a secret", proplists:get_value(~"text", Content)),
    gen_server:stop(Peer).

%% privateFeed.roots rolls up private threads with decrypted content and
%% excludes public posts.
private_feed_test(_Config) ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    ok = ssb_feed:post_private(OwnPid,
                               {[{~"type", ~"post"}, {~"text", ~"pf root"},
                                 {~"recps", [OwnId]}]}, [OwnId]),
    #message{id = RootId} = ssb_feed:fetch_last_msg(OwnPid),
    ok = ssb_feed:post_private(OwnPid,
                               {[{~"type", ~"post"}, {~"text", ~"pf reply"},
                                 {~"root", RootId}, {~"recps", [OwnId]}]}, [OwnId]),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, Frames} = ssb_peer:rpc_stream_call(
                     Peer, [~"patchwork", ~"privateFeed", ~"roots"], [{[]}]),
    Decoded = [utils:nat_decode(F) || F <- Frames],
    Match = [P || {P} <- Decoded, proplists:get_value(~"key", P) =:= RootId],
    ?assertMatch([_], Match),
    [Props] = Match,
    ?assertEqual(1, proplists:get_value(~"totalReplies", Props)),
    {Value} = proplists:get_value(~"value", Props),
    ?assertEqual(true, proplists:get_value(~"private", Value)),
    {Content} = proplists:get_value(~"content", Value),
    ?assertEqual(~"pf root", proplists:get_value(~"text", Content)),
    gen_server:stop(Peer).

%% subscriptions({channel}) lists a channel's subscribers as {from,value}.
subscriptions_test(_Config) ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    Ch = <<"subs", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"channel"},
                                         {~"channel", Ch},
                                         {~"subscribed", true}]}),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, _Ref} = ssb_peer:open_source(
                   Peer, [~"patchwork", ~"subscriptions"],
                   [{[{~"channel", Ch}]}], self()),
    receive
        {stream_data, _, F} ->
            {P} = utils:nat_decode(F),
            ?assertEqual(OwnId, proplists:get_value(~"from", P)),
            ?assertEqual(true,  proplists:get_value(~"value", P))
    after 3000 -> error(no_subscriber_frame)
    end,
    gen_server:stop(Peer).

%% recentFeeds lists feeds that recently started a thread.
recent_feeds_test(_Config) ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"}, {~"text", ~"recent root"}]}),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, Frames} = ssb_peer:rpc_stream_call(
                     Peer, [~"patchwork", ~"recentFeeds"], [{[{~"since", 0}]}]),
    Feeds = [utils:nat_decode(F) || F <- Frames],
    ?assert(lists:member(OwnId, Feeds)),
    gen_server:stop(Peer).

%% backlinks.referencesStream returns messages referencing a target, and
%% liveBacklinks.stream pushes new backlinks tagged with their dest.
backlinks_streams_test(_Config) ->
    OwnPid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"}, {~"text", ~"ref root"}]}),
    #message{id = RootId} = ssb_feed:fetch_last_msg(OwnPid),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"}, {~"text", ~"a ref"},
                                         {~"root", RootId}]}),
    #message{id = ReplyId} = ssb_feed:fetch_last_msg(OwnPid),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    %% referencesStream: the existing reply arrives in the snapshot
    {ok, _R} = ssb_peer:open_source(
                 Peer, [~"patchwork", ~"backlinks", ~"referencesStream"],
                 [{[{~"id", RootId}]}], self()),
    receive
        {stream_data, _, F} -> ?assertEqual(ReplyId, (message:decode(F, false))#message.id)
    after 3000 -> error(no_reference)
    end,
    %% liveBacklinks.stream: open, post a new backlink, receive it dest-tagged.
    %% (the referencesStream above is still open and also delivers the reply,
    %% so skip frames without a dest field.)
    {ok, _L} = ssb_peer:open_source(
                 Peer, [~"patchwork", ~"liveBacklinks", ~"stream"], [], self()),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"}, {~"text", ~"live ref"},
                                         {~"root", RootId}]}),
    ?assertEqual(RootId, recv_dest_frame()),
    gen_server:stop(Peer).

%% Receive stream frames until one carries a `dest` field, returning it.
recv_dest_frame() ->
    receive
        {stream_data, _, F} ->
            case utils:nat_decode(F) of
                {P} when is_list(P) ->
                    case proplists:get_value(~"dest", P) of
                        undefined -> recv_dest_frame();
                        Dest      -> Dest
                    end;
                _ -> recv_dest_frame()
            end
    after 3000 -> error(no_live_backlink)
    end.

%% channels.suggest matches a channel by prefix (with a post count), and
%% channels.recentStream lists recently-active channels.
channels_test(_Config) ->
    OwnPid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    Ch = <<"ct", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"}, {~"text", ~"in a channel"},
                                         {~"channel", Ch}]}),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    %% unique_integer can be a single digit, so the name may be <4 bytes
    Prefix = binary:part(Ch, 0, min(byte_size(Ch), 4)),
    {ok, Body} = ssb_peer:rpc_call(Peer, [~"patchwork", ~"channels", ~"suggest"],
                                   ~"async", [{[{~"text", Prefix}, {~"limit", 20}]}]),
    Items = utils:nat_decode(Body),
    Ids = [proplists:get_value(~"id", P) || {P} <- Items],
    ?assert(lists:member(Ch, Ids)),
    %% recentStream is live; read its first frame (the current list)
    {ok, _Ref} = ssb_peer:open_source(
                   Peer, [~"patchwork", ~"channels", ~"recentStream"],
                   [{[{~"limit", 10}]}], self()),
    receive
        {stream_data, _, Frame} ->
            ?assert(lists:member(Ch, utils:nat_decode(Frame)))
    after 3000 ->
        error(no_recent_frame)
    end,
    gen_server:stop(Peer).

%% patchwork.heartbeat is a live source the silkpurse_heartbeat timer
%% pulses under once a second; a frame must arrive to keep the client's
%% progress spinner hidden.
heartbeat_test(_Config) ->
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, _Ref} = ssb_peer:open_source(
                   Peer, [~"patchwork", ~"heartbeat"], [], self()),
    receive
        {stream_data, _, _TickFrame} -> ok
    after 3000 ->
        error(no_heartbeat_frame)
    end,
    gen_server:stop(Peer).

%% blobs.ls({old:false}) live-streams each newly stored blob id — the
%% renderer's blob-arrival wait path (blob/obs/has.js waitFor).
blobs_ls_test(_Config) ->
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, _Ref} = ssb_peer:open_source(Peer, [~"blobs", ~"ls"],
                                      [{[{~"old", false}]}], self()),
    %% requests on one connection are processed in order: once whoami
    %% answers, the ls subscription is registered and live
    {ok, _} = ssb_peer:rpc_call(Peer, [~"whoami"], ~"sync", []),
    BlobId = blobs:store(crypto:strong_rand_bytes(64)),
    receive
        {stream_data, _, Frame} ->
            ?assertEqual(BlobId, utils:nat_decode(Frame))
    after 3000 ->
        error(no_blobs_ls_frame)
    end,
    gen_server:stop(Peer).

%% contacts.stateStream (non-live) returns a feed's follow/block dict:
%% after following a target, the owner's forward state includes it as true.
contacts_state_stream_test(_Config) ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    Target = fresh_feed_id(),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"contact"},
                                         {~"contact", Target},
                                         {~"following", true}]}),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    Args = [{[{~"feedId", OwnId}]}],
    {ok, Frames} = ssb_peer:rpc_stream_call(
                     Peer, [~"patchwork", ~"contacts", ~"stateStream"], Args),
    {Dict} = utils:nat_decode(hd(Frames)),
    ?assertEqual(true, proplists:get_value(Target, Dict)),
    gen_server:stop(Peer).

%% publicFeed.roots returns each thread root once, carrying its reply
%% count and recent replies, ordered by activity.
public_feed_roots_test(_Config) ->
    OwnPid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    Marker = base64:encode(crypto:strong_rand_bytes(9)),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"}, {~"text", Marker}]}),
    #message{id = RootId} = ssb_feed:fetch_last_msg(OwnPid),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"}, {~"text", ~"reply one"},
                                         {~"root", RootId}]}),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"}, {~"text", ~"reply two"},
                                         {~"root", RootId}]}),

    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, Frames} = ssb_peer:rpc_stream_call(Peer, [~"patchwork", ~"publicFeed", ~"roots"],
                                            [{[{~"limit", 20}]}]),
    Items = [utils:nat_decode(F) || F <- Frames],
    Match = [P || {P} <- Items, proplists:get_value(~"key", P) =:= RootId],
    ?assertEqual(1, length(Match)),      %% the root appears exactly once
    [{Props}] = [{P} || {P} <- Items, proplists:get_value(~"key", P) =:= RootId],
    ?assertEqual(2, proplists:get_value(~"totalReplies", Props)),
    ?assertEqual(2, length(proplists:get_value(~"latestReplies", Props))),
    gen_server:stop(Peer).

%% profile.roots is publicFeed filtered to one author.
profile_roots_test(_Config) ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    Marker = base64:encode(crypto:strong_rand_bytes(9)),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"}, {~"text", Marker}]}),
    #message{id = RootId} = ssb_feed:fetch_last_msg(OwnPid),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, Mine} = ssb_peer:rpc_stream_call(
                   Peer, [~"patchwork", ~"profile", ~"roots"],
                   [{[{~"id", OwnId}, {~"limit", 50}]}]),
    ?assert(lists:member(RootId, root_keys(Mine))),
    %% a different author's profile does not include our root
    {ok, Other} = ssb_peer:rpc_stream_call(
                    Peer, [~"patchwork", ~"profile", ~"roots"],
                    [{[{~"id", fresh_feed_id()}, {~"limit", 50}]}]),
    ?assertNot(lists:member(RootId, root_keys(Other))),
    gen_server:stop(Peer).

%% mentionsFeed is publicFeed filtered to threads mentioning the owner.
mentions_feed_test(_Config) ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    ok = ssb_feed:post_content(
           OwnPid, {[{~"type", ~"post"}, {~"text", ~"hey @you"},
                     {~"mentions", [{[{~"link", OwnId}]}]}]}),
    #message{id = RootId} = ssb_feed:fetch_last_msg(OwnPid),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, Frames} = ssb_peer:rpc_stream_call(
                     Peer, [~"patchwork", ~"mentionsFeed", ~"roots"],
                     [{[{~"limit", 50}]}]),
    ?assert(lists:member(RootId, root_keys(Frames))),
    gen_server:stop(Peer).

root_keys(Frames) ->
    [proplists:get_value(~"key", P)
     || F <- Frames, {P} <- [utils:nat_decode(F)],
        proplists:is_defined(~"key", P)].

%% publicFeed.latest pushes a root item when a new post creates a
%% thread while the stream is open.
public_feed_latest_test(_Config) ->
    OwnPid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, _Ref} = ssb_peer:open_source(Peer, [~"patchwork", ~"publicFeed", ~"latest"],
                                      [{[]}], self()),
    Marker = base64:encode(crypto:strong_rand_bytes(9)),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"}, {~"text", Marker}]}),
    #message{id = RootId} = ssb_feed:fetch_last_msg(OwnPid),
    ?assert(wait_for_root(RootId, 12)),
    gen_server:stop(Peer).

wait_for_root(_RootId, 0) -> false;
wait_for_root(RootId, N) ->
    receive
        {stream_data, _, Frame} ->
            {Props} = utils:nat_decode(Frame),
            case proplists:get_value(~"key", Props) of
                RootId -> true;
                _      -> wait_for_root(RootId, N - 1)
            end
    after 3000 -> false
    end.

%% suggest.profile matches a self-assigned name by prefix.
suggest_profile_test(_Config) ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    Target = fresh_feed_id(),
    Name = <<"Zaphod", (base64:encode(crypto:strong_rand_bytes(4)))/binary>>,
    %% the owner names the target (an about); the about view indexes it
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"about"},
                                         {~"about", Target}, {~"name", Name}]}),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, Body} = ssb_peer:rpc_call(Peer, [~"patchwork", ~"suggest", ~"profile"],
                                   ~"async", [{[{~"text", ~"Zaphod"}]}]),
    Items = utils:nat_decode(Body),
    Ids = [proplists:get_value(~"id", P) || {P} <- Items],
    ?assert(lists:member(Target, Ids)),
    gen_server:stop(Peer).

%% profile.avatar returns {id, name, image} resolved from the about view.
profile_avatar_test(_Config) ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    Target = fresh_feed_id(),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"about"},
                                         {~"about", Target}, {~"name", ~"Trillian"}]}),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    {ok, Body} = ssb_peer:rpc_call(Peer, [~"patchwork", ~"profile", ~"avatar"],
                                   ~"async", [{[{~"id", Target}]}]),
    {Props} = utils:nat_decode(Body),
    ?assertEqual(Target,      proplists:get_value(~"id", Props)),
    ?assertEqual(~"Trillian", proplists:get_value(~"name", Props)),
    gen_server:stop(Peer).

%% friends.get({source, dest}) returns the follow relationship the
%% owner published: true after following the target over the wire.
friends_get_test(_Config) ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    Target = fresh_feed_id(),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"contact"},
                                         {~"contact", Target},
                                         {~"following", true}]}),
    ok = rpc_refresh_friends(),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    Args = [{[{~"source", OwnId}, {~"dest", Target}]}],
    {ok, Body} = ssb_peer:rpc_call(Peer, [~"friends", ~"get"], ~"async", Args),
    ?assertEqual(true, utils:nat_decode(Body)),
    %% a stranger relationship is null
    StrangerArgs = [{[{~"source", OwnId}, {~"dest", fresh_feed_id()}]}],
    {ok, Body2} = ssb_peer:rpc_call(Peer, [~"friends", ~"get"], ~"async",
                                    StrangerArgs),
    ?assertEqual(null, utils:nat_decode(Body2)),
    gen_server:stop(Peer).

%% friends folds contact messages via the view manager synchronously,
%% so the edge is visible immediately; nothing to wait for.
rpc_refresh_friends() -> ok.

%% about.socialValue resolves a feed's name from the owner's own
%% assignment (yourId priority) over the wire.
about_social_value_test(_Config) ->
    OwnPid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    Target = fresh_feed_id(),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"about"},
                                         {~"about", Target},
                                         {~"name", ~"Bob"}]}),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    Args = [{[{~"dest", Target}, {~"key", ~"name"}]}],
    {ok, Body} = ssb_peer:rpc_call(Peer, [~"about", ~"socialValue"],
                                   ~"async", Args),
    ?assertEqual(~"Bob", utils:nat_decode(Body)),
    gen_server:stop(Peer).

%% about.socialValueStream pushes the updated resolved value when a new
%% about arrives while the stream is open.
live_about_test(_Config) ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    Target = fresh_feed_id(),
    {ok, Peer} = ssb_peer:start_link("localhost", server_pk()),
    Args = [{[{~"dest", Target}, {~"key", ~"name"}]}],
    {ok, _Ref} = ssb_peer:open_source(Peer, [~"about", ~"socialValueStream"],
                                      Args, self()),
    %% drain the initial (null) snapshot value
    receive {stream_data, _, _Initial} -> ok after 2000 -> error(no_snapshot) end,

    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"about"},
                                         {~"about", Target},
                                         {~"name", ~"Carol"}]}),
    receive
        {stream_data, _, Frame} ->
            ?assertEqual(~"Carol", utils:nat_decode(Frame))
    after 3000 ->
        error(no_live_about)
    end,
    gen_server:stop(Peer).

fresh_feed_id() ->
    #{public := Pub} = enacl:sign_keypair(),
    <<"@", (base64:encode(Pub))/binary, ".ed25519">>.

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
