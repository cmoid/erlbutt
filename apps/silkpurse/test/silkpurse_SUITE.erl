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
    ?assert(lists:member(RootId, Keys)),   %% root present
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
