-module(erlbutt_conn_SUITE).

-export([init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0]).

-export([pub_msg_reaches_conn_db/1,
         conn_db_persists_across_restart/1,
         conn_json_has_correct_format/1,
         announcers_accumulate/1,
         contact_dispatch_no_crash/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ssb/include/ssb.hrl").

-define(PUB_HOST, <<"pub.ct-test.example.com">>).
-define(PUB_PORT, 8008).
-define(PUB_KEY,  <<"@ctpubkey=.ed25519">>).

%%%===================================================================
%%% CT callbacks
%%%===================================================================

init_per_suite(Config) ->
    cover:start(),
    application:ensure_all_started(ssb),
    Config.

end_per_suite(Config) ->
    application:stop(ssb),
    Config.

init_per_testcase(_Case, Config) ->
    %% Clean conn_db state before each test.
    ConnJson = binary_to_list(config:ssb_repo_loc()) ++ "conn.json",
    file:delete(ConnJson),
    %% Restart conn_db with empty state.
    supervisor:terminate_child(ssb_sup, conn_db),
    supervisor:restart_child(ssb_sup, conn_db),
    Config.

end_per_testcase(_Case, Config) ->
    Config.

all() ->
    [pub_msg_reaches_conn_db,
     conn_db_persists_across_restart,
     conn_json_has_correct_format,
     announcers_accumulate,
     contact_dispatch_no_crash].

%%%===================================================================
%%% Tests
%%%===================================================================

%% Posting a type:pub message to our feed triggers dispatch and lands
%% in conn_db — full pipeline test.
pub_msg_reaches_conn_db(_Config) ->
    OurId  = keys:pub_key_disp(),
    FeedPid = utils:find_or_create_feed_pid(OurId),
    ssb_feed:post_content(FeedPid, pub_content()),
    Peers = conn_db:all(),
    ?assert(maps:is_key(expected_addr(), Peers)),
    Peer = maps:get(expected_addr(), Peers),
    ?assertEqual(?PUB_HOST, maps:get(<<"host">>, Peer)),
    ?assertEqual(?PUB_PORT, maps:get(<<"port">>, Peer)),
    ?assertEqual(<<"pub">>,  maps:get(<<"source">>, Peer)),
    ?assert(maps:get(<<"autoconnect">>, Peer)).

%% Stopping and restarting conn_db reloads entries from conn.json.
conn_db_persists_across_restart(_Config) ->
    OurId   = keys:pub_key_disp(),
    FeedPid = utils:find_or_create_feed_pid(OurId),
    ssb_feed:post_content(FeedPid, pub_content()),
    ?assert(maps:is_key(expected_addr(), conn_db:all())),
    conn_db:flush(),
    supervisor:terminate_child(ssb_sup, conn_db),
    supervisor:restart_child(ssb_sup, conn_db),
    ?assert(maps:is_key(expected_addr(), conn_db:all())).

%% conn.json is written with JS-compatible fields.
conn_json_has_correct_format(_Config) ->
    OurId   = keys:pub_key_disp(),
    FeedPid = utils:find_or_create_feed_pid(OurId),
    ssb_feed:post_content(FeedPid, pub_content()),
    conn_db:flush(),
    ConnJson = binary_to_list(config:ssb_repo_loc()) ++ "conn.json",
    {ok, Bin} = file:read_file(ConnJson),
    Map = json:decode(Bin),
    ?assert(is_map(Map)),
    Peer = maps:get(expected_addr(), Map),
    ?assert(is_integer(maps:get(<<"birth">>,       Peer))),
    ?assert(is_integer(maps:get(<<"stateChange">>, Peer))),
    ?assert(is_map(maps:get(<<"duration">>, Peer))),
    ?assert(maps:get(<<"autoconnect">>, Peer)).

%% Two posts announcing the same pub increment the announcers count.
announcers_accumulate(_Config) ->
    OurId   = keys:pub_key_disp(),
    FeedPid = utils:find_or_create_feed_pid(OurId),
    ssb_feed:post_content(FeedPid, pub_content()),
    ssb_feed:post_content(FeedPid, pub_content()),
    Peer = maps:get(expected_addr(), conn_db:all()),
    ?assertEqual(2, maps:get(<<"announcers">>, Peer)).

%% Storing a contact/follow message dispatches without errors.
contact_dispatch_no_crash(_Config) ->
    OurId   = keys:pub_key_disp(),
    FeedPid = utils:find_or_create_feed_pid(OurId),
    Contact = {[{<<"type">>,      <<"contact">>},
                {<<"contact">>,   <<"@other=.ed25519">>},
                {<<"following">>, true}]},
    ?assertEqual(ok, ssb_feed:post_content(FeedPid, Contact)).

%%%===================================================================
%%% Helpers
%%%===================================================================

pub_content() ->
    {[{<<"type">>,    <<"pub">>},
      {<<"address">>, {[{<<"host">>, ?PUB_HOST},
                        {<<"port">>, ?PUB_PORT},
                        {<<"key">>,  ?PUB_KEY}]}}]}.

expected_addr() ->
    <<"net:", ?PUB_HOST/binary, ":8008~shs:ctpubkey=">>.
