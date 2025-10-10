-module(erlbutt_basic_SUITE).
-export([
         %% suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0
        ]).

-import(proplists, [get_value/2,
                    get_value/3]).

-export([connect_test/1,
         ping_test/1,
         whoami_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ssb/include/ssb.hrl").

%% ===================================================================
%% common_test callbacks
%% ===================================================================
init_per_suite(_Config) ->
    cover:start(),
    application:ensure_all_started(ssb),
    _Config.

end_per_suite(_Config) ->
    _Config.

init_per_testcase(_Case, _Config) ->
    _Config.

end_per_testcase(_, Config) ->
    Config.

all() ->
    [connect_test
     %%ping_test,
     %%whoami_test
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connect_test(Config) ->
    Host = get_value(hostname, Config, "localhost"),
    {ok, NewSbotPeer} = ssb_peer:start_link(Host, remote_long_pk()),
    ?assert(is_process_alive(NewSbotPeer)),
    Config.


ping_test(Config) ->
    Host = get_value(hostname, Config, "localhost"),
    {ok, NewSbotPeer} = ssb_peer:start_link(Host, remote_long_pk()),
    Now = erlang:system_time(millisecond),
    %% This send call is no longer synchronous but something like it needs to exist
    Time = ssb_peer:send(NewSbotPeer, ping()),
    End = binary_to_integer(message:nat_decode(Time)),
    ?assert((End - Now) < 5),
    Config.

whoami_test(Config) ->
    Host = get_value(hostname, Config, "localhost"),
    {ok, NewSbotClient} = ssb_peer:start_link(Host, remote_long_pk()),
    {WhoAmI} = utils:nat_decode(ssb_peer:send(NewSbotClient, whoami_req())),
    ?assert(keys:pub_key_disp() == ?pgv(<<"id">>, WhoAmI)),
    Config.


remote_long_pk() ->
    base64:decode(keys:pub_key()).

ping() ->
    Flags = rpc_processor:create_flags(1,0,2),
    Body = utils:encode_rec({[{<<"name">>,[<<"gossip">>,<<"ping">>]},
                          {<<"args">>,[{[{<<"timeout">>, 300000}]}]},
                          {<<"type">>,<<"duplex">>}]}),
    Header = rpc_processor:create_header(Flags, size(Body), 1),
    utils:combine(Header, Body).

whoami_req() ->
    Flags = rpc_processor:create_flags(1,0,2),
    Body = iolist_to_binary(message:ssb_encoder({[{<<"name">>,[?whoami]},
                          {<<"args">>,[]},
                          {<<"type">>,<<"async">>}]}, fun message:ssb_encoder/3, [])),
    Header = rpc_processor:create_header(Flags, size(Body), 1),
    utils:combine(Header, Body).
