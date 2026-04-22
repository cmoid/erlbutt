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

-export([connect_test/1]).

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
    application:stop(ssb),
    _Config.

init_per_testcase(_Case, _Config) ->
    _Config.

end_per_testcase(_, Config) ->
    Config.

all() ->
    [connect_test].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connect_test(Config) ->
    Host = get_value(hostname, Config, "localhost"),
    {ok, NewSbotPeer} = ssb_peer:start_link(Host, remote_long_pk()),
    ?assert(is_process_alive(NewSbotPeer)),
    Config.


remote_long_pk() ->
    base64:decode(keys:pub_key()).
