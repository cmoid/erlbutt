-module(erlbutt_basic_SUITE).
-export([
         %% suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0
        ]).

-export([basic_connect_test/1]).

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
    [basic_connect_test].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
basic_connect_test(Config) ->
    packet:secret_handshake([]),
    Config.
