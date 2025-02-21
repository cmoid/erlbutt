%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2025 Charles Moid
-module(ebt_vc).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([decode_clock_int/1,
         decode_clock/1]).

decode_clock_int(Int) ->
    if Int < 0 ->
            {false, false, 0};
       true ->
            {true, (Int band 1) == 0, Int bsr 1}
    end.

decode_clock(ClockList) ->
    lists:map(fun({Feed, Num}) ->
                      {Feed, decode_clock_int(Num)}
              end, ClockList).

-ifdef(TEST).

%% test table in protocol guide

row1_test() ->
    {false,false,0} == ebt_vc:decode_clock_int(-1).

row2_test() ->
    {true,true,0} == ebt_vc:decode_clock_int(0).

row3_test() ->
    {true,false,0} == ebt_vc:decode_clock_int(1).

row4_test() ->
    {true,true,1} == ebt_vc:decode_clock_int(2).

row5_test() ->
    {true,false,1} == ebt_vc:decode_clock_int(3).

row6_test() ->
    {true,true,6} == ebt_vc:decode_clock_int(12).

row7_test() ->
    {true,true,225} == ebt_vc:decode_clock_int(450).

simple_vec_clock1_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "vec_clock1",
    {ok, Vec_Clock} = file:read_file(F),
    {VClock} = utils:nat_decode(Vec_Clock),
    ?assert(is_list(VClock)).

simple_vec_clock2_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "vec_clock1",
    {ok, Vec_Clock} = file:read_file(F),
    {VClock} = utils:nat_decode(Vec_Clock),
    ?assert(is_list(VClock)),
    {Feed, Num} = hd(VClock),
    ?assert(Feed == <<"@qK93G/R9R5J2fiqK+kxV72HqqPUcss+rth8rACcYr4s=.ed25519">>),
    ?assert({true,true,225} == ebt_vc:decode_clock_int(Num)).

simple_vec_clock3_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "vec_clock1",
    {ok, Vec_Clock} = file:read_file(F),
    {VClock} = utils:nat_decode(Vec_Clock),
    ?assert(is_list(VClock)),
    {Feed, Num} = hd(VClock),
    ?assert(Feed == <<"@qK93G/R9R5J2fiqK+kxV72HqqPUcss+rth8rACcYr4s=.ed25519">>),
    ?assert({true,true,225} == ebt_vc:decode_clock_int(Num)),

    Dec_VClock = ebt_vc:decode_clock(VClock),
    {Feed1, Num1} = hd(Dec_VClock),
    ?assert(Feed1 == <<"@qK93G/R9R5J2fiqK+kxV72HqqPUcss+rth8rACcYr4s=.ed25519">>),
    ?assert({true,true,225} == Num1).





-endif.
