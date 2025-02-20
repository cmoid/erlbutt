%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2025 Charles Moid
-module(ebt_vc).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([decode_clock_int/1]).

decode_clock_int(Int) ->
    if Int < 0 ->
            {false, false, 0};
       true ->
            {true, (Int band 1) == 0, Int bsr 1}
    end.

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

-endif.
