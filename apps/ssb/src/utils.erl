%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2018 Dionne Associates, LLC.
-module(utils).
-include("ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([concat/1,
         incr/1,
         combine/2,
         send_data/4,
         log/1]).

concat(ListOfBins) ->
    iolist_to_binary(ListOfBins).

incr(Nonce) ->
    binary:encode_unsigned(binary:decode_unsigned(Nonce) + 1).

combine(nil, Bin) ->
    Bin;

combine(Bin1, Bin2) ->
    Bin1Len = size(Bin1),
    <<Bin1:Bin1Len/binary, Bin2/binary>>.

send_data(Data, Socket, Nonce, SecretBoxKey) ->

    {EncBox, NewNonce} =
        boxstream:box(Data, Nonce, SecretBoxKey),

    gen_tcp:send(Socket, EncBox),

    NewNonce.

log({Socket, Data}) ->
    ?debug("received a tcp packet of size ~p ~n on socket ~p ~n",
           [size(Data), Socket]);

log(Info) ->
    ?debug("received random info message ~p ~n",
           [Info]).


-ifdef(TEST).

combine_test() ->
    <<"foo">> = combine(nil, <<"foo">>),
    <<"foobar">> = combine(<<"foo">>,<<"bar">>).

-endif.
