%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(boxstream).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-compile({no_auto_import,[size/1]}).
-import(utils, [combine/2,
                incr/1,
                size/1]).

-export([box/3,
         unbox/3]).

box(?BOX_END, Nonce, SecretBoxKey) ->
    EndMsgBox = enacl:secretbox(?BOX_END,
                                Nonce,
                                SecretBoxKey),
    {EndMsgBox, incr(Nonce)};

box(Data, Nonce, SecretBoxKey) ->

    DataSizBin = size(Data),

    EndMsgBox = enacl:secretbox(Data,
                                incr(Nonce),
                                SecretBoxKey),

    <<BodyAuthTag:16/binary,Body/binary>> = EndMsgBox,

    Header = <<DataSizBin:2/big-integer-unit:8,
               BodyAuthTag:16/binary>>,

    EncHeader =
        enacl:secretbox(Header,Nonce,SecretBoxKey),

    {combine(EncHeader, Body),
     incr(incr(Nonce))}.

%% return complete or partial, no need for returning done. Complete
%% with ?BOX_END indicates done
unbox(_SecretBoxKey, Nonce, DataProc) when byte_size(DataProc) < 34 ->
    {partial, <<>>, Nonce, DataProc};

unbox(SecretBoxKey, Nonce, DataProc) ->

    <<Header:34/binary,RestData/binary>> = DataProc,

    Msg = shs:open_box(Header,
                       Nonce,
                       SecretBoxKey),

    EndBox = Msg == ?BOX_END,

    case EndBox of
        true ->
            {complete, ?BOX_END, Nonce, <<>>};
        _Else ->
            <<Len:2/binary,Rest:16/binary>> = Msg,

            LenInt = binary:decode_unsigned(Len),

            AlreadyHaveBytes = LenInt > 0 andalso
                LenInt =< size(RestData),

            if AlreadyHaveBytes ->

                    <<_DontCare:LenInt/binary,RemBytes/binary>> = RestData,
                    FullPacket = <<Rest:16/binary, RestData:LenInt/binary>>,
                    {complete,
                     shs:open_box(FullPacket,
                                  incr(Nonce),
                                  SecretBoxKey),
                     incr(incr(Nonce)),
                     RemBytes};
               true ->
                    {partial,
                     <<>>,
                     Nonce,
                     DataProc}
            end
    end.

-ifdef(TEST).

test_key()   -> crypto:strong_rand_bytes(32).
test_nonce() -> crypto:strong_rand_bytes(24).

%% box then unbox returns the original data, and both advance the nonce by 2.
box_unbox_roundtrip_test() ->
    Key   = test_key(),
    Nonce = test_nonce(),
    Data  = ~"hello boxstream",
    {Boxed, NewNonce} = box(Data, Nonce, Key),
    {complete, Data, NewNonce, <<>>} = unbox(Key, Nonce, Boxed).

%% boxing BOX_END produces a 34-byte frame; unboxing it signals end-of-stream.
box_end_test() ->
    Key   = test_key(),
    Nonce = test_nonce(),
    {Boxed, _} = box(?BOX_END, Nonce, Key),
    ?assert(byte_size(Boxed) =:= 34),
    {complete, ?BOX_END, Nonce, <<>>} = unbox(Key, Nonce, Boxed).

%% fewer than 34 bytes is always partial — not enough to decode the header.
partial_too_short_test() ->
    Key   = test_key(),
    Nonce = test_nonce(),
    Short = <<1, 2, 3>>,
    {partial, <<>>, Nonce, Short} = unbox(Key, Nonce, Short).

%% exactly the 34-byte header with no body bytes is also partial.
partial_header_only_test() ->
    Key   = test_key(),
    Nonce = test_nonce(),
    Data  = ~"some payload",
    {Boxed, _} = box(Data, Nonce, Key),
    <<Header:34/binary, _Body/binary>> = Boxed,
    {partial, <<>>, Nonce, Header} = unbox(Key, Nonce, Header).

%% two frames concatenated: unbox the first, then unbox the remainder.
multi_chunk_test() ->
    Key    = test_key(),
    Nonce  = test_nonce(),
    Data1  = ~"first chunk",
    Data2  = ~"second chunk",
    {Boxed1, Nonce1} = box(Data1, Nonce,  Key),
    {Boxed2, Nonce2} = box(Data2, Nonce1, Key),
    Combined = <<Boxed1/binary, Boxed2/binary>>,
    {complete, Data1, Nonce1, Rem}  = unbox(Key, Nonce,  Combined),
    {complete, Data2, Nonce2, <<>>} = unbox(Key, Nonce1, Rem).

%% nonce advances by 2 for regular data, by 1 for BOX_END.
nonce_advance_test() ->
    Key   = test_key(),
    Nonce = test_nonce(),
    {_, AfterData}   = box(~"x", Nonce, Key),
    {_, AfterBoxEnd} = box(?BOX_END, Nonce, Key),
    ?assert(AfterData    =:= incr(incr(Nonce))),
    ?assert(AfterBoxEnd  =:= incr(Nonce)).

-endif.
