%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(boxstream).

-include_lib("ssb/include/ssb.hrl").

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
