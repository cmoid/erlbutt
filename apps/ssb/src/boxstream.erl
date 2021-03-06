%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2020 Dionne Associates, LLC.
-module(boxstream).

-include("ssb.hrl").

-import(utils, [combine/2,
                incr/1]).

-export([box/3,
         unbox_and_parse/2]).

box(Data, Nonce, SecretBoxKey) ->

    DataSizBin = size(Data),

    EndMsgBox = enacl:secretbox(Data,
                                incr(Nonce),
                                SecretBoxKey),

    <<BodyAuthTag:16/binary,Body/binary>> = EndMsgBox,

    Header = <<DataSizBin:2/big-integer-unit:8,BodyAuthTag:16/binary>>,

    EncHeader =
        enacl:secretbox(Header,Nonce,SecretBoxKey),

    {combine(EncHeader, Body),
     incr(incr(Nonce))}.

unbox(_SecretBoxKey, Nonce,
      DataProc) when size(DataProc) < 34 ->
    {partial, <<>>, Nonce, DataProc};

unbox(SecretBoxKey, Nonce, DataProc) ->

    <<Header:34/binary,RestData/binary>> = DataProc,

    Msg = shs:open_box(Header,
                       Nonce,
                       SecretBoxKey),

    EndBox = Msg == ?BOX_END,

    case EndBox of
        true ->
            ?debug("Ok, that's all folks, just got the BOX END ~p ~n",[Msg]),
            {done, <<>>, Nonce, DataProc};
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

unbox_and_parse(BoxData, #sbox_state{socket=Socket,
                                dec_sbox_key = DecBoxKey,
                                enc_sbox_key = EncBoxKey,
                                dec_nonce = DecNonce,
                                enc_nonce = EncNonce,
                                rpc_rem_bytes = RpcLeftOver} = State) ->
    {Done, Msg, NewDecNonce, NewBoxLeftOver} =
        unbox(DecBoxKey, DecNonce,
                        BoxData),

    %% Should append Msg to rpc_rem_bytes from previous call?
    Parsed = rpc_parse(Done, combine(RpcLeftOver, Msg)),
    {NewRpcLeftOver, NewEncNonce} =
        case Parsed of
            nop ->
                {RpcLeftOver, EncNonce};
            {partial, nil, Rest} ->
                % if partial parse then Rest is the original input
                {Rest, EncNonce};
            {complete, {?RPC_END, <<>>}, Rest} ->
                ?debug("This is the end of the RPC stream ~p ~n",
                       [Rest]),
                {RpcLeftOver, EncNonce};
            {complete, {Header, Body}, Rest} ->
                ProcEncNonce =
                    rpc_processor:process({Header, Body},
                                          #ssb_conn{
                                             socket = Socket,
                                             nonce = EncNonce,
                                             secret_box = EncBoxKey}),
                {Rest, ProcEncNonce}
        end,

    NewState = State#sbox_state{dec_nonce = NewDecNonce,
                      enc_nonce = NewEncNonce,
                      box_rem_bytes = NewBoxLeftOver,
                      rpc_rem_bytes = NewRpcLeftOver},
    if (Done == complete andalso
        size(NewBoxLeftOver) > 34) ->
            unbox_and_parse(NewBoxLeftOver,
                            NewState#sbox_state{box_rem_bytes = <<>>});
       true ->
            {Done, NewState}
    end.

rpc_parse(complete, ?BOX_END) ->
    ?debug("THIS IS THE END OF BOXSTREAM ~n",[]),
    nop;

rpc_parse(complete, Msg) ->
    rpc_parse:parse(Msg);

rpc_parse(partial, _Msg) ->
    nop;

rpc_parse(done, _Msg) ->
    nop.
