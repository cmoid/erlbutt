%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(rpc_processor).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ssb.hrl").

%% API
-export([process/2,
         create_flags/3,
         create_header/3,
         parse_flags/1]).

process({Header, Body}, #ssb_conn{
                           socket = Socket,
                           nonce = Nonce,
                           secret_box = SecretBoxKey}) ->
    ReqNo = req_no(Header),

    ?LOG_DEBUG("Please process ~p ~n from pid: ~p ~n",[{ReqNo, Body}, self()]),

    case ReqNo < 0 of
        %% negative request numbers indicate responses
        true ->
            Resp = proc_response(ReqNo, Body),
            {Nonce, Resp};
        _Else ->
            ReqBod = create_req(Body),
            NewNonce = proc_request(ReqNo, ReqBod, Socket, Nonce, SecretBoxKey),
            {NewNonce, none}
    end.

parse_flags(Header) ->
    <<Flags:1/binary, _Rest/binary>> = Header,
    <<_Unused:4, Stream:1, EndOrError:1, Type:2>> = Flags,
    {Stream, EndOrError, Type}.

create_flags(Stream, EndOrError, Type) ->
    <<0:4, Stream:1, EndOrError:1, Type:2>>.

create_header(Flags, BodySize, ReqNo) ->
    <<Flags:1/binary,
      BodySize:4/big-unsigned-integer-unit:8,
      ReqNo:4/big-signed-integer-unit:8>>.


%%%===================================================================
%%% Internal functions
%%%===================================================================

req_no(Header) ->
    <<Flags:1/binary,
      BodySize:4/binary,
      Req:4/big-signed-integer-unit:8>> = Header,
    Req.

create_req(Body) ->
    DecBody = jiffy:decode(Body),
    IsTuple = is_tuple(DecBody),
    case IsTuple of
        true ->
            {Props} = DecBody,
            #ssb_rpc{
               name = proplists:get_value(<<"name">>, Props),
               args = proplists:get_value(<<"args">>, Props),
               type = proplists:get_value(<<"type">>, Props)};
        _Else ->
            DecBody
    end.

proc_response(ReqNo, RespBody) ->
    RespBody.

proc_request(ReqNo, #ssb_rpc{name = [?createhistorystream],
                             args = [{_Args}]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    % to start return true and close stream
    Flags = create_flags(1,1,2),
    Header = create_header(Flags,size(<<"true">>), -ReqNo),
    utils:send_data(utils:combine(Header,jiffy:encode(true, [pretty])),
                    Socket, Nonce, SecretBoxKey);

proc_request(ReqNo, #ssb_rpc{name = [?gossip, ?ping],
                             args = [{_Args}]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    % to start return true and close stream
    Flags = create_flags(1,0,10),
    TimeStamp = jiffy:encode(integer_to_binary(current_time()), [pretty]),
    Header = create_header(Flags,size(TimeStamp), -ReqNo),
    NewNonce = utils:send_data(utils:combine(Header, TimeStamp), Socket, Nonce, SecretBoxKey),
    NewNonce;
    %%utils:send_data(?BOX_END, Socket, NewNonce, SecretBoxKey);

proc_request(ReqNo, #ssb_rpc{name = [?whoami],
                             args = []}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    % to start return true and close stream
    Flags = create_flags(1,0,10),
    Body = whoami(),
    Header = create_header(Flags, size(Body), -ReqNo),
    Data = utils:combine(Header, Body),
    NewNonce = utils:send_data(Data, Socket, Nonce, SecretBoxKey),
    NewNonce1 = utils:send_data(?RPC_END, Socket, NewNonce, SecretBoxKey),
    utils:send_data(?BOX_END, Socket, NewNonce1, SecretBoxKey);

proc_request(ReqNo, #ssb_rpc{name = [?blobs, <<"createWants">>],
                             args = []}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    % to start return true and close stream
    Flags = create_flags(1,1,2),
    TrueEnd = jiffy:encode(true, [pretty]),
    Header = create_header(Flags,size(TrueEnd), -ReqNo),
    utils:send_data(utils:combine(utils:combine(Header,TrueEnd), ?RPC_END),
                    Socket, Nonce, SecretBoxKey);

proc_request(ReqNo, ReqBody, Socket, Nonce, SecretBoxKey) ->
    Flags = create_flags(1,1,10),
    TrueEnd = jiffy:encode(true, [pretty]),
    Header = create_header(Flags,size(TrueEnd), -ReqNo),
    NewNonce = utils:send_data(utils:combine(Header, TrueEnd),
                               Socket, Nonce, SecretBoxKey),
    utils:send_data(?BOX_END, Socket, NewNonce, SecretBoxKey).

current_time() ->
    erlang:system_time(millisecond).

whoami() ->
    jiffy:encode({[{<<"id">>, keys:pub_key_disp()}]}).

-ifdef(TEST).

body1_test() ->
    Rpc = create_req(jiffy:encode({[{<<"name">>,[<<"gossip">>,<<"ping">>]},
                                    {<<"args">>,[{[{<<"timeout">>,300000}]}]},
                                    {<<"type">>,<<"duplex">>}]})),
    ?assert(Rpc#ssb_rpc.name == [<<"gossip">>,<<"ping">>]),
    ?assert(Rpc#ssb_rpc.type == <<"duplex">>).

body2_test() ->
    Rpc = create_req(jiffy:encode(22222)),
    ?assert(Rpc == 22222).

flags_test() ->
    ?assert(<<10>> == create_flags(1,0,2)),
    ?assert({1,0,2} == parse_flags(<<10>>)),
    ?assert({1,1,2} == parse_flags(<<14>>)).

-endif.
