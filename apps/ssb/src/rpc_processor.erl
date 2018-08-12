%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2018 Dionne Associates, LLC.
-module(rpc_processor).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-include("ssb.hrl").

%% API
-export([start_link/0,
         process/3,
         create_flags/3,
         create_header/3,
         parse_flags/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).
%%%===================================================================
%%% API
%%%===================================================================
process( RpcPid, {_Header, _Body} = Msg, SsbConn ) ->
    gen_server:call(RpcPid, {process, Msg, SsbConn}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).


%%%===================================================================
%%% gen_server boilerplate
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({process, {Header, Body}, #ssb_conn{
                                        socket = Socket,
                                        nonce = Nonce,
                                        secret_box = SecretBoxKey}},
            _From,
            #state{} = State) ->

    ReqNo = req_no(Header),

    ?debug("Please process ~p ~n from pid: ~p ~n",[{ReqNo, Body}, self()]),

    case ReqNo < 0 of
        true ->
            proc_response(ReqNo, Body),
            {reply, Nonce, State#state{}};
        _Else ->
            ReqBod = create_req(Body),
            NewNonce = proc_request(ReqNo, ReqBod, Socket, Nonce, SecretBoxKey),
            {reply, NewNonce, State#state{}}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

req_no(Header) ->
    <<_Flags:1/binary,
      _BodySize:4/binary,
      Req:4/big-signed-integer-unit:8>> = Header,
    Req.

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

create_req(Body) ->
    DecBody = jiffy:decode(Body),
    IsTuple = is_tuple(DecBody),
    case IsTuple of
        true ->
            {Props} = DecBody,
            Name = proplists:get_value(<<"name">>, Props),
            Args = proplists:get_value(<<"args">>, Props),
            Type = proplists:get_value(<<"type">>, Props),
            #ssb_rpc{
               name = Name,
               args = Args,
               type = Type};
        _Else ->
            DecBody
    end.

proc_response(ReqNo, RespBody) ->
    ?debug("Please process response ~p ~n", [{ReqNo, RespBody}]).

proc_request(ReqNo, #ssb_rpc{name = [?createhistorystream],
                             args = [{Args}]}
             = ReqBody, Socket, Nonce, SecretBoxKey) ->
    % to start return true and close stream
    Flags = create_flags(1,1,2),
    Header = create_header(Flags,size(<<"true">>), -ReqNo),
    utils:send_data(utils:combine(Header,jiffy:encode(true, [pretty])),
                    Socket, Nonce, SecretBoxKey);

proc_request(ReqNo, #ssb_rpc{name = [?gossip, ?ping],
                             args = [{Args}]}
             = ReqBody, Socket, Nonce, SecretBoxKey) ->
    % to start return true and close stream
    Flags = create_flags(1,0,10),
    TimeStamp = jiffy:encode(integer_to_binary(current_time()), [pretty]),
    Header = create_header(Flags,size(TimeStamp), -ReqNo),
    NewNonce = utils:send_data(utils:combine(Header, TimeStamp), Socket, Nonce, SecretBoxKey),
    utils:send_data(?BOX_END, Socket, NewNonce, SecretBoxKey);

proc_request(ReqNo, #ssb_rpc{name = [?whoami],
                             args = []}
             = ReqBody, Socket, Nonce, SecretBoxKey) ->
    % to start return true and close stream
    Flags = create_flags(1,0,10),
    Body = whoami(),
    Header = create_header(Flags, size(Body), -ReqNo),
    NewNonce = utils:send_data(utils:combine(Header, Body), Socket, Nonce, SecretBoxKey),
    utils:send_data(?BOX_END, Socket, NewNonce, SecretBoxKey);

proc_request(ReqNo, #ssb_rpc{name = [?blobs, <<"createWants">>],
                             args = []}
             = ReqBody, Socket, Nonce, SecretBoxKey) ->
    % to start return true and close stream
    Flags = create_flags(1,1,2),
    TrueEnd = jiffy:encode(true, [pretty]),
    Header = create_header(Flags,size(TrueEnd), -ReqNo),
    NewNonce = utils:send_data(utils:combine(Header,TrueEnd),
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
