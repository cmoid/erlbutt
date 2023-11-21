%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(ssb_peer).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("ssb.hrl").

%% API for ranch protocol
-export([start_link/4,
         unbox_and_parse/2]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2,
         code_change/3]).

-import(utils, [concat/1,
                combine/2]).

start_link(Ref, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).

init([Ref, Socket, Transport, _Opts = []]) ->
    %% note the return of a 0 timeout, this is required to notify
    %% ranch that it owns the socket, and start_link doesn't return
    %% until init does.
    {ok, #sbox_state{ref = Ref,
                socket = Socket,
                transport = Transport}, 0}.

handle_info({tcp, Socket, Data},
            #sbox_state{socket=Socket,
                   transport=Transport,
                   shook_hands = 0} = State) ->
    try
        {ok, {DecBoxKey, DecNonce, EncBoxKey, EncNonce}}
            = shs:server_shake_hands(Data, Socket, Transport),
        Transport:setopts(Socket, [{active, once}]),
        {noreply, State#sbox_state{ dec_sbox_key = DecBoxKey,
                               enc_sbox_key = EncBoxKey,
                               dec_nonce = DecNonce,
                               enc_nonce = EncNonce,
                               shook_hands = 1}}
    catch
        error:Reason ->
            ?LOG_ERROR("Unable to shake hands with stranger ~p ~n",
                   [Reason]),
            {stop, Reason}
    end;

%% This function is called after the handshake is complete
%% and we can begin to process secret box messages
handle_info({tcp, Socket, Data},
            #sbox_state{socket=Socket,
                   transport=Transport,
                   box_rem_bytes = BoxLeftOver} = State) ->

    % combine new data with left overs from previous packets
    BoxData = combine(BoxLeftOver, Data),

    {Done, NewState} = unbox_and_parse(BoxData, State),

    case Done of
        done ->
            stop(done, NewState);
        _Else ->
            Transport:setopts(Socket, [{active, once}]),
            {noreply, NewState}
    end;

handle_info({tcp_closed, _Socket}, State) ->
    network_error(normal, State);

handle_info({tcp_error, _, Reason}, State) ->
    network_error(Reason, State);

handle_info(timeout, #sbox_state{ref = Ref,
                            socket = Socket,
                            transport = Transport} = State) ->
    %% notify ranch that it owns the socket
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info(Info, State) ->
    ?LOG_INFO("Stopped presumably for normal reason: ~p ~n",[Info]),
    {stop, normal, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

unbox_and_parse(BoxData, #sbox_state{socket=Socket,
                                dec_sbox_key = DecBoxKey,
                                enc_sbox_key = EncBoxKey,
                                dec_nonce = DecNonce,
                                enc_nonce = EncNonce,
                                rpc_rem_bytes = RpcLeftOver,
                                response = Response} = State) ->
    {Done, Msg, NewDecNonce, NewBoxLeftOver} =
        boxstream:unbox(DecBoxKey, DecNonce,
                        BoxData),

    %% Should append Msg to rpc_rem_bytes from previous call?
    Parsed = rpc_parse:parse(Done, combine(RpcLeftOver, Msg)),
    {NewRpcLeftOver, NewEncNonce, NewResponse} =
        case Parsed of
            nop ->
                {RpcLeftOver, EncNonce, Response};
            {partial, nil, Rest} ->
                % if partial parse then Rest is the original input
                {Rest, EncNonce, Response};
            {complete, {?RPC_END, <<>>}, _Rest} ->
                {RpcLeftOver, EncNonce, Response};
            {complete, {Header, Body}, Rest} ->
                %% Need to track request here somehow
                {ProcEncNonce, Resp} =
                    rpc_processor:process({Header, Body},
                                          #ssb_conn{
                                             socket = Socket,
                                             nonce = EncNonce,
                                             secret_box = EncBoxKey}),
                {Rest, ProcEncNonce, Resp}
        end,

    NewState = State#sbox_state{dec_nonce = NewDecNonce,
                      enc_nonce = NewEncNonce,
                      box_rem_bytes = NewBoxLeftOver,
                      rpc_rem_bytes = NewRpcLeftOver,
                      response = NewResponse},
    if (Done == complete andalso
        size(NewBoxLeftOver) > 34) ->
            unbox_and_parse(NewBoxLeftOver,
                            NewState#sbox_state{box_rem_bytes = <<>>});
       true ->
            ?LOG_DEBUG("complete or no more to process ~p ~n",[{NewRpcLeftOver, NewBoxLeftOver, Done}]),
            {Done, NewState}
    end.


network_error(Reason, State) ->
    ?LOG_ERROR("Network error ~p ~n",[Reason]),
    stop({shutdown, conn_closed}, State).

stop(Reason, State) ->
    {stop, Reason, State}.
