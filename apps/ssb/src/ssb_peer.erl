%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(ssb_peer).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-include_lib("ssb/include/ssb.hrl").

-export([start_link/2,
         start_link/4,
         send/2]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2,
         code_change/3]).

-compile({no_auto_import,[size/1]}).
-import(utils, [concat/1,
                combine/2,
                send_data/4,
                size/1]).

%% connect to another peer.
start_link(Ip, PubKey) ->
    gen_server:start_link(?MODULE, [Ip, PubKey], []).

%% send data to another peer, asynchronously
send(Pid, Data) ->
    gen_server:cast(Pid, {send, Data}).

%% accept a connection from another peer.
start_link(Ref, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).

init([Ip, PubKey]) ->
    process_flag(trap_exit, true),
    try
        {ok, {Socket, DecBoxKey, DecNonce, EncBoxKey, EncNonce}} =
            shs:client_shake_hands(connect(Ip, 8008), PubKey),
        ranch_tcp:setopts(Socket, [{active, false}]),
        {ok, #sbox_state{socket = Socket,
                         transport = ranch_tcp,
                         dec_sbox_key = DecBoxKey,
                         enc_sbox_key = EncBoxKey,
                         dec_nonce = DecNonce,
                         enc_nonce = EncNonce,
                         shook_hands = 1}}
    catch
        error:Reason ->
            {stop, Reason}
    end;

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
            ?LOG_DEBUG("Are we complete and need to wait? ~p ~n",[Done]),
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

handle_cast({send, Data}, #sbox_state{socket = Socket,
                                      transport = Transport,
                                      enc_sbox_key = EncBoxKey,
                                      dec_nonce = _ServerNonce,
                                      enc_nonce = EncNonce} = State) ->
    NewEncNonce = send_data(Data, Socket, EncNonce, EncBoxKey),
    Transport:setopts(Socket, [{active, once}]),
    {noreply, State#sbox_state{enc_nonce = NewEncNonce}};

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

unbox_and_parse(BoxData, #sbox_state{dec_sbox_key = DecBoxKey,
                                dec_nonce = DecNonce,
                                rpc_rem_bytes = RpcLeftOver} = State) ->
    {Status, Msg, NewDecNonce, NewBoxLeftOver} =
        boxstream:unbox(DecBoxKey, DecNonce,
                        BoxData),

    NewState = State#sbox_state{dec_nonce = NewDecNonce,
                                box_rem_bytes = NewBoxLeftOver},

    case Status of
        partial ->
            {partial, NewState};
        complete ->
            Done = Msg == ?BOX_END,
            case Done of
                true ->
                    ?LOG_DEBUG("Box end received ~p ~n",[Msg]),
                    {done, NewState};
                false ->
                    %% now parse rpc
                    NewState2 = rpc_parse(combine(RpcLeftOver, Msg), NewState),
                    case size(NewBoxLeftOver) > 34 of
                        true ->
                            unbox_and_parse(NewBoxLeftOver, NewState2);
                        false ->
                            {complete, NewState2}
                    end
            end
    end.

rpc_parse(Data, #sbox_state{socket = Socket,
                            enc_nonce = EncNonce,
                            enc_sbox_key = EncBoxKey,
                            response = Response} = State) ->

    %% Should append Msg to rpc_rem_bytes from previous call?
    Parsed = rpc_parse:parse(Data),
    {Status, NewRpcLeftOver, NewEncNonce, NewResponse} =
        case Parsed of
            {partial, nil, Rest} ->
                                                % if partial parse then Rest is the original input
                {partial, Rest, EncNonce, Response};
            {complete, ?RPC_END, <<>>} ->
                {complete, <<>>, EncNonce, Response};
            {complete, {Header, Body}, Rest} ->
                %% Need to track request here somehow
                {ProcEncNonce, Resp} =
                    rpc_processor:process({Header, Body},
                                          #ssb_conn{
                                             socket = Socket,
                                             nonce = EncNonce,
                                             secret_box = EncBoxKey}),
                ?LOG_DEBUG("The rpc call returned ~p ~n",[Resp]),
                {complete, Rest, ProcEncNonce, Resp}
        end,

    NewState = State#sbox_state{enc_nonce = NewEncNonce,
                                rpc_rem_bytes = NewRpcLeftOver,
                                response = NewResponse},
    case {size(NewRpcLeftOver) >= 9, Status} of
        {true, complete} ->
            %% parse some more
            rpc_parse(NewRpcLeftOver, NewState#sbox_state{
                                        rpc_rem_bytes = <<>>});
        _ ->
            NewState
    end.

network_error(Reason, State) ->
    ?LOG_ERROR("Network error ~p ~n",[Reason]),
    stop({shutdown, conn_closed}, State).

stop(Reason, State) ->
    {stop, Reason, State}.

connect(Host, Port) ->
    {ok, Socket} =
        gen_tcp:connect(Host, Port,
                        [binary,
                         {reuseaddr, true},
                         {active, false},
                         {packet, raw}
                        ],
                        5000),
    Socket.
