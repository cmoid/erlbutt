%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(ssb_peer).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("ssb.hrl").

-export([start_link/2,
         start_link/4,
         send/2]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2,
         code_change/3]).

-import(utils, [concat/1,
                combine/2,
                send_data/4]).

start_link(Ip, PubKey) ->
    gen_server:start_link(?MODULE, [Ip, PubKey], []).

send(Pid, Data) ->
    gen_server:call(Pid, {send, Data}).

start_link(Ref, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).

init([Ip, PubKey]) ->
    process_flag(trap_exit, true),
    try
        {ok, {Socket, DecBoxKey, DecNonce, EncBoxKey, EncNonce}} =
            shs:client_shake_hands(connect(Ip, 8008), PubKey),
        %%ranch_tcp:setopts(Socket, [{active, once}]),
        {ok, #sbox_state{socket = Socket,
                         transport = ranch_tcp,
                         dec_sbox_key = DecBoxKey,
                         enc_sbox_key = EncBoxKey,
                         dec_nonce = DecNonce,
                         enc_nonce = EncNonce,
                         rpc_procs = ets:new(rpc_procs, [])}}
    catch
        error:Reason ->
            ?LOG_DEBUG("Handshake failed, perhaps server is afraid of Corona beer ~p ~n",
                   [Reason]),
            {stop, Reason}
    end;



init([Ref, Socket, Transport, _Opts = []]) ->
    %% note the return of a 0 timeout, this is required to notify
    %% ranch that it owns the socket, and start_link doesn't return
    %% until init does.
    {ok, #sbox_state{ref = Ref,
                socket = Socket,
                transport = Transport,
                rpc_procs = ets:new(rpc_procs, [])}, 0}.

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

handle_call({send, Data}, _From,
            #sbox_state{socket = Socket,
                   enc_sbox_key = EncBoxKey,
                   dec_nonce = _ServerNonce,
                   enc_nonce = EncNonce} = State) ->
    NewEncNonce = send_data(Data, Socket, EncNonce, EncBoxKey),
    NewState = process(State),
    %%
    ?LOG_DEBUG("Sent data and received: ~p ~n",[NewState#sbox_state.response]),
    {reply, NewState#sbox_state.response,
     NewState#sbox_state{enc_nonce = NewEncNonce}};



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

process(#sbox_state{socket = Socket,
                   box_rem_bytes = BoxLeftOver}=State) ->
    DataRead = gen_tcp:recv(Socket, 0, 3000),

    case DataRead of
        {ok, Data} ->
            BoxData = combine(BoxLeftOver, Data),

            {Done, NewState} =
                unbox_and_parse(BoxData, State),

            case Done of
                complete ->
                    %%ranch_tcp:setopts(Socket, [{active, once}]),
                    %% need the response here
                    ?LOG_DEBUG("Client is done sending ~p ~n",[NewState#sbox_state.response]),
                    NewState;
                done ->
                    stop(done, NewState);
                _Else ->
                    %% recursive call, keep processing until complete
                    process(NewState)
            end;
        {error, Reason} ->
            ?LOG_DEBUG("nothing to read now? ~p ~n",[Reason]),
            State#sbox_state{response = Reason}
    end.

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
                    {done, NewState};
                false ->
                    %% now parse rpc
                    NewState2 = rpc_parse(combine(RpcLeftOver, Msg), NewState),
                    if (size(NewBoxLeftOver) > 34) ->
                            unbox_and_parse(NewBoxLeftOver, NewState2);
                       true ->
                            ?LOG_DEBUG("complete or no more to process ~p ~n",
                                       [{complete, NewBoxLeftOver}]),
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
    {NewRpcLeftOver, NewEncNonce, NewResponse} =
        case Parsed of
            {partial, nil, Rest} ->
                % if partial parse then Rest is the original input
                {Rest, EncNonce, Response};
            {complete, ?RPC_END, <<>>} ->
                {<<>>, EncNonce, Response};
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

    NewState = State#sbox_state{enc_nonce = NewEncNonce,
                                rpc_rem_bytes = NewRpcLeftOver,
                                response = NewResponse},
    if (size(NewRpcLeftOver) >= 9) ->
            %% parse some more
            rpc_parse(NewRpcLeftOver, NewState#sbox_state{
                                        rpc_rem_bytes = <<>>});
       true ->
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
