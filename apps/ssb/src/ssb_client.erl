1%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(ssb_client).

-include("ssb.hrl").

-import(utils, [concat/1,
                combine/2,
                send_data/4]).

-behaviour(gen_server).

%% API
-export([start_link/2, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ip, PubKey) ->
    gen_server:start_link(?MODULE, [Ip, PubKey], []).

send(Pid, Data) ->
    gen_server:call(Pid, {send, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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
                         enc_nonce = EncNonce}}
    catch
        error:Reason ->
            ?debug("Handshake failed, perhaps server is afraid of Corona beer ~p ~n",
                   [Reason]),
            {stop, Reason}
    end.

handle_call({send, Data}, _From,
            #sbox_state{socket = Socket,
                   enc_sbox_key = EncBoxKey,
                   dec_nonce = _ServerNonce,
                   enc_nonce = EncNonce} = State) ->
    NewEncNonce = send_data(Data, Socket, EncNonce, EncBoxKey),
    NewState = process(State),
    %%
    {reply, NewState#sbox_state.response,
     NewState#sbox_state{enc_nonce = NewEncNonce}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

process(#sbox_state{socket = Socket,
                   box_rem_bytes = BoxLeftOver}=State) ->
    DataRead = gen_tcp:recv(Socket, 0, 3000),

    case DataRead of
        {ok, Data} ->
            BoxData = combine(BoxLeftOver, Data),

            {Done, NewState} =
                boxstream:unbox_and_parse(BoxData, State),

            ?debug("The box returns ~p ~n",[{Done, NewState}]),

            case Done of
                complete ->
                    ranch_tcp:setopts(Socket, [{active, once}]),
                    %% need the response here
                    NewState;
                _Else ->
                    %% recursive call, keep processing until complete
                    process(NewState)
            end;
        {error, Reason} ->
            ?debug("nothing to read now? ~p ~n",[Reason]),
            State#sbox_state{response = Reason}
    end.

handle_info({tcp_closed, _Socket}, State) ->
    network_error(normal, State);

handle_info({tcp_error, _, Reason}, State) ->
    network_error(Reason, State);

%% a catchall that should not obtain?
handle_info({tcp, Socket, Data},
            #sbox_state{socket=Socket,
                   transport=Transport,
                   box_rem_bytes = BoxLeftOver} = State) ->
    utils:log({Socket, Data}),

    % combine new data with left overs from previous packets
    BoxData = combine(BoxLeftOver, Data),
    ?debug("Client needs to process data ~p ~n",[BoxData]),

    {Done, NewState} = boxstream:unbox_and_parse(BoxData, State),

    case Done of
        done ->
            stop(done, NewState);
        _Else ->
            Transport:setopts(Socket, [{active, once}]),
            {noreply, NewState}
    end;

handle_info(Info, State) ->
    ?debug("Random info request ~p ~n",[Info]),
    utils:log(Info),
    {noreply, State}.


stop(Reason, State) ->
    {stop, Reason, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
network_error(Reason, State) ->
    ?debug("Network error ~p ~n",[Reason]),
    stop({shutdown, conn_closed}, State).

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
