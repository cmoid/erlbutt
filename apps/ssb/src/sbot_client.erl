%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2018 Dionne Associates, LLC.
-module(sbot_client).

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
    gen_server:cast(Pid, {send, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Ip, PubKey]) ->
    process_flag(trap_exit, true),
    try
        {ok, {Socket, DecBoxKey, DecNonce, EncBoxKey, EncNonce}} =
            shs:client_shake_hands(connect(Ip, 8008), PubKey),
        ranch_tcp:setopts(Socket, [{active, once}]),
        {ok, #sbox_state{socket = Socket,
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

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% casts

handle_cast({send, Data},
            #sbox_state{socket = Socket,
                   enc_sbox_key = EncBoxKey,
                   dec_nonce = _ServerNonce,
                   enc_nonce = EncNonce} = State) ->
    NewEncNonce = send_data(Data, Socket, EncNonce, EncBoxKey),
    {noreply, State#sbox_state{enc_nonce = NewEncNonce}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% info
handle_info({tcp, Socket, Data},
            #sbox_state{socket = Socket,
                   box_rem_bytes = BoxLeftOver}=State) ->
    utils:log({Socket, Data}),

    BoxData = combine(BoxLeftOver, Data),

    {Done, NewState} = boxstream:unbox_and_parse(BoxData, State),

    case Done of
        done ->
            stop(done, NewState);
        _Else ->
            ranch_tcp:setopts(Socket, [{active, once}]),
            {noreply, NewState}
    end;

handle_info(Info, State) ->
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
