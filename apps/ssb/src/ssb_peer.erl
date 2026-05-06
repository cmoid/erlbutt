%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(ssb_peer).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-include_lib("ssb/include/ssb.hrl").

-export([start_link/2,
         start_link/3,
         start_link/4,
         start/2,
         start/3,
         send/2,
         request_blob_wants/2]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2,
         code_change/3]).

-compile({no_auto_import,[size/1]}).
-import(utils, [concat/1,
                combine/2,
                send_data/4,
                size/1]).

%% connect to another peer on the default port (ssb app env or 8008).
start_link(Ip, PubKey) ->
    gen_server:start_link(?MODULE, [Ip, PubKey], []).

%% connect to another peer on an explicit port.
start_link(Ip, Port, PubKey) when is_integer(Port) ->
    gen_server:start_link(?MODULE, [Ip, Port, PubKey], []).

%% Unlinked variants — use when the caller is a temporary process (e.g. rpc:call).
start(Ip, PubKey) ->
    gen_server:start(?MODULE, [Ip, PubKey], []).

start(Ip, Port, PubKey) when is_integer(Port) ->
    gen_server:start(?MODULE, [Ip, Port, PubKey], []).

%% send data to another peer, asynchronously
send(Pid, Data) ->
    gen_server:cast(Pid, {send, Data}).

%% accept a connection from another peer.
start_link(Ref, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).

init([Ip, PubKey]) ->
    Port = application:get_env(ssb, port, 8008),
    init([Ip, Port, PubKey]);

init([Ip, Port, PubKey]) ->
    process_flag(trap_exit, true),
    try
        {ok, {Socket, DecBoxKey, DecNonce, EncBoxKey, EncNonce}} =
            shs:client_shake_hands(connect(Ip, Port), PubKey),
        ranch_tcp:setopts(Socket, [{active, false}]),
        {ok, RpcProc} = rpc_processor:start_link(),
        NewEncNonce = initiate_ebt(Socket, EncBoxKey, EncNonce, PubKey),
        ok = rpc_processor:register_stream(RpcProc, -1, ebt),
        ranch_tcp:setopts(Socket, [{active, once}]),
        {ok, #sbox_state{socket = Socket,
                         transport = ranch_tcp,
                         dec_sbox_key = DecBoxKey,
                         enc_sbox_key = EncBoxKey,
                         dec_nonce = DecNonce,
                         enc_nonce = NewEncNonce,
                         rpc_proc = RpcProc,
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
        {ok, RpcProc} = rpc_processor:start_link(),
        Transport:setopts(Socket, [{active, once}]),
        {noreply, State#sbox_state{ dec_sbox_key = DecBoxKey,
                               enc_sbox_key = EncBoxKey,
                               dec_nonce = DecNonce,
                               enc_nonce = EncNonce,
                               rpc_proc = RpcProc,
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
            %%?LOG_DEBUG("Are we complete and need to wait? ~p ~n",[Done]),
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

handle_info({'EXIT', Pid, Reason}, #sbox_state{rpc_proc = RpcProc} = State) ->
    case Pid of
        RpcProc ->
            ?LOG_ERROR("ssb_peer: rpc_processor exited ~p~n", [Reason]),
            {stop, Reason, State};
        _ ->
            {noreply, State}
    end;

handle_info(Info, State) ->
    ?LOG_INFO("Stopped presumably for normal reason: ~p ~n",[Info]),
    {stop, normal, State}.

%% Send blobs.createWants on req 2, then a want message for each BlobId.
%% Registers blob_client to handle have responses on the -2 stream.
request_blob_wants(Pid, BlobIds) ->
    gen_server:call(Pid, {request_blob_wants, BlobIds}).



handle_call({request_blob_wants, BlobIds}, _From,
            #sbox_state{socket = Socket,
                        enc_sbox_key = EncBoxKey,
                        enc_nonce = EncNonce,
                        rpc_proc = RpcProc} = State) ->
    ok = rpc_processor:register_stream(RpcProc, -2, blob_client),
    NewEncNonce = send_blob_wants(Socket, EncBoxKey, EncNonce, BlobIds),
    {reply, ok, State#sbox_state{enc_nonce = NewEncNonce}};



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

terminate(_Reason, #sbox_state{rpc_proc = RpcProc}) when is_pid(RpcProc) ->
    gen_server:stop(RpcProc),
    ok;
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
                            rpc_proc = RpcProc,
                            response = Response} = State) ->

    %% Should append Msg to rpc_rem_bytes from previous call?
    Parsed = rpc_parse:parse(Data),
    {Status, NewRpcLeftOver, NewEncNonce, NewResponse} =
        case Parsed of
            {partial, nil, Rest} ->
                                                % if partial parse then Rest is the original input
                {partial, Rest, EncNonce, Response};
            {complete, ?RPC_END, <<>>} ->
                ?LOG_DEBUG("The rpc call has ended ~n",[]),
                {complete, <<>>, EncNonce, Response};
            {complete, {Header, Body}, Rest} ->
                %% Need to track request here somehow
                {ProcEncNonce, Resp} =
                    rpc_processor:process(RpcProc,
                                          {Header, Body},
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

%% Send the EBT replicate request and our initial clock.
%% ReqNo = 1 for the outbound duplex stream.
%% The clock includes the remote peer's feedId at seq 0 so the server
%% knows we want all of its messages.
initiate_ebt(Socket, EncBoxKey, EncNonce, RemotePubKey) ->
    EbtReq = utils:encode_rec({[{~"name", [~"ebt", ~"replicate"]},
                                 {~"args", [{[{~"version", 3},
                                              {~"format", ~"classic"}]}]},
                                 {~"type", ~"duplex"}]}),
    Flags = rpc_processor:create_flags(1, 0, 2),
    Header1 = rpc_processor:create_header(Flags, size(EbtReq), 1),
    N1 = send_data(combine(Header1, EbtReq), Socket, EncNonce, EncBoxKey),
    Clock = build_initial_clock(RemotePubKey),
    Header2 = rpc_processor:create_header(Flags, size(Clock), 1),
    ?LOG_DEBUG("Send initial vector ~p ~n", [Clock]),
    send_data(combine(Header2, Clock), Socket, N1, EncBoxKey).

%% Send blobs.createWants RPC on req 2, followed by want messages for BlobIds.
send_blob_wants(Socket, Key, Nonce, BlobIds) ->
    WantsRpc = utils:encode_rec({[{~"name", [?blobs, ?createwants]},
                                   {~"args", []},
                                   {~"type", ~"duplex"}]}),
    Flags = rpc_processor:create_flags(1, 0, 2),
    Header1 = rpc_processor:create_header(Flags, size(WantsRpc), 2),
    N1 = send_data(combine(Header1, WantsRpc), Socket, Nonce, Key),
    WantBody = utils:encode_rec({[{Id, -1} || Id <- BlobIds]}),
    Header2 = rpc_processor:create_header(Flags, size(WantBody), 2),
    send_data(combine(Header2, WantBody), Socket, N1, Key).

%% Build our initial vector clock, including the remote peer's feedId at seq 0
%% so the server sends us everything it has for that feed.
build_initial_clock(RemotePubKey) ->
    RemoteFeedId = <<"@", (base64:encode(RemotePubKey))/binary, ".ed25519">>,
    Pid = utils:find_or_create_feed_pid(RemoteFeedId),
    RemSeq = case ssb_feed:fetch_last_msg(Pid) of
        #message{sequence = S} -> S;
        _ -> 0
    end,
    {OurFeeds} = ebt:initial_vector(),
    AllFeeds = [{RemoteFeedId,
        ebt_vc:encode_clock_int(true, true, RemSeq)} | OurFeeds],
    utils:encode_rec({AllFeeds}).
