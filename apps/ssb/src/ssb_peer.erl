%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(ssb_peer).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-include_lib("ssb/include/ssb.hrl").

-export([start_link/2,
         start_link/3,
         start/2,
         start/3,
         send/2,
         rpc_call/3,
         rpc_call/4,
         rpc_stream_call/3,
         request_ebt/1,
         request_blob_wants/3,
         fetch_blob/2,
         has_blob/2,
         drain_haves/1]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2,
         code_change/3]).

-compile({no_auto_import,[size/1]}).
-import(utils, [concat/1,
                combine/2,
                send_data/4,
                size/1]).

-define(EBT_STALE_CHECK_MS,    ?DEFAULT_EBT_STALE_CHECK_MS).
-define(EBT_STALE_THRESHOLD_S, ?DEFAULT_EBT_STALE_THRESHOLD_S).
-define(EBT_ENTROPY_MS,        ?DEFAULT_EBT_ENTROPY_MS).

%% connect to another peer on the default port (ssb app env or 8008).
start_link(Ip, PubKey) ->
    gen_server:start_link(?MODULE, [Ip, PubKey], []).

%% connect to another peer on an explicit port.
start_link(Ip, Port, PubKey) when is_integer(Port) ->
    gen_server:start_link(?MODULE, [Ip, Port, PubKey], []);

%% ranch 2.x protocol callback — accept a connection from another peer.
%% Note: it no longer passes the socket in.
start_link(Ref, Transport, Opts) ->
    gen_server:start_link(?MODULE, {ranch, Ref, Transport, Opts}, []).

%% Unlinked variants — use when the caller is a temporary process (e.g. rpc:call).
start(Ip, PubKey) ->
    gen_server:start(?MODULE, [Ip, PubKey], []).

start(Ip, Port, PubKey) when is_integer(Port) ->
    gen_server:start(?MODULE, [Ip, Port, PubKey], []).

%% send data to another peer, asynchronously
send(Pid, Data) ->
    gen_server:cast(Pid, {send, Data}).

%% Send an RPC request and block until the response arrives (or timeout).
%% Method is a list of binaries, e.g. [<<"whoami">>].
%% Type is <<"async">>, <<"source">>, or <<"sync">>.
rpc_call(Pid, Method, Type) ->
    gen_server:call(Pid, {rpc_call, Method, Type, []}, 10000).

rpc_call(Pid, Method, Type, Args) ->
    gen_server:call(Pid, {rpc_call, Method, Type, Args}, 10000).

%% Send a source RPC request and collect all response bodies until end-of-stream.
%% Returns {ok, [Body]} in arrival order.
rpc_stream_call(Pid, Method, Args) ->
    gen_server:call(Pid, {rpc_stream_call, Method, Args}, 30000).

request_ebt(Pid) ->
    gen_server:cast(Pid, {request_ebt}).


init([Ip, PubKey]) ->
    Port = application:get_env(ssb, port, 8008),
    init([Ip, Port, PubKey]);

init([Ip, Port, PubKey]) ->
    process_flag(trap_exit, true),
    try
        {ok, State} = outbound_connect(Ip, Port, PubKey),
        {ok, State}
    catch
        error:Reason ->
            {stop, Reason}
    end;

%% a 0 timeout on this init is need because the ranch handshake is blocking
%% we'll make that call and get the socket in the timeout handler.
init({ranch, Ref, Transport, _Opts}) ->
    {ok, #sbox_state{ref = Ref,
                     transport = Transport}, 0}.

handle_info({tcp, Socket, Data},
            #sbox_state{socket=Socket,
                   transport=Transport,
                   shook_hands = 0} = State) ->
    try
        {ok, {DecBoxKey, DecNonce, EncBoxKey, EncNonce, ClientPk, WinNetId}}
            = shs:server_shake_hands(Data, Socket, Transport),
        network_id_cache:record(ClientPk, WinNetId),
        {ok, RpcProc} = rpc_processor:start_link(),
        ok = rpc_processor:set_remote_pk(RpcProc, ClientPk),
        Transport:setopts(Socket, [{active, once}]),
        %% Skip createWants for invite connections — the peer expects invite.use
        %% as the first message, not a createWants stream frame.
        IsInvite = invite_store:is_invite(ClientPk),
        {N1, WantsReqNo} = case IsInvite of
            true ->
                {EncNonce, undefined};
            false ->
                Req = 1,
                Nonce1 = open_wants_stream(Socket, EncBoxKey, EncNonce, Req),
                ok = rpc_processor:register_stream(RpcProc, -Req, blob_wants),
                {Nonce1, Req}
        end,
        {noreply, State#sbox_state{dec_sbox_key = DecBoxKey,
                                   enc_sbox_key = EncBoxKey,
                                   dec_nonce = DecNonce,
                                   enc_nonce = N1,
                                   rpc_proc = RpcProc,
                                   shook_hands = 1,
                                   remote_pk = ClientPk,
                                   our_wants_req = WantsReqNo,
                                   req_counter = case WantsReqNo of
                                                     undefined -> 0;
                                                     _         -> WantsReqNo
                                                 end}}
    catch
        error:Reason ->
            ?SSB_ERROR("Unable to shake hands with stranger ~p ~n",
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
                                 transport = Transport} = State) ->
    %% this timeout happens immediately on the init call
    %% so that the ranch handshake can be called, which
    %% returns the socket. This handshake replaces the previous accept_ack
    %% call that was blocking in the ranch 1.x protocol.
    {ok, Socket} = ranch:handshake(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    {noreply, State#sbox_state{socket = Socket}};

handle_info({'EXIT', Pid, Reason}, #sbox_state{rpc_proc = RpcProc} = State) ->
    case Pid of
        RpcProc ->
            ?SSB_ERROR("ssb_peer: rpc_processor exited ~p~n", [Reason]),
            {stop, Reason, State};
        _ ->
            {noreply, State}
    end;

handle_info({rpc_reply, Ref, Body},
            #sbox_state{pending_rpc = Pending} = State) ->
    case maps:take(Ref, Pending) of
        {From, Rest} ->
            gen_server:reply(From, {ok, Body}),
            {noreply, State#sbox_state{pending_rpc = Rest}};
        error ->
            {noreply, State}
    end;

handle_info({stream_data, Ref, Body},
            #sbox_state{pending_rpc = Pending} = State) ->
    case maps:find(Ref, Pending) of
        {ok, {collecting, From, Acc}} ->
            {noreply, State#sbox_state{
                pending_rpc = Pending#{Ref := {collecting, From, [Body | Acc]}}}};
        _ ->
            {noreply, State}
    end;

handle_info({stream_done, Ref},
            #sbox_state{pending_rpc = Pending} = State) ->
    case maps:take(Ref, Pending) of
        {{collecting, From, Acc}, Rest} ->
            gen_server:reply(From, {ok, lists:reverse(Acc)}),
            {noreply, State#sbox_state{pending_rpc = Rest}};
        _ ->
            {noreply, State}
    end;

handle_info({blob_collected, Ref, Data},
            #sbox_state{pending_fetch = {From, Ref}} = State) ->
    gen_server:reply(From, {ok, Data}),
    {noreply, State#sbox_state{pending_fetch = undefined}};

handle_info({has_collected, Ref, Result},
            #sbox_state{pending_has = {From, Ref}} = State) ->
    gen_server:reply(From, {ok, Result}),
    {noreply, State#sbox_state{pending_has = undefined}};

handle_info(check_ebt_stale, #sbox_state{ebt_active = true,
                                          ebt_last_rx = LastRx} = State) ->
    Elapsed = erlang:system_time(second) - LastRx,
    case Elapsed >= ?EBT_STALE_THRESHOLD_S of
        true ->
            ?SSB_INFO("EBT: connection stale (~ps idle), closing~n", [Elapsed]),
            {stop, {shutdown, ebt_stale}, State};
        false ->
            {noreply, State#sbox_state{ebt_stale_ref = schedule_ebt_stale_check()}}
    end;

handle_info(check_ebt_stale, State) ->
    {noreply, State};

handle_info(ebt_anti_entropy, #sbox_state{ebt_active = true,
                                           ebt_out_req = OutReq,
                                           socket = Socket,
                                           enc_sbox_key = Key,
                                           enc_nonce = Nonce} = State) ->
    Clock = utils:encode_rec(ebt:full_clock()),
    Flags = rpc_processor:create_flags(1, 0, 2),
    Header = rpc_processor:create_header(Flags, size(Clock), OutReq),
    NewNonce = send_data(combine(Header, Clock), Socket, Nonce, Key),
    ?SSB_DEBUG("EBT: sent anti-entropy clock~n", []),
    {noreply, State#sbox_state{enc_nonce = NewNonce,
                               ebt_entropy_ref = schedule_ebt_entropy()}};

handle_info(ebt_anti_entropy, State) ->
    {noreply, State};

handle_info(Info, State) ->
    ?SSB_INFO("Stopped presumably for normal reason: ~p ~n",[Info]),
    {stop, normal, State}.

%% Send blobs.createWants, then a want message for each BlobId.
%% Haves arrive asynchronously as {have, BlobId, Size} messages to NotifyPid.
request_blob_wants(Pid, BlobIds, NotifyPid) ->
    gen_server:cast(Pid, {request_blob_wants, BlobIds, NotifyPid}).

%% Fetch a blob from the remote peer via blobs.get.
fetch_blob(Pid, BlobId) ->
    gen_server:call(Pid, {fetch_blob, BlobId}).

%% Check whether the remote peer holds a blob. Returns {ok, true} or {ok, false}.
has_blob(Pid, BlobId) ->
    gen_server:call(Pid, {has_blob, BlobId}).

%% Drain {have, BlobId, Size} messages from the mailbox until Timeout ms of
%% silence. Returns [{BlobId, Size}] in arrival order.
drain_haves(Timeout) ->
    drain_haves(Timeout, []).
drain_haves(Timeout, Acc) ->
    receive
        {have, BlobId, Size} -> drain_haves(Timeout, [{BlobId, Size} | Acc])
    after Timeout ->
        lists:reverse(Acc)
    end.

handle_call({rpc_call, Method, Type, Args}, From,
            #sbox_state{socket = Socket,
                        enc_sbox_key = EncBoxKey,
                        enc_nonce = EncNonce,
                        rpc_proc = RpcProc,
                        pending_rpc = Pending} = State) ->
    {ReqNo, State1} = next_req(State),
    Ref = make_ref(),
    ok = rpc_processor:register_stream(RpcProc, -ReqNo, {reply_to, self(), Ref}),
    ReqBody = utils:encode_rec({[{~"name", Method},
                                  {~"args", Args},
                                  {~"type", Type}]}),
    Flags = rpc_processor:create_flags(0, 0, 2),
    Header = rpc_processor:create_header(Flags, size(ReqBody), ReqNo),
    NewNonce = utils:send_data(utils:combine(Header, ReqBody), Socket, EncNonce, EncBoxKey),
    {noreply, State1#sbox_state{enc_nonce   = NewNonce,
                                pending_rpc = Pending#{Ref => From}}};

handle_call({rpc_stream_call, Method, Args}, From,
            #sbox_state{socket = Socket,
                        enc_sbox_key = EncBoxKey,
                        enc_nonce = EncNonce,
                        rpc_proc = RpcProc,
                        pending_rpc = Pending} = State) ->
    {ReqNo, State1} = next_req(State),
    Ref = make_ref(),
    ok = rpc_processor:register_stream(RpcProc, -ReqNo, {stream_to, self(), Ref}),
    ReqBody = utils:encode_rec({[{~"name", Method},
                                  {~"args", Args},
                                  {~"type", ~"source"}]}),
    Flags = rpc_processor:create_flags(1, 0, 2),
    Header = rpc_processor:create_header(Flags, size(ReqBody), ReqNo),
    NewNonce = utils:send_data(utils:combine(Header, ReqBody), Socket, EncNonce, EncBoxKey),
    {noreply, State1#sbox_state{enc_nonce   = NewNonce,
                                pending_rpc = Pending#{Ref => {collecting, From, []}}}};

handle_call({fetch_blob, BlobId}, From,
            #sbox_state{socket = Socket,
                        enc_sbox_key = EncBoxKey,
                        enc_nonce = EncNonce,
                        rpc_proc = RpcProc} = State) ->
    {ReqNo, State1} = next_req(State),
    Ref = make_ref(),
    SelfPid = self(),
    SinkPid = spawn(fun() -> blob_collect(SelfPid, Ref, <<>>) end),
    ok = rpc_processor:register_stream(RpcProc, -ReqNo, {blob_get_client, SinkPid}),
    NewEncNonce = send_blob_get(Socket, EncBoxKey, EncNonce, BlobId, ReqNo),
    {noreply, State1#sbox_state{enc_nonce = NewEncNonce,
                                pending_fetch = {From, Ref}}};


handle_call({has_blob, BlobId}, From,
            #sbox_state{socket = Socket,
                        enc_sbox_key = EncBoxKey,
                        enc_nonce = EncNonce,
                        rpc_proc = RpcProc} = State) ->
    {ReqNo, State1} = next_req(State),
    Ref = make_ref(),
    SelfPid = self(),
    SinkPid = spawn(fun() -> blob_has_collect(SelfPid, Ref) end),
    ok = rpc_processor:register_stream(RpcProc, -ReqNo, {blob_has_client, SinkPid}),
    NewEncNonce = send_blob_has(Socket, EncBoxKey, EncNonce, BlobId, ReqNo),
    {noreply, State1#sbox_state{enc_nonce = NewEncNonce,
                                pending_has = {From, Ref}}};

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

handle_cast({request_blob_wants, BlobIds, NotifyPid},
            #sbox_state{socket = Socket,
                        enc_sbox_key = EncBoxKey,
                        enc_nonce = EncNonce,
                        rpc_proc = RpcProc,
                        our_wants_req = OurWantsReq,
                        remote_wants_req = RemoteWantsReq} = State) ->
    %% Register on both channels so haves reach us regardless of peer style.
    ok = rpc_processor:register_stream(RpcProc, -OurWantsReq, {blob_wants, NotifyPid}),
    case RemoteWantsReq of
        undefined -> ok;
        _         -> ok = rpc_processor:register_stream(RpcProc, RemoteWantsReq, {blob_wants, NotifyPid})
    end,
    %% Duplex-style peers (PW, TF) expect our wants on their response channel (-remote_wants_req).
    %% Peers that haven't opened createWants get wants on our own source stream.
    SendReqNo = case RemoteWantsReq of
        undefined -> OurWantsReq;
        _         -> -RemoteWantsReq
    end,
    NewEncNonce = send_want_body(Socket, EncBoxKey, EncNonce, BlobIds, SendReqNo),
    {noreply, State#sbox_state{enc_nonce = NewEncNonce}};

handle_cast({request_ebt}, #sbox_state{ebt_active = true} = State) ->
    {noreply, State};

handle_cast({request_ebt}, #sbox_state{socket = Socket,
                                       enc_sbox_key = EncBoxKey,
                                       enc_nonce = EncNonce,
                                       rpc_proc = RpcProc,
                                       remote_pk = PubKey} = State) ->
    {ReqNo, State1} = next_req(State),
    ok = rpc_processor:register_stream(RpcProc, -ReqNo, ebt),
    NewEncNonce = initiate_ebt(Socket, EncBoxKey, EncNonce, PubKey, ReqNo),
    {noreply, State1#sbox_state{enc_nonce = NewEncNonce,
                                ebt_active = true,
                                ebt_out_req = ReqNo,
                                ebt_last_rx = erlang:system_time(second),
                                ebt_stale_ref = schedule_ebt_stale_check(),
                                ebt_entropy_ref = schedule_ebt_entropy()}};

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #sbox_state{rpc_proc = RpcProc,
                               ebt_stale_ref = StaleRef,
                               ebt_entropy_ref = EntropyRef}) when is_pid(RpcProc) ->
    cancel_ebt_timer(StaleRef),
    cancel_ebt_timer(EntropyRef),
    gen_server:stop(RpcProc),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

schedule_ebt_stale_check() ->
    erlang:send_after(?EBT_STALE_CHECK_MS, self(), check_ebt_stale).

cancel_ebt_timer(undefined) -> ok;
cancel_ebt_timer(Ref)       -> erlang:cancel_timer(Ref), ok.

schedule_ebt_entropy() ->
    erlang:send_after(?EBT_ENTROPY_MS, self(), ebt_anti_entropy).

next_req(#sbox_state{req_counter = N} = State) ->
    {N + 1, State#sbox_state{req_counter = N + 1}}.

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
                    ?SSB_DEBUG("Box end received ~p ~n",[Msg]),
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
                            our_wants_req = OurWantsReq,
                            response = Response} = State) ->

    Parsed = rpc_parse:parse(Data),
    {Status, NewRpcLeftOver, NewEncNonce, NewResponse} =
        case Parsed of
            {partial, nil, Rest} ->
                {partial, Rest, EncNonce, Response};
            {complete, ?RPC_END, <<>>} ->
                ?SSB_DEBUG("The rpc call has ended ~n",[]),
                {complete, <<>>, EncNonce, Response};
            {complete, {Header, Body}, Rest} ->
                {ProcEncNonce, Resp} =
                    rpc_processor:process(RpcProc,
                                          {Header, Body},
                                          #ssb_conn{
                                             socket = Socket,
                                             nonce = EncNonce,
                                             secret_box = EncBoxKey,
                                             our_wants_req = OurWantsReq}),
                ?SSB_DEBUG("The rpc call returned ~p ~n",[Resp]),
                {complete, Rest, ProcEncNonce, Resp}
        end,

    NewState0 = State#sbox_state{enc_nonce = NewEncNonce,
                                 rpc_rem_bytes = NewRpcLeftOver,
                                 response = NewResponse},
    NewState = case NewResponse of
        {wants_stream, ReqNo} ->
            NewState0#sbox_state{remote_wants_req = ReqNo};
        {ebt_stream, ReqNo} ->
            NewState0#sbox_state{ebt_active = true,
                                 ebt_out_req = -ReqNo,
                                 ebt_last_rx = erlang:system_time(second),
                                 ebt_stale_ref = schedule_ebt_stale_check(),
                                 ebt_entropy_ref = schedule_ebt_entropy()};
        _ ->
            case NewState0#sbox_state.ebt_active of
                true  -> NewState0#sbox_state{ebt_last_rx = erlang:system_time(second)};
                false -> NewState0
            end
    end,
    case {size(NewRpcLeftOver) >= 9, Status} of
        {true, complete} ->
            rpc_parse(NewRpcLeftOver, NewState#sbox_state{rpc_rem_bytes = <<>>});
        _ ->
            NewState
    end.

network_error(Reason, State) ->
    ?SSB_ERROR("Network error ~p ~n",[Reason]),
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

initiate_ebt(Socket, EncBoxKey, EncNonce, RemotePubKey, ReqNo) ->
    EbtReq = utils:encode_rec({[{~"name", [~"ebt", ~"replicate"]},
                                 {~"args", [{[{~"version", 3},
                                              {~"format", ~"classic"}]}]},
                                 {~"type", ~"duplex"}]}),
    Flags = rpc_processor:create_flags(1, 0, 2),
    Header1 = rpc_processor:create_header(Flags, size(EbtReq), ReqNo),
    N1 = send_data(combine(Header1, EbtReq), Socket, EncNonce, EncBoxKey),
    Clock = build_initial_clock(RemotePubKey),
    Header2 = rpc_processor:create_header(Flags, size(Clock), ReqNo),
    ?SSB_DEBUG("Send initial vector ~p ~n", [Clock]),
    send_data(combine(Header2, Clock), Socket, N1, EncBoxKey).

blob_collect(Peer, Ref, Acc) ->
    receive
        {chunk, Data} -> blob_collect(Peer, Ref, <<Acc/binary, Data/binary>>);
        done          -> Peer ! {blob_collected, Ref, Acc}
    end.



blob_has_collect(Peer, Ref) ->
    receive
        {has_result, Result} -> Peer ! {has_collected, Ref, Result}
    after 5000 ->
        Peer ! {has_collected, Ref, false}
    end.

send_blob_has(Socket, Key, Nonce, BlobId, ReqNo) ->
    HasRpc = utils:encode_rec({[{~"name", [?blobs, ?blobshas]},
                                 {~"args", [BlobId]},
                                 {~"type", ~"async"}]}),
    Flags = rpc_processor:create_flags(0, 0, 2),
    Header = rpc_processor:create_header(Flags, size(HasRpc), ReqNo),
    ?SSB_DEBUG("asking blob has ~p ~n", [ReqNo]),
    send_data(combine(Header, HasRpc), Socket, Nonce, Key).

send_blob_get(Socket, Key, Nonce, BlobId, ReqNo) ->
    GetRpc = utils:encode_rec({[{~"name", [?blobs, ?blobsget]},
                                 {~"args", [BlobId]},
                                 {~"type", ~"source"}]}),
    Flags = rpc_processor:create_flags(1, 0, 2),
    Header = rpc_processor:create_header(Flags, size(GetRpc), ReqNo),
    send_data(combine(Header, GetRpc), Socket, Nonce, Key).

%% Open our createWants source stream.  Call once per connection.
open_wants_stream(Socket, Key, Nonce, ReqNo) ->
    WantsRpc = utils:encode_rec({[{~"name", [?blobs, ?createwants]},
                                   {~"args", []},
                                   {~"type", ~"source"}]}),
    Flags = rpc_processor:create_flags(1, 0, 2),
    Header = rpc_processor:create_header(Flags, size(WantsRpc), ReqNo),
    send_data(combine(Header, WantsRpc), Socket, Nonce, Key).

%% Send only the want body on an existing duplex stream at ReqNo.
send_want_body(Socket, Key, Nonce, BlobIds, ReqNo) ->
    WantBody = utils:encode_rec({[{Id, -1} || Id <- BlobIds]}),
    Flags = rpc_processor:create_flags(1, 0, 2),
    Header = rpc_processor:create_header(Flags, size(WantBody), ReqNo),
    send_data(combine(Header, WantBody), Socket, Nonce, Key).

outbound_connect(Ip, Port, PubKey) ->
    NetIds = ordered_net_ids(PubKey),
    try_net_ids(Ip, Port, PubKey, NetIds, 0).

ordered_net_ids(PubKey) ->
    All = config:network_ids(),
    case network_id_cache:lookup(PubKey) of
        {ok, Cached} -> [Cached | lists:delete(Cached, All)];
        miss          -> All
    end.

try_net_ids(_Ip, _Port, _PubKey, [], _Attempt) ->
    error(no_matching_network_id);
try_net_ids(Ip, Port, PubKey, [NetId | Rest], Attempt) ->
    case Attempt > 0 of
        true  -> timer:sleep(backoff(Attempt));
        false -> ok
    end,
    Socket = connect(Ip, Port),
    try shs:client_shake_hands(Socket, PubKey, NetId) of
        {ok, {Socket, DecBoxKey, DecNonce, EncBoxKey, EncNonce}} ->
            network_id_cache:record(PubKey, NetId),
            {ok, build_outbound_state(Socket, DecBoxKey, DecNonce, EncBoxKey, EncNonce, PubKey)}
    catch
        _:_ ->
            gen_tcp:close(Socket),
            try_net_ids(Ip, Port, PubKey, Rest, Attempt + 1)
    end.

backoff(N) ->
    min(1000 * (1 bsl (N - 1)), 30000).

build_outbound_state(Socket, DecBoxKey, DecNonce, EncBoxKey, EncNonce, PubKey) ->
    ranch_tcp:setopts(Socket, [{active, false}]),
    {ok, RpcProc} = rpc_processor:start_link(),
    ranch_tcp:setopts(Socket, [{active, once}]),
    WantsReqNo = 1,
    N1 = open_wants_stream(Socket, EncBoxKey, EncNonce, WantsReqNo),
    ok = rpc_processor:register_stream(RpcProc, -WantsReqNo, blob_wants),
    #sbox_state{socket = Socket,
                transport = ranch_tcp,
                dec_sbox_key = DecBoxKey,
                enc_sbox_key = EncBoxKey,
                dec_nonce = DecNonce,
                enc_nonce = N1,
                rpc_proc = RpcProc,
                remote_pk = PubKey,
                shook_hands = 1,
                our_wants_req = WantsReqNo,
                req_counter = WantsReqNo}.

%% Build our initial vector clock from all locally known feeds, ensuring the
%% remote peer's feed is included (at seq 0 if we have no messages from them yet).
build_initial_clock(RemotePubKey) ->
    RemoteFeedId = <<"@", (base64:encode(RemotePubKey))/binary, ".ed25519">>,
    {FullClock} = ebt:full_clock(),
    ClockWithRemote = case lists:keymember(RemoteFeedId, 1, FullClock) of
        true  -> FullClock;
        false -> [{RemoteFeedId, ebt_vc:encode_clock_int(true, true, 0)} | FullClock]
    end,
    utils:encode_rec({ClockWithRemote}).
