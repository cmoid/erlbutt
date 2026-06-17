%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(rpc_processor).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2,
         code_change/3]).

%% API
-export([start_link/0,
         process/3,
         register_stream/3,
         set_remote_pk/2,
         set_owner/2,
         create_flags/3,
         create_header/3,
         parse_flags/1]).

-import(message, [ssb_encoder/3]).
-compile({no_auto_import,[size/1]}).
-import(utils, [size/1]).

%% Start a per-connection rpc_processor; returns {ok, Pid}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Pid is the per-connection rpc_processor started by ssb_peer.
process(Pid, {Header, Body}, Connection) ->
    gen_server:call(Pid, {rpc_process, {Header, Body}, Connection}, 45000).

%% Register a duplex stream so that subsequent messages on ReqNo are
%% routed to Module:handle_data/3.  Used by the client-side peer to
%% register the negative ReqNo it expects for its outbound requests.
register_stream(Pid, ReqNo, Module) ->
    gen_server:call(Pid, {register_stream, ReqNo, Module}).

%% Store the remote peer's public key so invite.use can validate it.
set_remote_pk(Pid, RemotePk) ->
    gen_server:call(Pid, {set_remote_pk, RemotePk}).

%% Store the owning ssb_peer pid so tunnel handlers can send frames back
%% on this connection (writes must go through ssb_peer to keep nonce order).
set_owner(Pid, OwnerPid) ->
    gen_server:call(Pid, {set_owner, OwnerPid}).

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

init([]) ->
    process_flag(trap_exit, true),
    ?LOG_INFO("Started rpc processor ~n", []),
    {ok, #rpc_state{calls = ets:new(rpc_calls, [set, public])}}.

handle_info(Info, State) ->
    ?LOG_INFO("Stopped presumably for normal reason: ~p ~n", [Info]),
    {stop, normal, State}.

handle_call({register_stream, ReqNo, Module}, _From,
            #rpc_state{calls = Calls} = State) ->
    ets:insert(Calls, {ReqNo, Module}),
    {reply, ok, State};

handle_call({set_remote_pk, RemotePk}, _From,
            #rpc_state{calls = Calls} = State) ->
    ets:insert(Calls, {remote_pk, RemotePk}),
    {reply, ok, State};

handle_call({set_owner, OwnerPid}, _From,
            #rpc_state{calls = Calls} = State) ->
    ets:insert(Calls, {owner, OwnerPid}),
    {reply, ok, State};

handle_call({rpc_process, {Header, Body}, #ssb_conn{
                                            socket = Socket,
                                            nonce = Nonce,
                                            secret_box = SecretBoxKey} = Conn},
                            _From, #rpc_state{calls = Calls} = State) ->
    %% Positive ReqNo = new or continuing request from peer.
    %% Negative ReqNo = response to a request we initiated.
    %% Both positive and negative registered streams are routed to the
    %% owning module's handle_data/3.
    ReqNo = req_no(Header),
    HasSeen = ets:lookup(Calls, ReqNo),
    {Non, Res} = case HasSeen of
        [{ReqNo, noop}] ->
            %% Stream already ended or handled; silently ignore continuations.
            {Nonce, none};
        [{ReqNo, {reply_to, Pid, Ref}}] ->
            %% Sync RPC reply: deliver body directly to the waiting caller.
            Pid ! {rpc_reply, Ref, Body},
            {Nonce, none};
        [{ReqNo, {stream_to, Pid, Ref}}] ->
            %% Source stream reply: deliver data or signal end-of-stream.
            {_, IsEnd, _} = parse_flags(Header),
            case IsEnd of
                1 -> Pid ! {stream_done, Ref};
                0 -> Pid ! {stream_data, Ref, Body}
            end,
            {Nonce, none};
        [{ReqNo, {Mod, SinkPid}}] ->
            ?SSB_DEBUG("Stream continuation for req with pid: ~p ~n", [ReqNo]),
            NewNonce = Mod:handle_data(ReqNo, Body, Conn, SinkPid),
            {NewNonce, none};
        [{ReqNo, Mod}] ->
            ?SSB_DEBUG("Stream continuation for req: ~p with body: ~p ~n", [ReqNo, Body]),
            NewNonce = Mod:handle_data(ReqNo, Body, Conn),
            {NewNonce, none};
        [] ->
            dispatch(Calls, ReqNo, Body, Socket, Nonce, SecretBoxKey)
    end,
    {reply, {Non, Res}, State}.

handle_cast(_Msg, State) ->
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

create_req(Body) ->
    DecBody = utils:nat_decode(Body),
    ?SSB_DEBUG("Body decoded is ~p ~n", [DecBody]),
    decode_body(DecBody).

decode_body({Props}) ->
    #ssb_rpc{
        name = proplists:get_value(~"name", Props),
        args = proplists:get_value(~"args", Props),
        type = proplists:get_value(~"type", Props)};

decode_body(Other) ->
    Other.

dispatch(_Calls, ReqNo, Body, _Socket, Nonce, _SecretBoxKey) when ReqNo < 0 ->
    {Nonce, proc_response(ReqNo, Body)};
dispatch(Calls, ReqNo, Body, Socket, Nonce, SecretBoxKey) ->
    Req = create_req(Body),
    NewNonce = proc_request(Calls, ReqNo, Req, Socket, Nonce, SecretBoxKey),
    Tag = case Req of
        #ssb_rpc{name = [?blobs, ?createwants]} -> {wants_stream, ReqNo};
        #ssb_rpc{name = [?ebt,   ~"replicate"]} -> {ebt_stream,   ReqNo};
        #ssb_rpc{name = [?room,  ?attendants]}  -> {attendants_stream, ReqNo};
        _                                        -> none
    end,
    {NewNonce, Tag}.


proc_response(ReqNo, RespBody) ->
    ?SSB_DEBUG("The response from ~p was ~p ~n", [ReqNo, RespBody]),
    RespBody.

proc_request(Calls, ReqNo, #ssb_rpc{name = [?createhistorystream],
                             args = [{ArgProps}]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    Id    = proplists:get_value(~"id",    ArgProps),
    Seq   = proplists:get_value(~"seq",   ArgProps, 0),
    Limit = proplists:get_value(~"limit", ArgProps, -1),
    Keys  = proplists:get_value(~"keys",  ArgProps, true),
    ets:insert(Calls, {ReqNo, noop}),
    FeedPid = utils:find_or_create_feed_pid(Id),
    {NewNonce, _} = ssb_feed:foldl(FeedPid,
        fun(MsgData, {N, Count}) ->
            case Limit >= 0 andalso Count >= Limit of
                true -> {N, Count};
                false ->
                    try
                        Msg = message:decode(MsgData, false),
                        case Msg#message.sequence > Seq of
                            true ->
                                Body = case Keys of
                                    true  -> message:encode(Msg);
                                    false -> MsgData
                                end,
                                Flags  = create_flags(1, 0, 2),
                                Header = create_header(Flags, size(Body), -ReqNo),
                                N1 = utils:send_data(utils:combine(Header, Body),
                                                     Socket, N, SecretBoxKey),
                                {N1, Count + 1};
                            false ->
                                {N, Count}
                        end
                    catch _:_ -> {N, Count}
                    end
            end
        end, {Nonce, 0}),
    TrueEnd = iolist_to_binary(message:ssb_encoder(true, fun message:ssb_encoder/3, [pretty])),
    Flags  = create_flags(1, 1, 2),
    Header = create_header(Flags, size(TrueEnd), -ReqNo),
    utils:send_data(utils:combine(Header, TrueEnd), Socket, NewNonce, SecretBoxKey);

proc_request(_Calls, ReqNo, #ssb_rpc{name = [~"publish"],
                             args = [Content]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    OurId   = keys:pub_key_disp(),
    FeedPid = utils:find_or_create_feed_pid(OurId),
    ok = ssb_feed:post_content(FeedPid, Content),
    Msg  = ssb_feed:fetch_last_msg(FeedPid),
    Body = message:encode(Msg),
    Flags  = create_flags(1, 1, 2),
    Header = create_header(Flags, size(Body), -ReqNo),
    utils:send_data(utils:combine(Header, Body), Socket, Nonce, SecretBoxKey);

proc_request(_Calls, ReqNo, #ssb_rpc{name = [~"get"],
                             args = [MsgId]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    case mess_auth:get(MsgId) of
        not_found ->
            ErrMsg = utils:error_msg(~"Error", ~"message not found"),
            Flags  = create_flags(0, 1, 2),
            Header = create_header(Flags, size(ErrMsg), -ReqNo),
            utils:send_data(utils:combine(Header, ErrMsg), Socket, Nonce, SecretBoxKey);
        Author ->
            FeedPid = utils:find_or_create_feed_pid(Author),
            Msg  = ssb_feed:fetch_msg(FeedPid, MsgId),
            Body = message:encode(Msg),
            Flags  = create_flags(1, 1, 2),
            Header = create_header(Flags, size(Body), -ReqNo),
            utils:send_data(utils:combine(Header, Body), Socket, Nonce, SecretBoxKey)
    end;

proc_request(Calls, ReqNo, #ssb_rpc{name = Name}
             = _ReqBody, Socket, Nonce, SecretBoxKey)
  when Name =:= [~"createLogStream"] orelse Name =:= [~"createFeedStream"] ->
    ets:insert(Calls, {ReqNo, noop}),
    LogFile = <<(config:ssb_repo_loc())/binary, "log.offset">>,
    NewNonce = utils:fold_log_file(
        fun(Body, N) ->
            Flags  = create_flags(1, 0, 2),
            Header = create_header(Flags, size(Body), -ReqNo),
            utils:send_data(utils:combine(Header, Body), Socket, N, SecretBoxKey)
        end, Nonce, LogFile),
    TrueEnd = iolist_to_binary(message:ssb_encoder(true, fun message:ssb_encoder/3, [pretty])),
    Flags  = create_flags(1, 1, 2),
    Header = create_header(Flags, size(TrueEnd), -ReqNo),
    utils:send_data(utils:combine(Header, TrueEnd), Socket, NewNonce, SecretBoxKey);

proc_request(Calls, ReqNo, #ssb_rpc{name = [?gossip, ?ping],
                             args = [{_Args}]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    ets:insert(Calls, {ReqNo, noop}),
    Flags = create_flags(1,0,2),
    TimeStamp = iolist_to_binary(message:ssb_encoder(integer_to_binary(current_time()),
                                    fun message:ssb_encoder/3, [pretty])),
    Header = create_header(Flags, size(TimeStamp), -ReqNo),
    ?SSB_DEBUG("Answering ping with ~p ~n", [{Header, TimeStamp}]),
    utils:send_data(utils:combine(Header, TimeStamp), Socket, Nonce, SecretBoxKey);

proc_request(_Calls, ReqNo, #ssb_rpc{name = [?whoami],
                             args = []}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    %% stream+end+json closes only this call; never send ?RPC_END/?BOX_END
    %% while other streams (e.g. EBT) may be active.
    Flags = create_flags(1, 1, 2),
    Body = whoami(),
    ?SSB_DEBUG("rpc_processor: whoami:  ~p~n", [Body]),
    Header = create_header(Flags, size(Body), -ReqNo),
    utils:send_data(utils:combine(Header, Body), Socket, Nonce, SecretBoxKey);

proc_request(Calls, ReqNo, #ssb_rpc{name = [?blobs, ?createwants],
                             args = []}
             = _ReqBody, _Socket, Nonce, _SecretBoxKey) ->
    %% Register so subsequent messages on this stream route to blob_wants.
    %% Haves are sent on our own createWants stream, not this response channel.
    ?SSB_DEBUG("rpc_processor: createwants reqno ~p~n", [ReqNo]),
    ets:insert(Calls, {ReqNo, blob_wants}),
    Nonce;

proc_request(_Calls, ReqNo, #ssb_rpc{name = [?blobs, ?blobshas],
                             args = [BlobId]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    Has  = blobs:has(BlobId),
    Body = iolist_to_binary(message:ssb_encoder(Has, fun message:ssb_encoder/3, [pretty])),
    Flags  = create_flags(0, 0, 2),
    Header = create_header(Flags, size(Body), -ReqNo),
    NewNonce = utils:send_data(utils:combine(Header, Body), Socket, Nonce, SecretBoxKey),
    utils:send_data(?RPC_END, Socket, NewNonce, SecretBoxKey);

proc_request(_Calls, ReqNo, #ssb_rpc{name = [?blobs, ?blobsget],
                             args = Args}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    BlobId = case Args of
        [Id] when is_binary(Id) -> Id;
        [{Props}] -> proplists:get_value(~"hash", Props)
    end,
    case blobs:fetch(BlobId) of
        {error, not_found} ->
            ErrMsg = utils:error_msg(~"Error", ~"blob not found"),
            Flags  = create_flags(0, 1, 2),
            Header = create_header(Flags, size(ErrMsg), -ReqNo),
            utils:send_data(utils:combine(Header, ErrMsg), Socket, Nonce, SecretBoxKey);
        {ok, BlobData} ->
            send_blob_chunks(BlobData, -ReqNo, Socket, Nonce, SecretBoxKey)
    end;

proc_request(_Calls, ReqNo, #ssb_rpc{name = [?tunnel, ?isRoom],
                             args = []}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    Flags = create_flags(1, 1, 2),
    Body = iolist_to_binary(message:ssb_encoder(config:is_room(),
                                                fun message:ssb_encoder/3, [pretty])),
    Header = create_header(Flags, size(Body), -ReqNo),
    utils:send_data(utils:combine(Header, Body), Socket, Nonce, SecretBoxKey);

proc_request(_Calls, ReqNo, #ssb_rpc{name = [?room, ?metadata]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    %% Membership is trivially true for open rooms; community/restricted
    %% rooms will consult the member registry once it exists (Phase 2b+).
    IsMember = config:room_privacy() =:= open,
    Body = utils:encode_rec({[{~"name", config:room_name()},
                              {~"membership", IsMember},
                              {~"features", [?tunnel, ~"room1", ~"room2"]}]}),
    Flags = create_flags(1, 1, 2),
    Header = create_header(Flags, size(Body), -ReqNo),
    utils:send_data(utils:combine(Header, Body), Socket, Nonce, SecretBoxKey);

proc_request(Calls, ReqNo, #ssb_rpc{name = [?tunnel, ?connect], args = Args}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    case config:is_room() of
        true  -> tunnel_relay_connect(Calls, ReqNo, Args, Socket, Nonce, SecretBoxKey);
        false -> tunnel_accept_connect(Calls, ReqNo, Args, Socket, Nonce, SecretBoxKey)
    end;

proc_request(Calls, ReqNo, #ssb_rpc{name = [?room, ?attendants]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    %% Send the current presence snapshot; subsequent joined/left updates are
    %% pushed by ssb_peer (see the {attendants_stream, ReqNo} dispatch tag).
    ets:insert(Calls, {ReqNo, noop}),
    Ids = room_attendants:list(),
    Body = utils:encode_rec({[{~"type", ~"state"}, {~"ids", Ids}]}),
    Flags = create_flags(1, 0, 2),
    Header = create_header(Flags, size(Body), -ReqNo),
    utils:send_data(utils:combine(Header, Body), Socket, Nonce, SecretBoxKey);

proc_request(Calls, ReqNo, #ssb_rpc{name = [?ebt, ~"replicate"],
                             args = Args}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    {Version, Format} = case Args of
        [{ArgProps}] ->
            {proplists:get_value(~"version", ArgProps, 3),
             proplists:get_value(~"format", ArgProps, ~"classic")};
        _ ->
            {undefined, undefined}
    end,
    %% ponchoWonky/tildefriends don't use these args, though they send the version
    %% and and tildefriends sends the format. We'll use the defaults for
    %% both since they seem to be dead args for now.
    case {Version, Format} of
        {3, ~"classic"} ->
            ets:insert(Calls, {ReqNo, ebt}),
            Flags = create_flags(1, 0, 2),
            InitVectorEnc = utils:encode_rec(ebt:initial_vector()),
            Header = create_header(Flags, size(InitVectorEnc), -ReqNo),
            ?SSB_DEBUG("Answering ebt_rep req ~p with ~p ~n", [ReqNo, {Header, InitVectorEnc}]),
            %% Keep the duplex stream open — do NOT send RPC_END
            utils:send_data(utils:combine(Header, InitVectorEnc),
                            Socket, Nonce, SecretBoxKey);
        _ ->
            ErrMsg = utils:error_msg(~"Error", ~"unsupported EBT version or format"),
            Flags = create_flags(0, 1, 2),
            Header = create_header(Flags, size(ErrMsg), -ReqNo),
            utils:send_data(utils:combine(Header, ErrMsg), Socket, Nonce, SecretBoxKey)
    end;

proc_request(Calls, ReqNo, #ssb_rpc{name = [~"invite", ~"use"],
                             args = [{ArgProps}]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    FeedId = proplists:get_value(~"feed", ArgProps),
    ClientPk = case ets:lookup(Calls, remote_pk) of
        [{remote_pk, Pk}] -> Pk;
        []                -> undefined
    end,
    case invite:validate_and_consume(ClientPk) of
        ok ->
            OurId   = keys:pub_key_disp(),
            FeedPid = utils:find_or_create_feed_pid(OurId),
            FollowContent = {[{~"type",      ~"contact"},
                              {~"contact",   FeedId},
                              {~"following", true},
                              {~"pub",       true}]},
            ok = ssb_feed:post_content(FeedPid, FollowContent),
            TrueBody = iolist_to_binary(message:ssb_encoder(true, fun message:ssb_encoder/3, [pretty])),
            Flags  = create_flags(0, 0, 2),
            Header = create_header(Flags, size(TrueBody), -ReqNo),
            utils:send_data(utils:combine(Header, TrueBody), Socket, Nonce, SecretBoxKey);
        {error, _} ->
            ErrMsg = utils:error_msg(~"Error", ~"invalid or expired invite"),
            Flags  = create_flags(0, 1, 2),
            Header = create_header(Flags, size(ErrMsg), -ReqNo),
            utils:send_data(utils:combine(Header, ErrMsg), Socket, Nonce, SecretBoxKey)
    end;

proc_request(Calls, ReqNo, ReqBody, Socket, Nonce, SecretBoxKey) ->
    ?SSB_DEBUG("Fall thru with ~p ~n", [ReqBody]),
    ets:insert(Calls, {ReqNo, noop}),
    Flags = create_flags(1, 1, 2),
    TrueEnd = message:ssb_encoder(true, fun message:ssb_encoder/3, [pretty]),
    Header = create_header(Flags, size(TrueEnd), -ReqNo),
    utils:send_data(utils:combine(Header, TrueEnd), Socket, Nonce, SecretBoxKey).

%% Stream BlobData in 65 536-byte binary chunks, then close with JSON true.
send_blob_chunks(<<>>, ReqNo, Socket, Nonce, Key) ->
    TrueBody = iolist_to_binary(message:ssb_encoder(true, fun message:ssb_encoder/3, [pretty])),
    Flags    = create_flags(1, 1, 2),
    Header   = create_header(Flags, size(TrueBody), ReqNo),
    utils:send_data(utils:combine(Header, TrueBody), Socket, Nonce, Key);
send_blob_chunks(Data, ReqNo, Socket, Nonce, Key) ->
    ChunkSize = 65536,
    {Chunk, Rest} = case Data of
        <<C:ChunkSize/binary, R/binary>> -> {C, R};
        Last                             -> {Last, <<>>}
    end,
    Flags    = create_flags(1, 0, 0),
    Header   = create_header(Flags, size(Chunk), ReqNo),
    NewNonce = utils:send_data(utils:combine(Header, Chunk), Socket, Nonce, Key),
    send_blob_chunks(Rest, ReqNo, Socket, NewNonce, Key).

current_time() ->
    erlang:system_time(millisecond).

whoami() ->
    iolist_to_binary(message:ssb_encoder({[{~"id", keys:pub_key_disp()}]},
                       fun message:ssb_encoder/3, [])).

%% Room side of tunnel.connect: look up the target attendant and bridge the
%% two connections.  ReqNo is the positive request number on this (the
%% caller's) connection; bridge sends the caller's responses on -ReqNo.
tunnel_relay_connect(Calls, ReqNo, Args, Socket, Nonce, SecretBoxKey) ->
    Target = tunnel_target(Args),
    case Target =/= undefined andalso room_attendants:lookup(Target) of
        {ok, TargetPid} ->
            OwnerPid = owner(Calls),
            {ok, Bridge} = tunnel_bridge:start(OwnerPid, ReqNo, TargetPid, Args),
            ets:insert(Calls, {ReqNo, {tunnel_relay, Bridge}}),
            Nonce;
        _ ->
            tunnel_error(ReqNo, ~"target not connected", Socket, Nonce, SecretBoxKey)
    end.

%% Endpoint side of tunnel.connect: a non-room node was asked (by a room) to
%% accept an incoming tunnelled connection.  Hand it to the registered tunnel
%% listener, which drives the inner protocol (Phase 2c: SHS over the tunnel).
tunnel_accept_connect(Calls, ReqNo, _Args, Socket, Nonce, SecretBoxKey) ->
    OwnerPid = owner(Calls),
    case tunnel_endpoint:accept(ReqNo, OwnerPid) of
        {ok, SinkPid} ->
            ets:insert(Calls, {ReqNo, {tunnel_relay, SinkPid}}),
            Nonce;
        _ ->
            tunnel_error(ReqNo, ~"tunnel not accepted", Socket, Nonce, SecretBoxKey)
    end.

tunnel_target(Args) ->
    case Args of
        [{Props}] -> proplists:get_value(~"target", Props);
        _         -> undefined
    end.

owner(Calls) ->
    case ets:lookup(Calls, owner) of
        [{owner, Pid}] -> Pid;
        []             -> undefined
    end.

tunnel_error(ReqNo, Reason, Socket, Nonce, SecretBoxKey) ->
    ErrMsg = utils:error_msg(~"Error", Reason),
    Flags  = create_flags(0, 1, 2),
    Header = create_header(Flags, size(ErrMsg), -ReqNo),
    utils:send_data(utils:combine(Header, ErrMsg), Socket, Nonce, SecretBoxKey).

-ifdef(TEST).

body1_test() ->
    Rpc = create_req(iolist_to_binary(ssb_encoder({[{~"name",[~"gossip",~"ping"]},
                                    {~"args",[{[{~"timeout",300000}]}]},
                                    {~"type",~"duplex"}]}, fun message:ssb_encoder/3, []))),
    ?assert(Rpc#ssb_rpc.name == [~"gossip",~"ping"]),
    ?assert(Rpc#ssb_rpc.type == ~"duplex").

body2_test() ->
    Rpc = create_req(iolist_to_binary(ssb_encoder(22222, fun message:ssb_encoder/3, []))),
    ?assert(Rpc == 22222).

flags_test() ->
    ?assert(<<10>> == create_flags(1,0,2)),
    ?assert({1,0,2} == parse_flags(<<10>>)),
    ?assert({1,1,2} == parse_flags(<<14>>)),
    ?assert(<<14>> == create_flags(1,1,2)).

flags_eq_test() ->
    ?assert(create_flags(1,0,10) == create_flags(1,0,2)).

req_no_test() ->
    Flags = create_flags(1,1,2),
    Header = create_header(Flags,size(~"true"), 5),
    ?assert(req_no(Header) == 5).

neg_req_no_test() ->
    Flags = create_flags(1,1,2),
    Header = create_header(Flags,size(~"true"), -5),
    ?assert(req_no(Header) == -5).

-endif.
