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
        [{ReqNo, {live_source, StreamPid}}] ->
            %% only the client's end/cancel matters on a live source
            {_, IsEnd, _} = parse_flags(Header),
            case IsEnd of
                1 ->
                    view_stream:stop(StreamPid),
                    ets:insert(Calls, {ReqNo, noop});
                0 -> ok
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
        name = normalize_name(proplists:get_value(~"name", Props)),
        args = proplists:get_value(~"args", Props),
        type = proplists:get_value(~"type", Props)};

decode_body(Other) ->
    Other.

%% muxrpc sends a method name as an array of path segments
%% (["blobs","get"]), but the manifest reflection call — which every
%% ssb-client makes on connect to build its api — arrives as a bare
%% string ("manifest").  Normalize a bare binary to a single-segment
%% list so dispatch and the plugin registry see one consistent shape.
normalize_name(Name) when is_binary(Name) -> [Name];
normalize_name(Name)                       -> Name.

dispatch(_Calls, ReqNo, Body, _Socket, Nonce, _SecretBoxKey) when ReqNo < 0 ->
    {Nonce, proc_response(ReqNo, Body)};
dispatch(Calls, ReqNo, Body, Socket, Nonce, SecretBoxKey) ->
    Req = create_req(Body),
    case permitted(Calls, Req) of
        true ->
            NewNonce = proc_request(Calls, ReqNo, Req, Socket, Nonce, SecretBoxKey),
            Tag = case Req of
                #ssb_rpc{name = [?blobs, ?createwants]} -> {wants_stream, ReqNo};
                #ssb_rpc{name = [?ebt,   ~"replicate"]} -> {ebt_stream,   ReqNo};
                #ssb_rpc{name = [?room,  ?attendants]}  -> {attendants_stream, ReqNo};
                #ssb_rpc{name = [?tunnel, ?endpoints]}  -> {endpoints_stream, ReqNo};
                _                                        -> none
            end,
            {NewNonce, Tag};
        {denied, Name, Class, Perm} ->
            ?SSB_INFO("rpc ~p denied for ~p (class ~p, needs ~p)",
                      [Name, caller_feed_id(Calls), Class, Perm]),
            ets:insert(Calls, {ReqNo, noop}),
            {rpc_error(ReqNo, ~"method not allowed", Socket, Nonce, SecretBoxKey),
             none}
    end.

%% Single permission choke point for every incoming request, builtin or
%% plugin.  Methods the registry does not know keep the old fall-through
%% behavior (answered with true), so unknown-method handling is unchanged.
permitted(Calls, #ssb_rpc{name = Name}) when is_list(Name) ->
    Perm = case plugin_registry:lookup(Name) of
        {ok, {_Mod, _Kind, P}}  -> P;
        {builtin, _Kind, P}     -> P;
        unknown                 -> anyone
    end,
    case Perm of
        anyone -> true;
        _ ->
            Class = maps:get(class, caller_info(Calls)),
            case plugin_registry:allowed(Class, Perm) of
                true  -> true;
                false -> {denied, Name, Class, Perm}
            end
    end;
permitted(_Calls, _Req) ->
    true.


proc_response(ReqNo, RespBody) ->
    ?SSB_DEBUG("The response from ~p was ~p ~n", [ReqNo, RespBody]),
    RespBody.

proc_request(Calls, ReqNo, #ssb_rpc{name = [Method],
                             args = [{ArgProps}]}
             = _ReqBody, Socket, Nonce, SecretBoxKey)
  when Method =:= ?createhistorystream orelse Method =:= ~"createUserStream" ->
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
    %% async reply: stream=0.  A stream/end-flagged frame is rejected by
    %% a standard muxrpc async caller ("no stream for incoming msg").
    Flags  = create_flags(0, 0, 2),
    Header = create_header(Flags, size(Body), -ReqNo),
    utils:send_data(utils:combine(Header, Body), Socket, Nonce, SecretBoxKey);

proc_request(_Calls, ReqNo, #ssb_rpc{name = [~"get"],
                             args = [Arg]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    %% ssb-db get accepts either a bare message id or an options object
    %% {id, private, ...}; the silkpurse renderer sends the object form.
    MsgId = case Arg of
        {Props} when is_list(Props) -> proplists:get_value(~"id", Props);
        Id when is_binary(Id)       -> Id;
        _                           -> undefined
    end,
    case MsgId =/= undefined andalso mess_auth:get(MsgId) of
        Author when is_binary(Author) ->
            FeedPid = utils:find_or_create_feed_pid(Author),
            Msg  = ssb_feed:fetch_msg(FeedPid, MsgId),
            %% ssb-db get returns the bare message value, not the
            %% {key, value, timestamp} envelope (the renderer reads
            %% value.content directly).
            Body = message:encode_value(Msg),
            Flags  = create_flags(0, 0, 2),   %% async reply: stream=0
            Header = create_header(Flags, size(Body), -ReqNo),
            utils:send_data(utils:combine(Header, Body), Socket, Nonce, SecretBoxKey);
        _ ->
            ErrMsg = utils:error_msg(~"Error", ~"message not found"),
            Flags  = create_flags(0, 1, 2),
            Header = create_header(Flags, size(ErrMsg), -ReqNo),
            utils:send_data(utils:combine(Header, ErrMsg), Socket, Nonce, SecretBoxKey)
    end;

proc_request(Calls, ReqNo, #ssb_rpc{name = Name}
             = _ReqBody, Socket, Nonce, SecretBoxKey)
  when Name =:= [~"createLogStream"] orelse Name =:= [~"createFeedStream"] ->
    ets:insert(Calls, {ReqNo, noop}),
    %% arrival order via the ingest journal; bodies from the per-feed store
    NewNonce = ingest_journal:stream_messages(
        fun(Body, N) ->
            Flags  = create_flags(1, 0, 2),
            Header = create_header(Flags, size(Body), -ReqNo),
            utils:send_data(utils:combine(Header, Body), Socket, N, SecretBoxKey)
        end, Nonce),
    TrueEnd = iolist_to_binary(message:ssb_encoder(true, fun message:ssb_encoder/3, [pretty])),
    Flags  = create_flags(1, 1, 2),
    Header = create_header(Flags, size(TrueEnd), -ReqNo),
    utils:send_data(utils:combine(Header, TrueEnd), Socket, NewNonce, SecretBoxKey);

proc_request(Calls, ReqNo, #ssb_rpc{name = [?gossip, ?ping],
                             args = [{_Args}]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    %% gossip.ping is pull-ping: a 5-minute keepalive volley, NOT a 30s liveness
    %% check.  Answer the opening frame once and mark the stream noop.  Do NOT
    %% volley every frame: combined with this unprompted reply it desyncs
    %% pull-ping's serve/volley alternation into a tight RTT-speed loop.
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
    %% sync reply: stream=0.  (A stream/end-flagged frame is rejected by
    %% a standard muxrpc sync/async caller as "no stream for incoming
    %% msg".)  This is a single reply on this req, so it does not touch
    %% other open streams (e.g. EBT).
    Flags = create_flags(0, 0, 2),
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
    %% blobs.get args are either the id string directly, or an object whose
    %% blob id lives under "key" (ssb-blobs / PonchoWonky) or "hash".
    %% this is missing in the protocol guide.
    BlobId = case Args of
        [Id] when is_binary(Id) -> Id;
        [{Props}] ->
            case proplists:get_value(~"key", Props) of
                undefined -> proplists:get_value(~"hash", Props);
                Key       -> Key
            end;
        _ -> undefined
    end,
    case BlobId =/= undefined andalso blobs:fetch(BlobId) of
        {ok, BlobData} ->
            send_blob_chunks(BlobData, -ReqNo, Socket, Nonce, SecretBoxKey);
        _ ->
            ErrMsg = utils:error_msg(~"Error", ~"blob not found"),
            Flags  = create_flags(0, 1, 2),
            Header = create_header(Flags, size(ErrMsg), -ReqNo),
            utils:send_data(utils:combine(Header, ErrMsg), Socket, Nonce, SecretBoxKey)
    end;

proc_request(Calls, ReqNo, #ssb_rpc{name = [?tunnel, ?isRoom],
                             args = []}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    %% PROBE: a rooms-2.0 client (manyverse) only calls this when room.metadata
    %% came back as "method missing"; poncho (1.0) always calls it. So seeing
    %% this from manyverse means it rejected our metadata reply above.
    ?SSB_INFO("ROOMDBG tunnel.isRoom (1.0 path) from=~p answering ~p~n",
              [caller_feed_id(Calls), config:is_room()]),
    %% async reply: stream=0,end=0. A stream/end-flagged reply is routed by the
    %% caller's packet-stream to _onstream (no matching outstream) and dropped,
    %% so the client's isRoom() promise never resolves.
    Flags = create_flags(0, 0, 2),
    Body = iolist_to_binary(message:ssb_encoder(config:is_room(),
                                                fun message:ssb_encoder/3, [pretty])),
    Header = create_header(Flags, size(Body), -ReqNo),
    utils:send_data(utils:combine(Header, Body), Socket, Nonce, SecretBoxKey);

proc_request(Calls, ReqNo, #ssb_rpc{name = [?room, ?metadata]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    %% When not configured as a room, answer plain `false` so clients don't
    %% treat us as one (matches tildefriends/go-ssb-room).  Otherwise: open
    %% rooms admit anyone; community/restricted rooms report whether the caller
    %% has joined (redeemed an invite — see room_store / invite.use).
    IsMember = room_caller_allowed(Calls),
    Body = case config:is_room() of
        false ->
            iolist_to_binary(message:ssb_encoder(false, fun message:ssb_encoder/3, [pretty]));
        true ->
            utils:encode_rec({[{~"name", config:room_name()},
                               {~"membership", IsMember},
                               {~"features", [?tunnel, ~"room1", ~"room2"]}]})
    end,
    %% async reply: stream=0,end=0 (see tunnel.isRoom note). With stream/end set,
    %% the client's room.metadata() promise never resolves and it never goes on
    %% to subscribe room.attendants.
    Flags = create_flags(0, 0, 2),
    %% PROBE: caller, what we report, and the exact reply bytes. If manyverse
    %% stays on the 2.0 path you should see this (and NOT tunnel.isRoom) per
    %% connection; the body here is what feeds room-observer's features.
    ?SSB_INFO("ROOMDBG room.metadata from=~p membership=~p privacy=~p "
              "flags=~p reply=~s~n",
              [caller_feed_id(Calls), IsMember, config:room_privacy(),
               Flags, Body]),
    Header = create_header(Flags, size(Body), -ReqNo),
    utils:send_data(utils:combine(Header, Body), Socket, Nonce, SecretBoxKey);

proc_request(Calls, ReqNo, #ssb_rpc{name = [?tunnel, ?connect], args = Args}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    %% PROBE: this is the payoff — a peer asking us to relay to `target`. If you
    %% never see this when tapping an attendant in manyverse, the client never
    %% got a usable attendant from the steps above.
    ?SSB_INFO("ROOMDBG tunnel.connect from=~p (is_room=~p) args=~p~n",
              [caller_feed_id(Calls), config:is_room(), Args]),
    case config:is_room() of
        true  -> tunnel_relay_connect(Calls, ReqNo, Args, Socket, Nonce, SecretBoxKey);
        false -> tunnel_accept_connect(Calls, ReqNo, Args, Socket, Nonce, SecretBoxKey)
    end;

proc_request(Calls, ReqNo, #ssb_rpc{name = [?room, ?attendants]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    %% Send the current presence snapshot; subsequent joined/left updates are
    %% pushed by ssb_peer (see the {attendants_stream, ReqNo} dispatch tag).
    ets:insert(Calls, {ReqNo, noop}),
    Ids = other_attendants(Calls),
    %% PROBE: who subscribed and the snapshot they get. An empty/short list when
    %% you expect peers is a presence problem, not a detection one. Watch for
    %% follow-up "room_event joined" pushes too.
    ?SSB_INFO("ROOMDBG room.attendants subscribe from=~p snapshot_ids=~p~n",
              [caller_feed_id(Calls), Ids]),
    Body = utils:encode_rec({[{~"type", ~"state"}, {~"ids", Ids}]}),
    Flags = create_flags(1, 0, 2),
    Header = create_header(Flags, size(Body), -ReqNo),
    utils:send_data(utils:combine(Header, Body), Socket, Nonce, SecretBoxKey);

proc_request(Calls, ReqNo, #ssb_rpc{name = [?tunnel, ?endpoints]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    %% Rooms 1.0 presence: a source stream whose values are the full array of
    %% attendant ids (vs room.attendants' {type,id} deltas).  ssb_peer re-emits
    %% the updated array on every join/leave (see {endpoints_stream, ReqNo}).
    ets:insert(Calls, {ReqNo, noop}),
    Ids = other_attendants(Calls),
    ?SSB_INFO("ROOMDBG tunnel.endpoints subscribe from=~p snapshot_ids=~p~n",
              [caller_feed_id(Calls), Ids]),
    Body = utils:encode_rec(Ids),
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

proc_request(_Calls, ReqNo, #ssb_rpc{name = [~"invite", ~"create"],
                             args = [Host, Port]}
             = _ReqBody, Socket, Nonce, SecretBoxKey) ->
    {ok, Code} = invite:create(binary_to_list(Host), Port),
    Body = iolist_to_binary(message:ssb_encoder({[{~"invite", Code}]},
                              fun message:ssb_encoder/3, [])),
    Flags  = create_flags(0, 0, 2),   %% async reply: stream=0
    Header = create_header(Flags, size(Body), -ReqNo),
    utils:send_data(utils:combine(Header, Body), Socket, Nonce, SecretBoxKey);

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
            %% A room grants membership (same invite bootstrap, different
            %% post-handshake effect); a pub posts a follow of the new user.
            case config:is_room() of
                true ->
                    ?SSB_INFO("ROOMDBG invite.use granted room membership to ~p~n", [FeedId]),
                    ok = room_store:add_member(FeedId);
                false ->
                    OurId   = keys:pub_key_disp(),
                    FeedPid = utils:find_or_create_feed_pid(OurId),
                    FollowContent = {[{~"type",      ~"contact"},
                                      {~"contact",   FeedId},
                                      {~"following", true},
                                      {~"pub",       true}]},
                    ok = ssb_feed:post_content(FeedPid, FollowContent)
            end,
            %% The new member/follow just changed our replication set; refresh
            %% it now so we replicate their feed immediately rather than after
            %% the next periodic (20s) recompute.
            ok = ebt:refresh_repl_set(),
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

proc_request(Calls, ReqNo, #ssb_rpc{name = Name, args = Args} = ReqBody,
             Socket, Nonce, SecretBoxKey) when is_list(Name) ->
    %% Not handled above: consult the plugin registry before giving up.
    %% Permission was already checked at the dispatch/6 choke point.
    case plugin_registry:lookup(Name) of
        {ok, {Mod, Kind, _Perm}} ->
            plugin_dispatch(Mod, Kind, Name, Args, caller_info(Calls), Calls,
                            ReqNo, Socket, Nonce, SecretBoxKey);
        _ ->  %% unknown, or a builtin whose args didn't match its clause
            unhandled_request(Calls, ReqNo, ReqBody, Socket, Nonce, SecretBoxKey)
    end;

proc_request(Calls, ReqNo, ReqBody, Socket, Nonce, SecretBoxKey) ->
    unhandled_request(Calls, ReqNo, ReqBody, Socket, Nonce, SecretBoxKey).

unhandled_request(Calls, ReqNo, ReqBody, Socket, Nonce, SecretBoxKey) ->
    ?SSB_INFO("ROOMDBG unhandled RPC (fall thru): ~p~n", [ReqBody]),
    ets:insert(Calls, {ReqNo, noop}),
    Flags = create_flags(1, 1, 2),
    TrueEnd = message:ssb_encoder(true, fun message:ssb_encoder/3, [pretty]),
    Header = create_header(Flags, size(TrueEnd), -ReqNo),
    utils:send_data(utils:combine(Header, TrueEnd), Socket, Nonce, SecretBoxKey).

%% Dispatch to a registered plugin and frame its result.  sync/async
%% methods answer one JSON value (stream=0); source methods emit one
%% frame per item then close with JSON true, like createHistoryStream.
%% A crashing plugin answers an error frame instead of killing the
%% connection's rpc_processor.
plugin_dispatch(Mod, Kind, Name, Args, Caller, Calls,
                ReqNo, Socket, Nonce, SecretBoxKey) ->
    Result = try Mod:handle_rpc(Name, Args, Caller)
             catch C:R:Stack ->
                     ?SSB_ERROR("plugin ~p crashed on ~p: ~p:~p ~p",
                                [Mod, Name, C, R, Stack]),
                     {error, ~"internal error"}
             end,
    case {Kind, Result} of
        {source, {source, Items}} ->
            ets:insert(Calls, {ReqNo, noop}),
            N1 = send_source_items(Items, ReqNo, Socket, Nonce, SecretBoxKey),
            TrueEnd = encode_json(true),
            Header = create_header(create_flags(1, 1, 2), size(TrueEnd), -ReqNo),
            utils:send_data(utils:combine(Header, TrueEnd), Socket, N1, SecretBoxKey);
        {source, {live_source, Pairs, ViewMod, EventFun}} ->
            %% snapshot now, then keep the stream open: a view_stream
            %% pushes later matches via the connection's ordered writes
            case owner(Calls) of
                undefined ->
                    %% no owning ssb_peer (e.g. unit tests): finite fallback
                    ets:insert(Calls, {ReqNo, noop}),
                    N0 = send_source_items([{json, B} || {_, B} <- Pairs],
                                           ReqNo, Socket, Nonce, SecretBoxKey),
                    TrueEnd = encode_json(true),
                    Header = create_header(create_flags(1, 1, 2),
                                           size(TrueEnd), -ReqNo),
                    utils:send_data(utils:combine(Header, TrueEnd),
                                    Socket, N0, SecretBoxKey);
                OwnerPid ->
                    {ok, Stream} = view_stream:start(OwnerPid, -ReqNo,
                                                     ViewMod, EventFun),
                    ets:insert(Calls, {ReqNo, {live_source, Stream}}),
                    N1 = send_source_items([{json, B} || {_, B} <- Pairs],
                                           ReqNo, Socket, Nonce, SecretBoxKey),
                    ok = view_stream:release(Stream, [Id || {Id, _} <- Pairs]),
                    N1
            end;
        {_, {reply, Term}} when Kind =:= sync; Kind =:= async ->
            Body = encode_json(Term),
            Header = create_header(create_flags(0, 0, 2), size(Body), -ReqNo),
            utils:send_data(utils:combine(Header, Body), Socket, Nonce, SecretBoxKey);
        {_, {error, Reason}} when is_binary(Reason) ->
            rpc_error(ReqNo, Reason, Socket, Nonce, SecretBoxKey);
        {_, Other} ->
            ?SSB_ERROR("plugin ~p returned ~p for ~p method ~p",
                       [Mod, Other, Kind, Name]),
            rpc_error(ReqNo, ~"internal error", Socket, Nonce, SecretBoxKey)
    end.

send_source_items(Items, ReqNo, Socket, Nonce, SecretBoxKey) ->
    lists:foldl(
      fun(Item, N) ->
              Body = encode_json(Item),
              Header = create_header(create_flags(1, 0, 2),
                                     size(Body), -ReqNo),
              utils:send_data(utils:combine(Header, Body),
                              Socket, N, SecretBoxKey)
      end, Nonce, Items).

rpc_error(ReqNo, Reason, Socket, Nonce, SecretBoxKey) ->
    ErrMsg = utils:error_msg(~"Error", Reason),
    Flags  = create_flags(0, 1, 2),
    Header = create_header(Flags, size(ErrMsg), -ReqNo),
    utils:send_data(utils:combine(Header, ErrMsg), Socket, Nonce, SecretBoxKey).

%% {json, Bin} is pre-encoded JSON passed through untouched — used by
%% plugins that serve stored message bytes (see ssb_plugin).
encode_json({json, Bin}) when is_binary(Bin) ->
    Bin;
encode_json(Term) ->
    iolist_to_binary(message:ssb_encoder(Term, fun message:ssb_encoder/3, [pretty])).

%% The caller's identity and permission class, from the remote key the
%% handshake authenticated: the node's own key = a local client (owner);
%% a registered room member = member; anyone else = peer.  Computed per
%% dispatch because membership can change mid-connection (invite.use).
caller_info(Calls) ->
    FeedId = caller_feed_id(Calls),
    Class = case ets:lookup(Calls, remote_pk) of
        [{remote_pk, Pk}] ->
            %% keys:pub_key() is base64 text; remote_pk is the raw key
            %% the handshake authenticated.
            case base64:decode(keys:pub_key()) of
                Pk -> owner;
                _  ->
                    case room_store:is_member(FeedId) of
                        true  -> member;
                        false -> peer
                    end
            end;
        [] -> peer
    end,
    #{feed_id => FeedId, class => Class}.

%% Stream BlobData in 65 536-byte muxrpc frames, then close with JSON true.
%% Each frame is further split into <=4096-byte box-stream packets by
%% boxstream:box (the box-stream length field is only 2 bytes wide).
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
%% Membership gating happens at the dispatch choke point (the method's
%% registry perm is `room`), not here.
tunnel_relay_connect(Calls, ReqNo, Args, Socket, Nonce, SecretBoxKey) ->
    Target = tunnel_target(Args),
    case Target =/= undefined andalso room_attendants:lookup(Target) of
        {ok, TargetPid} ->
            OwnerPid = owner(Calls),
            {ok, Bridge} = tunnel_bridge:start(OwnerPid, ReqNo, TargetPid, Args),
            ets:insert(Calls, {ReqNo, {tunnel_relay, Bridge}}),
            %% PROBE: relay accepted — target was found and a bridge spawned.
            ?SSB_INFO("ROOMDBG relay target=~p FOUND pid=~p bridge=~p reqno=~p~n",
                      [Target, TargetPid, Bridge, ReqNo]),
            Nonce;
        _ ->
            %% PROBE: relay rejected — target not in room_attendants. If you see
            %% this, the attendant disconnected (or never registered) between the
            %% snapshot and the connect; manyverse will unstage it.
            ?SSB_INFO("ROOMDBG relay target=~p NOT connected; sending error~n",
                      [Target]),
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

%% Whether the connection's peer may use this room: open rooms admit anyone;
%% community/restricted rooms require the caller to be a registered member.
room_caller_allowed(Calls) ->
    case config:room_privacy() of
        open -> true;
        _    -> room_store:is_member(caller_feed_id(Calls))
    end.

caller_feed_id(Calls) ->
    case ets:lookup(Calls, remote_pk) of
        [{remote_pk, Pk}] -> <<"@", (base64:encode(Pk))/binary, ".ed25519">>;
        []                -> undefined
    end.

%% Current attendants minus the caller: a client should not see itself in its
%% own presence snapshot (room.attendants / tunnel.endpoints).
other_attendants(Calls) ->
    Self = caller_feed_id(Calls),
    [Id || Id <- room_attendants:list(), Id =/= Self].

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
