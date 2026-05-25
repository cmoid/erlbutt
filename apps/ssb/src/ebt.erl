%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2025 Charles Moid
-module(ebt).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("ssb/include/ssb.hrl").

-behaviour(gen_server).
-behavior(rpc_behavior).

%% API
-export([start_link/0,
         initial_vector/0,
         full_clock/0,
         handle_data/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile({no_auto_import,[size/1]}).
-import(utils, [size/1]).

-define(SERVER, ?MODULE).

%% ebt is both a gen_server (supervised singleton in ssb_sup) and an
%% rpc_behavior (callbacks invoked per-connection by each rpc_processor).
%% The gen_server carries no state; all connection context arrives via args.
-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

initial_vector() ->
    full_clock().

%% Build a full vector clock from all feeds in the local registry.
%% Each entry is {FeedId, encoded_int} with receive=true so the peer will push us updates.
full_clock() ->
    Entries = ets:tab2list(ssb_feed_registry),
    {[{FeedId, clock_entry_for(Pid)}
      || {FeedId, Pid} <- Entries,
         is_process_alive(Pid)]}.

%% Called by rpc_processor for each subsequent message on an open EBT
%% duplex stream (after the initial ebt.replicate handshake).
%%
%% The peer sends either:
%%   - a vector clock (JSON object keyed by @feed.ed25519 IDs)
%%   - a feed message (JSON with "key"/"value"/"timestamp")
handle_data(ReqNo, Body, #ssb_conn{socket = Socket,
                                    nonce = Nonce,
                                    secret_box = Key}) ->
    Decoded = utils:nat_decode(Body),
    case is_vector_clock(Decoded) of
        true ->
            ?SSB_DEBUG("EBT: received vector clock ~n", []),
            handle_clock(ReqNo, Decoded, Socket, Nonce, Key);
        false ->
            case is_peer_error(Decoded) of
                true ->
                    Nonce;
                false ->
                    ?SSB_DEBUG("EBT: received message from peer ~p ~n", [Body]),
                    case store_message(Body) of
                        {ok, FeedId, Seq} ->
                            send_clock_ack(FeedId, Seq, -ReqNo, Socket, Nonce, Key);
                        error ->
                            Nonce
                    end
            end
    end.

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
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

%% A vector clock's top-level keys all start with "@" (feed IDs).
%% A message's keys are "key", "value", "timestamp".
is_vector_clock({[{<<$@, _/binary>>, _} | _]}) -> true;
is_vector_clock(_) -> false.

%% Peer error responses (e.g. PW's "reconnected to peer" JSON) have a "name"
%% key but no "author" key.  Discard them silently — they are the remote
%% peer's internal diagnostics, not SSB data.
is_peer_error({Props}) when is_list(Props) ->
    lists:keymember(~"name", 1, Props) andalso
        not lists:keymember(~"author", 1, Props);
is_peer_error(_) -> false.

%% For each feed in the peer's clock, send them any messages they are
%% missing (i.e. messages with sequence > their last known sequence).
handle_clock(ReqNo, {PeerClock}, Socket, Nonce, Key) ->
    {NewNonce, Cnt} =
    lists:foldl(fun({FeedId, EncodedInt}, {NonceAcc, Fc}) ->
                        check_feed_cnt(Fc),
                        {Rep, Rec, PeerSeq} = ebt_vc:decode_clock_int(EncodedInt),
                        ?SSB_DEBUG("EBT: decode vector to ~p for feed ~p ~n", [{Rep, Rec, PeerSeq},
                            {FeedId, EncodedInt}]),
                        {send_feed_msgs_after(FeedId, {Rep, Rec,PeerSeq}, -ReqNo, Socket, NonceAcc, Key), Fc + 1}
                end, {Nonce, 0}, PeerClock),
    ?SSB_DEBUG("EBT: handle_clock: processed ~p clocks ~n", [Cnt]),
    NewNonce.

send_feed_msgs_after(_, {false, _, _}, _, _, Nonce, _) -> Nonce;

send_feed_msgs_after(_, {true, false, _}, _, _, Nonce, _) -> Nonce;

%% Iterate through a feed and send all messages with sequence > AfterSeq.
%% Each message is re-encoded: only the "value" field is sent, not the full
%% {key, value, timestamp} envelope stored on disk.
send_feed_msgs_after(FeedId, {true, true,AfterSeq}, OutReqNo, Socket, Nonce, Key) ->
    Pid = utils:find_or_create_feed_pid(FeedId),
    case Pid of
        bad ->
            Nonce;
        _ ->
            ssb_feed:foldl(Pid,
                           fun(MsgData, NonceAcc) ->
                                   try
                                       #message{sequence = Seq} =
                                           message:decode(MsgData, false),
                                       case Seq > AfterSeq of
                                           true ->
                                               send_msg_data(MsgData, OutReqNo,
                                                             Socket, NonceAcc, Key);
                                           false ->
                                               NonceAcc
                                       end
                                   catch
                                       _:Err ->
                                           ?SSB_INFO("EBT: skipping bad stored msg: ~p~n", [Err]),
                                           NonceAcc
                                   end
                           end, Nonce)
    end.

%% Send a single raw message binary over the duplex stream.
%% Only the "value" object is sent (not the full {key,value,timestamp} envelope),
%% consistent with how createHistoryStream works with keys:false.
send_msg_data(MsgData, OutReqNo, Socket, Nonce, Key) ->
    {PropList} = utils:nat_decode(MsgData),
    SendData = iolist_to_binary(
                   message:ssb_encoder(proplists:get_value(~"value", PropList),
                                       fun message:ssb_encoder/3, [pretty, use_nil])),
    ?SSB_DEBUG("EBT: sending msg ~p to output req ~p~n", [SendData, OutReqNo]),
    Flags = rpc_processor:create_flags(1, 0, 2),
    Header = rpc_processor:create_header(Flags, size(SendData), OutReqNo),
    utils:send_data(utils:combine(Header, SendData), Socket, Nonce, Key).

%% Send a single-feed clock acknowledgment back to the peer after storing a message.
%% This tells the peer we have the feed up to Seq and still want to receive more.
send_clock_ack(FeedId, Seq, OutReqNo, Socket, Nonce, Key) ->
    Ack = utils:encode_rec({[{FeedId, ebt_vc:encode_clock_int(true, true, Seq)}]}),
    Flags = rpc_processor:create_flags(1, 0, 2),
    Header = rpc_processor:create_header(Flags, size(Ack), OutReqNo),
    utils:send_data(utils:combine(Header, Ack), Socket, Nonce, Key).

clock_entry_for(Pid) ->
    Seq = case ssb_feed:fetch_last_msg(Pid) of
              #message{sequence = S} -> S;
              _ -> 0
          end,
    ebt_vc:encode_clock_int(true, true, Seq).

%% Decode an incoming message and store it in the appropriate feed.
%% Body is the value-only JSON (no key/timestamp wrapper), as sent by EBT
%% and createHistoryStream with keys:false.
%% Returns {ok, FeedId, Seq} on success so the caller can send a clock ack.
store_message(Body) ->
    try
        Msg = message:decode_value(Body, true),
        case utils:find_or_create_feed_pid(Msg#message.author) of
            bad ->
                ?SSB_INFO("EBT: bad author in received message: ~p~n",
                    [{Msg#message.author, Msg#message.id}]),
                error;
            Pid ->
                ssb_feed:store_msg(Pid, Msg),
                {ok, Msg#message.author, Msg#message.sequence}
        end
    catch
        _:Reason ->
            ?SSB_INFO("EBT: failed to decode/store message: ~p~n", [Reason]),
            error
    end.

check_feed_cnt(Cnt) when Cnt rem 1000 =:= 0 ->
    ?SSB_DEBUG("EBT: feeds processed: ~p~n", [Cnt]);
check_feed_cnt(_) ->
    ok.

-ifdef(TEST).

full_clock_test() ->
    config:start_link("test/ssb.cfg"),
    keys:start_link(),
    ssb_feed_sup:start_link(),

    %% Register three feeds in the local registry.
    {PubA, _} = utils:create_key_pair(),
    {PubB, _} = utils:create_key_pair(),
    FeedA = utils:display_pub(PubA),
    FeedB = utils:display_pub(PubB),
    OwnFeed = keys:pub_key_disp(),
    _ = utils:find_or_create_feed_pid(FeedA),
    _ = utils:find_or_create_feed_pid(FeedB),
    _ = utils:find_or_create_feed_pid(OwnFeed),

    {Clock} = full_clock(),
    ?assert(is_list(Clock)),

    %% All three feeds must appear in the clock.
    ?assert(lists:keymember(FeedA, 1, Clock)),
    ?assert(lists:keymember(FeedB, 1, Clock)),
    ?assert(lists:keymember(OwnFeed, 1, Clock)),

    %% Every entry must decode to {replicate=true, receive=true, seq>=0}.
    lists:foreach(fun({_Id, Enc}) ->
        {true, true, Seq} = ebt_vc:decode_clock_int(Enc),
        ?assert(Seq >= 0)
    end, Clock),

    gen_server:stop(keys),
    gen_server:stop(config).

-endif.
