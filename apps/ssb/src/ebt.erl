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
         handle_data/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile({no_auto_import,[size/1]}).
-import(utils, [size/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Build our initial vector clock: include our own feed with its current
%% last sequence so the peer knows what we already have.
initial_vector() ->
    PeerKey = keys:pub_key_disp(),
    Pid = utils:find_or_create_feed_pid(PeerKey),
    Seq = case ssb_feed:fetch_last_msg(Pid) of
              #message{sequence = S} -> S;
              _ -> 0
          end,
    {[{PeerKey, ebt_vc:encode_clock_int(true, true, Seq)}]}.

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
            ?LOG_DEBUG("EBT: received vector clock ~n", []),
            handle_clock(ReqNo, Decoded, Socket, Nonce, Key);
        false ->
            ?LOG_DEBUG("EBT: received message from peer ~n", [Body]),
            store_message(Body),
            Nonce
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

%% For each feed in the peer's clock, send them any messages they are
%% missing (i.e. messages with sequence > their last known sequence).
handle_clock(ReqNo, {PeerClock}, Socket, Nonce, Key) ->
    lists:foldl(fun({FeedId, EncodedInt}, NonceAcc) ->
                        {_Rcv, _Sync, PeerSeq} = ebt_vc:decode_clock_int(EncodedInt),
                        send_feed_msgs_after(FeedId, PeerSeq, -ReqNo, Socket, NonceAcc, Key)
                end, Nonce, PeerClock).

%% Iterate through a feed and send all messages with sequence > AfterSeq.
%% The raw bytes from foldl are already in the {key, value, timestamp}
%% JSON format that EBT expects, so we send them directly.
send_feed_msgs_after(FeedId, AfterSeq, OutReqNo, Socket, Nonce, Key) ->
    Pid = utils:find_or_create_feed_pid(FeedId),
    case Pid of
        bad ->
            Nonce;
        _ ->
            ssb_feed:foldl(Pid,
                           fun(MsgData, NonceAcc) ->
                                   #message{sequence = Seq} =
                                       message:decode(MsgData, false),
                                   case Seq > AfterSeq of
                                       true ->
                                           send_msg_data(MsgData, OutReqNo,
                                                         Socket, NonceAcc, Key);
                                       false ->
                                           NonceAcc
                                   end
                           end, Nonce)
    end.

%% Send a single raw message binary over the duplex stream.
send_msg_data(MsgData, OutReqNo, Socket, Nonce, Key) ->
    Flags = rpc_processor:create_flags(1, 0, 2),
    Header = rpc_processor:create_header(Flags, size(MsgData), OutReqNo),
    utils:send_data(utils:combine(Header, MsgData), Socket, Nonce, Key).

%% Decode an incoming message and store it in the appropriate feed.
%% Body is the raw JSON bytes in {key, value, timestamp} format.
store_message(Body) ->
    try
        Msg = message:decode(Body, false),
        case utils:find_or_create_feed_pid(Msg#message.author) of
            bad ->
                ?LOG_INFO("EBT: bad author in received message: ~p~n",
                          [Msg#message.author]);
            Pid ->
                ssb_feed:store_msg(Pid, Msg)
        end
    catch
        error:Reason ->
            ?LOG_INFO("EBT: failed to decode/store message: ~p~n", [Reason])
    end.
