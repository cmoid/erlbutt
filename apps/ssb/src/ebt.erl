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
         replicate_feed/1,
         refresh_repl_set/0,
         handle_data/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile({no_auto_import,[size/1]}).
-import(utils, [size/1]).

-define(SERVER, ?MODULE).

%% Cached replication set: the feeds we will store and serve.  Recomputed
%% from the follow graph + room members − blocks, on a timer and on demand.
-define(REPL_SET, ebt_repl_set).
-define(REPL_REFRESH_MS, 20000).

%% ebt is both a gen_server (supervised singleton in ssb_sup) and an
%% rpc_behavior (callbacks invoked per-connection by each rpc_processor).
%% The gen_server carries no state; all connection context arrives via args.
-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

initial_vector() ->
    full_clock().

%% Build a full vector clock from the feeds we hold that are within our
%% replication set.  Each entry is {FeedId, encoded_int} with receive=true so
%% the peer pushes us updates.
full_clock() ->
    Entries = ets:tab2list(ssb_feed_registry),
    {[{FeedId, clock_entry_for(Pid)}
      || {FeedId, Pid} <- Entries,
         is_process_alive(Pid),
         replicate_feed(FeedId)]}.

%% Whether we replicate (store/serve) FeedId: our own feed, anyone within our
%% follow horizon, or an explicit room member — minus anyone we block.  Reads
%% the cached set; if the table is absent (ebt not started, e.g. isolated
%% eunit) it fails open so unrelated paths keep working.
replicate_feed(FeedId) ->
    try ets:member(?REPL_SET, FeedId)
    catch error:badarg -> true
    end.

%% Recompute the cached replication set now (e.g. after a new follow/block or
%% room member). Synchronous so callers/tests see the effect immediately.
refresh_repl_set() ->
    gen_server:call(?SERVER, refresh_repl_set).

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
                    ?SSB_DEBUG("EBT: received message from peer ~n", []),
                    case store_message(Body) of
                        {ok, FeedId, Seq} ->
                            send_clock_ack(FeedId, Seq, -ReqNo, Socket, Nonce, Key);
                        _ ->
                            %% skipped (duplicate) or error: no ack.
                            Nonce
                    end
            end
    end.

init([]) ->
    process_flag(trap_exit, true),
    ets:new(?REPL_SET, [set, named_table, public]),
    recompute_repl_set(),
    schedule_repl_refresh(),
    {ok, #state{}}.

handle_call(refresh_repl_set, _From, State) ->
    recompute_repl_set(),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(refresh_repl_set, State) ->
    recompute_repl_set(),
    schedule_repl_refresh(),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

schedule_repl_refresh() ->
    erlang:send_after(?REPL_REFRESH_MS, self(), refresh_repl_set).

%% Replication set = {self} ∪ follows(self, hops) ∪ room members − blocks(self).
%% A block wins even over membership.
recompute_repl_set() ->
    try
        Self    = keys:pub_key_disp(),
        Follows = friends:follows(Self, config:replication_hops()),
        Members = room_store:members(),
        Blocked = friends:blocks(Self),
        Combined = lists:usort([Self | Follows] ++ Members),
        Set = [F || F <- Combined, not lists:member(F, Blocked)],
        ets:delete_all_objects(?REPL_SET),
        [ets:insert(?REPL_SET, {F}) || F <- Set],
        ok
    catch Class:Reason ->
        ?SSB_INFO("EBT: repl set recompute failed: ~p~n", [{Class, Reason}]),
        ok
    end.

%% A vector clock's top-level keys all start with "@" (feed IDs).
%% A message's keys are "key", "value", "timestamp".
is_vector_clock({[]}) -> true;
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
send_feed_msgs_after(FeedId, {true, true, AfterSeq}, OutReqNo, Socket, Nonce, Key) ->
    case replicate_feed(FeedId) of
        false -> Nonce;
        true  -> send_feed_msgs_after_ok(FeedId, AfterSeq, OutReqNo, Socket, Nonce, Key)
    end.

send_feed_msgs_after_ok(FeedId, AfterSeq, OutReqNo, Socket, Nonce, Key) ->
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
        case replicate_feed(Msg#message.author) of
            false ->
                ?SSB_DEBUG("EBT: dropping msg for non-replicated feed ~p~n",
                           [Msg#message.author]),
                error;
            true ->
                case utils:find_or_create_feed_pid(Msg#message.author) of
                    bad ->
                        ?SSB_INFO("EBT: bad author in received message: ~p~n",
                            [{Msg#message.author, Msg#message.id}]),
                        error;
                    Pid ->
                        case ssb_feed:store_msg(Pid, Msg) of
                            stored ->
                                {ok, Msg#message.author, Msg#message.sequence};
                            _ ->
                                %% Duplicate (already have this seq): don't ack,
                                %% so we don't re-invite the peer to resend.
                                skipped
                        end
                end
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

    process_flag(trap_exit, true),
    exit(whereis(ssb_feed_sup), shutdown),
    receive {'EXIT', _, _} -> ok after 1000 -> ok end,
    gen_server:stop(keys),
    gen_server:stop(config).

%% Gate logic: the replication set admits our own feed, direct follows and
%% room members, rejects strangers, and lets a block win even over membership.
repl_set_test_() ->
    {setup, fun rs_setup/0, fun rs_cleanup/1, fun(_) -> ?_test(repl_set_logic()) end}.

rs_setup() ->
    catch gen_server:stop(ebt),
    catch gen_server:stop(room_store),
    catch gen_server:stop(friends),
    catch gen_server:stop(keys),
    catch gen_server:stop(config),
    Home = filename:join("/tmp", "ebt_rs_"
                         ++ integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("test/ssb.cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = friends:start_link(),
    {ok, _} = room_store:start_link(),
    {ok, _} = ebt:start_link(),
    Home.

rs_cleanup(Home) ->
    catch gen_server:stop(ebt),
    catch gen_server:stop(room_store),
    catch gen_server:stop(friends),
    catch gen_server:stop(keys),
    catch gen_server:stop(config),
    os:cmd("rm -rf " ++ Home),
    application:unset_env(ssb, ssb_home),
    ok.

rs_fresh_feed() ->
    #{public := Pub} = enacl:sign_keypair(),
    <<"@", (base64:encode(Pub))/binary, ".ed25519">>.

repl_set_logic() ->
    Self     = keys:pub_key_disp(),
    Follow   = rs_fresh_feed(),
    Member   = rs_fresh_feed(),
    Blocked  = rs_fresh_feed(),
    Stranger = rs_fresh_feed(),
    %% Seed the follow/block graphs directly and add a room member.
    ets:insert(ssb_follow_graph, {Self, #{Follow => true}}),
    ets:insert(ssb_block_graph,  {Self, #{Blocked => true}}),
    ok = room_store:add_member(Member),
    ok = refresh_repl_set(),
    ?assert(replicate_feed(Self)),         %% our own feed
    ?assert(replicate_feed(Follow)),       %% a direct follow
    ?assert(replicate_feed(Member)),       %% a room member
    ?assertNot(replicate_feed(Blocked)),   %% blocked
    ?assertNot(replicate_feed(Stranger)),  %% neither followed nor a member
    %% A block wins even over membership.
    ets:insert(ssb_block_graph, {Self, #{Blocked => true, Member => true}}),
    ok = refresh_repl_set(),
    ?assertNot(replicate_feed(Member)).

-endif.
