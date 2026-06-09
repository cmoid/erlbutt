%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Periodically dials known peers from two sources:
%%   - heartbeat: LAN peers discovered via UDP broadcast
%%   - conn_db: pub-announced peers with autoconnect: true
%%
%% Before dialing, checks peer_registry to avoid opening a duplicate
%% connection to a peer that has already connected to us inbound.
-module(peer_dialer).

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

-export([start_link/0,
         trigger/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(INITIAL_DELAY_MS, 5_000).
-define(POLL_MS,         30_000).
-define(MAX_CONNS,           10).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Kick an immediate dial pass — call when a new peer is discovered.
trigger() ->
    ?MODULE ! poll,
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    erlang:send_after(?INITIAL_DELAY_MS, self(), poll),
    {ok, #{}}.

handle_info(poll, State) ->
    dial_candidates(),
    erlang:send_after(?POLL_MS, self(), poll),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

dial_candidates() ->
    Candidates = dedup(lan_candidates() ++ known_candidates()),
    ?SSB_DEBUG("peer_dialer: ~p candidate(s), ~p connected~n",
               [length(Candidates), length(peer_registry:all())]),
    lists:foreach(fun maybe_dial/1, Candidates).

%% Gather {Host, Port, RawPubKey} triples from LAN heartbeat peers.
lan_candidates() ->
    lists:filtermap(
        fun({_Ip, nokey}) -> false;
           ({Ip, KeyB64}) ->
                try {true, {Ip, 8008, base64:decode(KeyB64)}}
                catch _:_ -> false
                end
        end, heartbeat:peers()).

%% Gather candidates from conn_db (pub-announced, autoconnect: true).
known_candidates() ->
    lists:filtermap(
        fun({_Addr, Meta}) ->
            case maps:get(~"autoconnect", Meta, false) of
                true  -> parse_meta(Meta);
                false -> false
            end
        end, maps:to_list(conn_db:all())).

parse_meta(Meta) ->
    Host = maps:get(~"host", Meta, undefined),
    Port = maps:get(~"port", Meta, 8008),
    Key  = maps:get(~"key",  Meta, undefined),
    case {Host, Key} of
        {H, K} when is_binary(H), is_binary(K) ->
            case decode_feed_id(K) of
                error  -> false;
                RawKey -> {true, {H, Port, RawKey}}
            end;
        _ ->
            false
    end.

%% "@base64=.ed25519" → raw 32-byte binary
decode_feed_id(<<"@", Rest/binary>>) ->
    case binary:split(Rest, <<".ed25519">>) of
        [B64, _] ->
            try base64:decode(B64) catch _:_ -> error end;
        _ ->
            error
    end;
decode_feed_id(_) ->
    error.

%% Remove duplicate candidates by raw public key, preserving first occurrence.
dedup(Candidates) ->
    {Uniq, _} = lists:foldl(
        fun({_, _, K} = Cand, {Acc, Seen}) ->
            case sets:is_element(K, Seen) of
                true  -> {Acc, Seen};
                false -> {[Cand | Acc], sets:add_element(K, Seen)}
            end
        end, {[], sets:new()}, Candidates),
    lists:reverse(Uniq).

maybe_dial({Host, Port, RawKey}) ->
    case length(peer_registry:all()) >= ?MAX_CONNS of
        true ->
            ?SSB_DEBUG("peer_dialer: at connection cap, skipping~n", []);
        false ->
            case peer_registry:is_connected(RawKey) of
                true ->
                    ok;
                false ->
                    ?SSB_INFO("peer_dialer: dialing ~p:~p~n", [Host, Port]),
                    case ssb_peer:start(Host, Port, RawKey) of
                        {ok, Pid} ->
                            ssb_peer:request_ebt(Pid);
                        {error, Reason} ->
                            ?SSB_INFO("peer_dialer: dial failed ~p:~p reason ~p~n",
                                      [Host, Port, Reason])
                    end
            end
    end.
