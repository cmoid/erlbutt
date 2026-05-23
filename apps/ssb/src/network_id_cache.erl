%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Bounded LRU cache: peer public key → the network ID that last worked
%% for that peer.  Used by ssb_peer to order network ID candidates on
%% connect so the most likely match is tried first.
%%
%% Reads are direct ETS lookups (no gen_server round-trip).
%% Writes go through the gen_server to serialise eviction.
-module(network_id_cache).

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/0,
         lookup/1,
         record/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {capacity}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Returns {ok, NetworkId} if a cached entry exists for PeerPubKey,
%% or miss if none is known.  Direct ETS read — no gen_server hop.
lookup(PeerPubKey) ->
    case ets:lookup(?MODULE, PeerPubKey) of
        [{PeerPubKey, NetId, _TS}] -> {ok, NetId};
        []                         -> miss
    end.

%% Record that NetworkId worked for PeerPubKey.  Fire-and-forget.
record(PeerPubKey, NetworkId) ->
    gen_server:cast(?SERVER, {record, PeerPubKey, NetworkId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?MODULE, [named_table, set, public, {read_concurrency, true}]),
    Capacity = application:get_env(ssb, network_id_cache_size, ?DEFAULT_CACHE_CAPACITY),
    {ok, #state{capacity = Capacity}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({record, PeerPubKey, NetworkId}, #state{capacity = Cap} = State) ->
    TS = erlang:monotonic_time(),
    ets:insert(?MODULE, {PeerPubKey, NetworkId, TS}),
    case ets:info(?MODULE, size) > Cap of
        true  -> evict_oldest();
        false -> ok
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

evict_oldest() ->
    {OldestKey, _} = ets:foldl(
        fun({Key, _NetId, TS}, {_AccKey, AccTS} = Acc) ->
            if TS < AccTS -> {Key, TS};
               true       -> Acc
            end
        end,
        {undefined, infinity},
        ?MODULE),
    case OldestKey of
        undefined -> ok;
        Key       -> ets:delete(?MODULE, Key)
    end.

-ifdef(TEST).

setup() ->
    case whereis(?MODULE) of
        undefined -> {ok, Pid} = start_link(), Pid;
        Pid       -> Pid
    end.

teardown(_Pid) ->
    ets:delete_all_objects(?MODULE).

cache_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [fun miss_on_unknown_key/1,
      fun record_and_lookup/1,
      fun overwrite_same_key/1]}.

miss_on_unknown_key(_) ->
    fun() ->
        ?assertEqual(miss, lookup(crypto:strong_rand_bytes(32)))
    end.

record_and_lookup(_) ->
    fun() ->
        Key   = crypto:strong_rand_bytes(32),
        NetId = crypto:strong_rand_bytes(32),
        record(Key, NetId),
        timer:sleep(10),
        ?assertEqual({ok, NetId}, lookup(Key))
    end.

overwrite_same_key(_) ->
    fun() ->
        Key    = crypto:strong_rand_bytes(32),
        NetId1 = crypto:strong_rand_bytes(32),
        NetId2 = crypto:strong_rand_bytes(32),
        record(Key, NetId1),
        record(Key, NetId2),
        timer:sleep(10),
        ?assertEqual({ok, NetId2}, lookup(Key))
    end.

%% Standalone test — starts its own cache with capacity 3 to verify eviction.
eviction_test() ->
    %% Stop shared instance if running so we can register the name.
    case whereis(?MODULE) of
        undefined -> ok;
        Pid       -> gen_server:stop(Pid)
    end,
    application:set_env(ssb, network_id_cache_size, 3),
    {ok, _} = start_link(),
    [record(crypto:strong_rand_bytes(32), crypto:strong_rand_bytes(32))
     || _ <- lists:seq(1, 5)],
    timer:sleep(50),
    ?assert(ets:info(?MODULE, size) =< 3),
    gen_server:stop(?MODULE),
    application:unset_env(ssb, network_id_cache_size).

-endif.
