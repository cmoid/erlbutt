%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Persists known SSB peers in conn.json using the same format as JS SSB clients,
%% so the file is compatible with ponchowonky and other clients.
%%
%% Key:   multiserver address, e.g. <<"net:ssb.example.com:8008~shs:base64key=">>
%% Value: peer metadata map (birth, host, port, key, source, announcers, etc.)
-module(conn_db).

-behaviour(gen_server).
-include_lib("ssb/include/ssb.hrl").

-export([start_link/0,
         remember/3,
         all/0,
         flush/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {peers = #{}, file, dirty = false}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Record a peer discovered from a pub message.
%% Addr is the multiserver address binary, Meta is a map of fields from
%% the pub address object, Source is <<"pub">>.
remember(Addr, Meta, Source) ->
    gen_server:cast(?MODULE, {remember, Addr, Meta, Source}).

all() ->
    gen_server:call(?MODULE, all).

flush() ->
    gen_server:call(?MODULE, flush).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    File = ?b2l(config:ssb_repo_loc()) ++ "conn.json",
    Peers = load(File),
    {ok, #state{peers = Peers, file = File}}.

handle_call(all, _From, #state{peers = Peers} = State) ->
    {reply, Peers, State};

handle_call(flush, _From, #state{peers = Peers, file = File} = State) ->
    save(Peers, File),
    {reply, ok, State#state{dirty = false}}.

handle_cast({remember, Addr, Meta, Source},
            #state{peers = Peers, dirty = WasDirty} = State) ->
    Now = erlang:system_time(millisecond),
    Existing = maps:get(Addr, Peers, #{
        ~"birth"       => Now,
        ~"announcers"  => 0,
        ~"duration"    => #{~"mean"  => 0, ~"stdev" => 0,
                               ~"count" => 0, ~"sum"   => 0, ~"sqsum" => 0},
        ~"autoconnect" => true
    }),
    Updated = maps:merge(Existing, Meta#{
        ~"source"      => Source,
        ~"stateChange" => Now,
        ~"announcers"  => maps:get(~"announcers", Existing, 0) + 1
    }),
    case WasDirty of
        false -> erlang:send_after(5000, self(), flush);
        true  -> ok
    end,
    {noreply, State#state{peers = Peers#{Addr => Updated}, dirty = true}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush, #state{peers = Peers, file = File} = State) ->
    save(Peers, File),
    {noreply, State#state{dirty = false}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{peers = Peers, file = File, dirty = true}) ->
    save(Peers, File);
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================
load(File) ->
    case file:read_file(File) of
        {ok, Json} ->
            try json:decode(Json) of
                Map when is_map(Map) -> Map;
                _                    -> #{}
            catch _:_ -> #{}
            end;
        {error, enoent} -> #{}
    end.

save(Peers, File) ->
    file:write_file(File, pretty_json(Peers)).

pretty_json(Map) when is_map(Map) ->
    case maps:size(Map) of
        0 -> <<"{}">>;
        _ ->
            Entries = [peer_entry(K, V) || {K, V} <- lists:sort(maps:to_list(Map))],
            iolist_to_binary(["{\n", lists:join(",\n", Entries), "\n}"])
    end.

peer_entry(K, V) ->
    [<<"    \"">>, K, <<"\": ">>, peer_value(V)].

peer_value(V) when is_map(V) ->
    case maps:size(V) of
        0 -> ~"{}";
        _ ->
            Entries = [field_entry(K, Val) || {K, Val} <- lists:sort(maps:to_list(V))],
            iolist_to_binary(["{\n", lists:join(",\n", Entries), "\n    }"])
    end;
peer_value(V) ->
    json:encode(V).

field_entry(K, V) when is_map(V) ->
    Inner = [inner_entry(IK, IV) || {IK, IV} <- lists:sort(maps:to_list(V))],
    iolist_to_binary(["        \"", K, "\": {\n",
                      lists:join(",\n", Inner),
                      "\n        }"]);
field_entry(K, V) ->
    iolist_to_binary(["        \"", K, "\": ", json:encode(V)]).

inner_entry(K, V) ->
    iolist_to_binary(["            \"", K, "\": ", json:encode(V)]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

conn_db_setup() ->
    ConfigStarted = case whereis(config) of
        undefined -> {ok, _} = config:start_link("test/ssb.cfg"), true;
        _         -> false
    end,
    {ok, Pid} = conn_db:start_link(),
    {Pid, ConfigStarted}.

conn_db_teardown({Pid, ConfigStarted}) ->
    ConnJson = ?b2l(config:ssb_repo_loc()) ++ "conn.json",
    gen_server:stop(Pid),
    file:delete(ConnJson),
    case ConfigStarted of
        true -> gen_server:stop(config);
        false -> ok
    end.

conn_db_test_() ->
    {foreach,
     fun conn_db_setup/0,
     fun conn_db_teardown/1,
     [fun remember_stores_peer/1,
      fun announcers_increments/1,
      fun remember_sets_required_fields/1]}.

remember_stores_peer({_Pid, _}) ->
    fun() ->
        Addr = ~"net:ssb.example.com:8008~shs:abc123=",
        Meta = #{~"host" => ~"ssb.example.com", ~"port" => 8008,
                 ~"key"  => ~"@abc123=.ed25519", ~"type" => ~"pub"},
        conn_db:remember(Addr, Meta, ~"pub"),
        All = conn_db:all(),  %% call/0 acts as a sync barrier for the cast
        ?assert(maps:is_key(Addr, All)),
        Peer = maps:get(Addr, All),
        ?assertEqual(~"ssb.example.com", maps:get(~"host", Peer)),
        ?assertEqual(~"pub",             maps:get(~"source", Peer)),
        ?assert(maps:get(~"autoconnect", Peer))
    end.

announcers_increments({_Pid, _}) ->
    fun() ->
        Addr = ~"net:ssb.test.com:8008~shs:xyz=",
        Meta = #{~"host" => ~"ssb.test.com", ~"port" => 8008,
                 ~"key"  => ~"@xyz=.ed25519", ~"type" => ~"pub"},
        conn_db:remember(Addr, Meta, ~"pub"),
        conn_db:remember(Addr, Meta, ~"pub"),
        Peer = maps:get(Addr, conn_db:all()),
        ?assertEqual(2, maps:get(~"announcers", Peer))
    end.

remember_sets_required_fields({_Pid, _}) ->
    fun() ->
        Addr = ~"net:ssb.fields.com:8008~shs:f1=",
        Meta = #{~"host" => ~"ssb.fields.com", ~"port" => 8008,
                 ~"key"  => ~"@f1=.ed25519", ~"type" => ~"pub"},
        conn_db:remember(Addr, Meta, ~"pub"),
        Peer = maps:get(Addr, conn_db:all()),
        ?assert(is_integer(maps:get(~"birth",       Peer))),
        ?assert(is_integer(maps:get(~"stateChange", Peer))),
        ?assert(is_map(maps:get(~"duration", Peer)))
    end.

-endif.
