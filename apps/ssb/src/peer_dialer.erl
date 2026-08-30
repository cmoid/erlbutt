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
         trigger/0,
         enable/0,
         disable/0,
         apply_enabled/1,
         is_enabled/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(INITIAL_DELAY_MS, 5_000).
-define(POLL_MS,         30_000).
-define(MAX_CONNS,           10).

%% Wait after the first failed dial, doubling per attempt to the ceiling.
%% The cap is hours rather than permanent on purpose: a pub that is down
%% for a week has to be able to come back on its own.  A dead address is
%% not meant to be forgotten, only to stop costing a connect timeout and
%% an SHS retry loop on every pass.
-define(BACKOFF_BASE_MS,    300_000).      %% 5 minutes
-define(BACKOFF_MAX_MS,  21_600_000).      %% 6 hours
%% Retry rows untouched for this long are dropped: the address has left
%% conn.json, or has not been a candidate in months.
-define(ROW_TTL_MS,   2_592_000_000).      %% 30 days

-define(SCHEMA_VERSION, 1).
%% Retry state gets its own table rather than extra fields on the conn.json
%% entry.  That file is the JS-client-compatible format shared with
%% patchX, so erlbutt-only bookkeeping written into it would show up
%% in every other client that reads the file.
%%
%% last_ok is never read by the dial decision — it exists so an operator
%% can tell "down since Tuesday" from "has never once completed a
%% handshake", which are the same row until you look.  It carries a
%% DEFAULT so a later version bump can add columns to an existing table.
-define(DDL,
        ["CREATE TABLE IF NOT EXISTS peer_dial_try("
         "  addr     TEXT PRIMARY KEY,"
         "  attempts INTEGER NOT NULL,"
         "  last_try INTEGER NOT NULL,"
         "  last_ok  INTEGER NOT NULL DEFAULT 0) WITHOUT ROWID;"]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Kick an immediate dial pass — call when a new peer is discovered.
%% Safe before peer_dialer has started (it is one of the last children to
%% boot, while heartbeat — which calls this on every LAN broadcast — is one of
%% the first): a bare `!` to an unregistered name is a badarg that would crash
%% the caller, so guard on whereis/1.
trigger() ->
    case whereis(?MODULE) of
        undefined -> ok;
        Pid       -> Pid ! poll, ok
    end.

%% Turn automatic dialing on (kicks an immediate pass) or off.  The poll
%% timer keeps running while disabled; passes are skipped.
%%
%% Goes through config so the choice OUTLIVES A RESTART.  These used to
%% flip only the running server, which meant a pub turned on by hand came
%% back off after the next upgrade — visible weeks later as "why has it
%% stopped finding peers", with nothing in the logs to connect it to.
enable() ->
    config:set_dialer(true).

disable() ->
    config:set_dialer(false).

%% Apply a setting to the running server without recording it again.
%% config:set_dialer/1 persists first, then calls this; going the other
%% way round would loop.
apply_enabled(Bool) when is_boolean(Bool) ->
    gen_server:call(?MODULE, {set_enabled, Bool}).

is_enabled() ->
    gen_server:call(?MODULE, is_enabled).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% startup default comes from ssb.cfg ({peer_dialer, Bool}); on when
    %% config is absent (tests)
    Enabled = try config:dialer_enabled() catch _:_ -> true end,
    ok = declare_schema(),
    Timer = erlang:send_after(?INITIAL_DELAY_MS, self(), poll),
    {ok, #{enabled => Enabled, timer => Timer, dialing => undefined}}.

%% Dial passes run in a monitored worker so the server stays responsive:
%% dialing dead peers blocks for seconds per candidate (connect timeout,
%% SHS network-id retries with backoff), which used to starve callers of
%% enable/disable/is_enabled into gen_server timeouts.
handle_info(poll, #{enabled := Enabled, timer := Timer, dialing := Dialing} = State) ->
    %% Cancel the pending timer so an out-of-band poll (trigger/enable)
    %% does not fork a second periodic chain.
    cancel_timer(Timer),
    NewDialing = case {Enabled, Dialing} of
        {true, undefined} ->
            spawn_monitor(fun dial_candidates/0);
        _ ->
            %% disabled, or the previous pass is still running
            Dialing
    end,
    NewTimer = erlang:send_after(?POLL_MS, self(), poll),
    {noreply, State#{timer := NewTimer, dialing := NewDialing}};

handle_info({'DOWN', Ref, process, Pid, _Reason},
            #{dialing := {Pid, Ref}} = State) ->
    {noreply, State#{dialing := undefined}};

handle_info(_Info, State) ->
    {noreply, State}.

handle_call({set_enabled, Bool}, _From, State) ->
    case Bool of
        true  -> self() ! poll;
        false -> ok
    end,
    {reply, ok, State#{enabled := Bool}};

handle_call(is_enabled, _From, #{enabled := Enabled} = State) ->
    {reply, Enabled, State};

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

cancel_timer(undefined) -> ok;
cancel_timer(Ref)       -> erlang:cancel_timer(Ref), ok.

dial_candidates() ->
    Now = erlang:system_time(millisecond),
    Backoff = load_backoff(),
    Candidates = dedup(lan_candidates() ++ known_candidates()),
    {Due, Held} = lists:partition(
                    fun(C) -> due(addr(C), Backoff, Now) end, Candidates),
    ?SSB_DEBUG("peer_dialer: ~p candidate(s), ~p connected~n",
               [length(Candidates), length(peer_registry:all())]),
    log_held(Held, Candidates),
    lists:foreach(fun(C) -> maybe_dial(C, Backoff) end, Due).

%% What the backoff skipped is logged rather than dropped quietly.  A dial
%% list that has silently shrunk to nothing looks exactly like one that is
%% working, and that is the failure this whole mechanism could otherwise
%% introduce.
log_held([], _All) ->
    ok;
log_held(Held, All) ->
    ?SSB_INFO("peer_dialer: ~p of ~p candidate(s) held back by backoff~n",
              [length(Held), length(All)]).

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

%% Re-check the flag before every candidate so disable/0 takes effect in
%% the middle of a long pass, not just between passes.
maybe_dial(Candidate, Backoff) ->
    case is_enabled() of
        true  -> dial(Candidate, Backoff);
        false -> ok
    end.

dial({Host, Port, RawKey} = Candidate, Backoff) ->
    case length(peer_registry:all()) >= ?MAX_CONNS of
        true ->
            ?SSB_DEBUG("peer_dialer: at connection cap, skipping~n", []);
        false ->
            case peer_registry:is_connected(RawKey) of
                true ->
                    ok;
                false ->
                    ?SSB_INFO("peer_dialer: dialing ~p:~p~n", [Host, Port]),
                    do_dial(Candidate, Backoff)
            end
    end.

%% Only a real attempt moves the retry state.  Being at the connection cap
%% or already connected says nothing about whether the address answers, and
%% counting either as a failure would back off the peers that work.
do_dial({Host, Port, RawKey} = Candidate, Backoff) ->
    Addr = addr(Candidate),
    Now = erlang:system_time(millisecond),
    case ssb_peer:start(Host, Port, RawKey) of
        {ok, Pid} ->
            record_ok(Addr, Now),
            ssb_peer:request_ebt(Pid);
        Other ->
            %% gen_server:start/3 can also answer `ignore`, which is a
            %% failed dial like any other.
            Reason = case Other of
                         {error, R} -> R;
                         R          -> R
                     end,
            Attempts = attempts(Addr, Backoff) + 1,
            record_fail(Addr, Attempts, Now),
            ?SSB_INFO("peer_dialer: dial failed ~p:~p reason ~p"
                      " (attempt ~p, next try in ~ps)~n",
                      [Host, Port, Reason, Attempts,
                       backoff_ms(Attempts) div 1000])
    end.

%%%===================================================================
%%% Dial backoff
%%%
%%% A conn.json entry is created with autoconnect on the first time any
%%% peer announces the address, and nothing ever records whether dialing
%%% it worked.  So an address that has never once completed a handshake is
%%% dialed with the same priority as one that connects every time, forever
%%% — and deleting a stale entry does not help, because the next peer that
%%% announces it puts it straight back.
%%%
%%% Backing off is what survives re-announcement: the entry returns, costs
%%% one failed dial, and drops out of the rotation again on its own.
%%%===================================================================

%% The conn.json multiserver address, rebuilt from the candidate triple so
%% that LAN and pub-announced peers key the same way.
%%
%% Host and key together are the unit, which is the part that matters: one
%% host can announce more than one identity, and a pub that has rotated its
%% key leaves the old address behind alongside the live one.  Keying on the
%% host alone would back off both together.
addr({Host, Port, RawKey}) ->
    <<"net:", (to_bin(Host))/binary, ":", (to_bin(Port))/binary,
      "~shs:", (base64:encode(RawKey))/binary>>.

to_bin(B) when is_binary(B)  -> B;
to_bin(L) when is_list(L)    -> list_to_binary(L);
to_bin(I) when is_integer(I) -> integer_to_binary(I);
to_bin(A) when is_atom(A)    -> atom_to_binary(A, utf8);
to_bin(T) when is_tuple(T)   ->
    case inet:ntoa(T) of
        {error, _} -> list_to_binary(io_lib:format("~p", [T]));
        S          -> list_to_binary(S)
    end.

%% Never tried, or the wait since the last failure has elapsed.  A row with
%% no failures behind it (attempts = 0, written by a success) is due.
due(Addr, Backoff, Now) ->
    case maps:get(Addr, Backoff, undefined) of
        undefined        -> true;
        {0, _}           -> true;
        {Attempts, Last} -> Last + backoff_ms(Attempts) =< Now
    end.

attempts(Addr, Backoff) ->
    case maps:get(Addr, Backoff, undefined) of
        undefined -> 0;
        {A, _}    -> A
    end.

backoff_ms(Attempts) when Attempts =< 1 ->
    ?BACKOFF_BASE_MS;
backoff_ms(Attempts) ->
    %% The shift is bounded before it is taken; ?BACKOFF_MAX_MS alone would
    %% still build a bignum for an address that has failed a few hundred
    %% times, which the dead entries in a long-lived conn.json will.
    min(?BACKOFF_BASE_MS bsl min(Attempts - 1, 20), ?BACKOFF_MAX_MS).

%% A store that is unavailable costs the backoff, not the dialer: every
%% candidate then reads as due, which is exactly the behaviour before this
%% table existed.
declare_schema() ->
    case catch ssb_store:declare(?MODULE, ?SCHEMA_VERSION, ?DDL) of
        ok ->
            prune_rows();
        Err ->
            ?SSB_ERROR("peer_dialer: could not declare its schema (~p) — "
                       "dial backoff is off for this run, every candidate "
                       "will be dialed every pass~n", [Err]),
            ok
    end.

prune_rows() ->
    Cutoff = erlang:system_time(millisecond) - ?ROW_TTL_MS,
    _ = catch ssb_store:write("DELETE FROM peer_dial_try WHERE last_try < ?1",
                              [Cutoff]),
    ok.

%% Read once per pass rather than per candidate: conn.json runs to
%% thousands of entries, and a query each would put that many calls
%% through the store on every poll.
load_backoff() ->
    maps:from_list(
      [{Addr, {Attempts, LastTry}}
       || [Addr, Attempts, LastTry]
              <- rows("SELECT addr, attempts, last_try FROM peer_dial_try", [])]).

%% A connection that worked clears the history, so the next failure starts
%% from the base wait instead of inheriting a backoff earned months ago.
record_ok(Addr, Now) ->
    _ = catch ssb_store:write(
                "INSERT INTO peer_dial_try(addr, attempts, last_try, last_ok)"
                " VALUES(?1, 0, ?2, ?2)"
                " ON CONFLICT(addr) DO UPDATE SET attempts=0,"
                " last_try=excluded.last_try, last_ok=excluded.last_ok",
                [Addr, Now]),
    ok.

%% last_ok is left to its DEFAULT here: a failure must not disturb the
%% record of when the address last actually worked.
record_fail(Addr, Attempts, Now) ->
    _ = catch ssb_store:write(
                "INSERT INTO peer_dial_try(addr, attempts, last_try)"
                " VALUES(?1, ?2, ?3)"
                " ON CONFLICT(addr) DO UPDATE SET attempts=excluded.attempts,"
                " last_try=excluded.last_try",
                [Addr, Attempts, Now]),
    ok.

rows(Sql, Params) ->
    try ssb_store:q(Sql, Params) of
        L when is_list(L) -> L;
        _                 -> []
    catch _:_ -> []
    end.

-ifdef(TEST).

%% Without config running the dialer defaults to enabled; enable/disable
%% flip it at runtime.  The poll fired by enable/0 must not kill the
%% server even though heartbeat/conn_db are absent here.
enable_disable_test() ->
    {ok, Pid} = peer_dialer:start_link(),
    ?assert(peer_dialer:is_enabled()),
    ok = peer_dialer:apply_enabled(false),
    ?assertNot(peer_dialer:is_enabled()),
    ok = peer_dialer:apply_enabled(true),
    ?assert(peer_dialer:is_enabled()),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid).

%% The server must answer calls while a dial pass is stuck — this is the
%% maxbutt toggle-timeout regression.  A fake heartbeat that never replies
%% hangs the worker pass in heartbeat:peers(); is_enabled/disable must
%% still respond immediately.
responsive_during_dial_test() ->
    HbStarted = case whereis(heartbeat) of
        undefined ->
            Hb = spawn(fun() -> receive never -> ok end end),
            register(heartbeat, Hb),
            Hb;
        _ ->
            false
    end,
    {ok, Pid} = peer_dialer:start_link(),
    peer_dialer:trigger(),
    timer:sleep(50),
    ?assert(peer_dialer:is_enabled()),
    ok = peer_dialer:apply_enabled(false),
    ?assertNot(peer_dialer:is_enabled()),
    gen_server:stop(Pid),
    case HbStarted of
        false -> ok;
        HbPid -> exit(HbPid, kill)
    end.

%% {peer_dialer, false} in the config file starts the dialer disabled.
config_startup_test() ->
    Cfg = "test/dialer_test.cfg",
    ok = file:write_file(Cfg, ~"{peer_dialer, false}.\n"),
    ConfigStarted = case whereis(config) of
        undefined -> {ok, _} = config:start_link(Cfg), true;
        _         -> false
    end,
    ?assertNot(config:dialer_enabled()),
    {ok, Pid} = peer_dialer:start_link(),
    ?assertNot(peer_dialer:is_enabled()),
    gen_server:stop(Pid),
    case ConfigStarted of
        true  -> gen_server:stop(config);
        false -> ok
    end,
    file:delete(Cfg).

-endif.
