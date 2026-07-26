%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% The embedded store for derived state (doc/persistence.md §4, §6).
%%
%% Everything a view derives from the log lives here instead of in ETS
%% plus a 60-second ets:tab2file snapshot.  That swap buys three things:
%% durability that costs O(change) rather than O(state), atomicity across
%% a whole batch, and derived state that is no longer bounded by RAM.
%%
%% WHAT DOES NOT LIVE HERE: signed messages and blobs.  Those stay as
%% flat files.  Their integrity comes from the signature chain, not from
%% a storage engine, and keeping them in SSB's own format is what keeps
%% erlbutt interoperable.  This file holds only what can be thrown away
%% and refolded from the log.
%%
%% CONCURRENCY.  esqlite's connection is safe to use from several
%% processes at once, so reads run in the CALLER's process — a view query
%% is a plain function call, not a gen_server round-trip, which is what
%% makes replacing an ETS lookup acceptable.  Writes go through this
%% server, which matches SQLite's single-writer model and gives one place
%% to batch them.
%%
%% BATCHING IS NOT OPTIONAL.  Every esqlite call occupies a dirty IO
%% scheduler for its whole duration, and that pool defaults to ten
%% threads node-wide (§6).  One transaction per ingest batch runs at
%% ~500k rows/s; a transaction per row does not.  transaction/1 exists to
%% make the batched shape the easy one.
-module(ssb_store).

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/0,
         declare/3,
         q/1, q/2,
         exec/1,
         write/2,
         transaction/1,
         insert_many/2,
         mark_complete/1,
         clear_complete/1,
         complete/1,
         available/0,
         db_file/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(HANDLE, {?MODULE, db}).      %% persistent_term key for the connection

%% How long SQLite waits out a lock before reporting busy, and how many
%% times we then retry a step ourselves.
-define(BUSY_TIMEOUT_MS, 5000).
-define(BUSY_RETRIES, 5).
-define(BUSY_SLEEP_MS, 50).

-record(st, {db, file}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Is the store up?  Callers that must degrade rather than crash (an RPC
%% handler, a view whose server is restarting) check this first.
available() ->
    persistent_term:get(?HANDLE, undefined) =/= undefined.

db_file() ->
    <<(config:ssb_repo_loc())/binary, "store.db">>.

%% A view declares the tables it owns.  DDL is applied only when the
%% recorded version for Name differs, so this is cheap to call on every
%% boot; bumping Version re-applies it.
%%
%% The DDL must be idempotent-safe to re-run (CREATE TABLE IF NOT EXISTS,
%% CREATE INDEX IF NOT EXISTS).  A version bump does NOT drop anything —
%% dropping and refolding is view_manager's business, not the schema's.
declare(Name, Version, DDL) when is_atom(Name), is_integer(Version),
                                 is_list(DDL) ->
    gen_server:call(?SERVER, {declare, Name, Version, DDL}, infinity).

%% Read.  Runs in the calling process on the shared connection.
q(Sql) ->
    q(Sql, []).

q(Sql, Params) ->
    esqlite3:q(db(), Sql, Params).

%% Single write statement, no parameters.
exec(Sql) ->
    gen_server:call(?SERVER, {exec, Sql}, infinity).

%% Single parameterised write.  esqlite3:exec/2 takes no parameters, so
%% this goes through q/3, which returns [] for an INSERT/UPDATE/DELETE.
%%
%% Each call is its own implicit transaction, hence its own commit.  That
%% is fine for a view whose writes are rare (a follow graph only writes
%% on a contact message); a view that writes for most messages wants
%% insert_many/2 or transaction/1 instead — see §6 on batching.
write(Sql, Params) ->
    gen_server:call(?SERVER, {write, Sql, Params}, infinity).

%%%===================================================================
%%% View completeness
%%%===================================================================
%%
%% view_manager keeps its checkpoints in ETS with a periodic snapshot,
%% while a store-backed view's rows are durable immediately.  The two can
%% therefore disagree after a crash — in one direction only: the store is
%% always at or ahead of the last snapshotted checkpoint, so the view
%% replays messages it has already folded, which is safe because folds are
%% idempotent per {feed, seq}.
%%
%% The dangerous case is the other one: checkpoints claiming coverage of
%% a store that is EMPTY (store.db deleted, checkpoints.tab kept), which
%% would leave a view silently blank forever.  A view marks itself
%% complete here; view_load/0 asks, and a fresh database answers no.

mark_complete(Name) when is_atom(Name) ->
    write("INSERT INTO ssb_view_state(name,complete) VALUES(?1,1)"
          " ON CONFLICT(name) DO UPDATE SET complete=1",
          [atom_to_binary(Name, utf8)]).

clear_complete(Name) when is_atom(Name) ->
    write("INSERT INTO ssb_view_state(name,complete) VALUES(?1,0)"
          " ON CONFLICT(name) DO UPDATE SET complete=0",
          [atom_to_binary(Name, utf8)]).

complete(Name) when is_atom(Name) ->
    try q("SELECT complete FROM ssb_view_state WHERE name=?1",
          [atom_to_binary(Name, utf8)]) of
        [[1]] -> true;
        _     -> false
    catch _:_ -> false
    end.

%% Run Fun inside one transaction, in this server's process.  Fun is
%% given the connection and may use esqlite3 directly.  Returns ok, or
%% {error, {Class, Reason}} with the whole batch rolled back — a
%% half-applied batch would leave the derived tier inconsistent with
%% itself, which is precisely what the store exists to prevent.
transaction(Fun) when is_function(Fun, 1) ->
    gen_server:call(?SERVER, {transaction, Fun}, infinity).

%% Insert many rows through one prepared statement in one transaction —
%% the shape §6 says to use, wrapped so callers do not hand-roll it.
insert_many(_Sql, []) ->
    ok;
insert_many(Sql, Rows) when is_list(Rows) ->
    transaction(
      fun(Db) ->
              {ok, Stmt} = esqlite3:prepare(Db, Sql),
              lists:foreach(
                fun(Row) ->
                        ok = esqlite3:bind(Stmt, Row),
                        ok = step_done(Stmt, ?BUSY_RETRIES),
                        ok = esqlite3:reset(Stmt)
                end, Rows)
      end).

%% Step a statement to completion.
%%
%% This is the one place we call step/1 directly, so it is the one place
%% that can see '$busy' as a value rather than as a case_clause from
%% inside esqlite3:q/3.  The busy_timeout PRAGMA should mean we never get
%% here, so reaching a retry at all is worth noticing; exhausting them
%% fails the transaction, which rolls back cleanly.
%%
%% A statement with a RETURNING clause yields rows before '$done', so
%% they are drained rather than treated as an error.
step_done(_Stmt, 0) ->
    error({sqlite_busy, retries_exhausted});
step_done(Stmt, Retries) ->
    case esqlite3:step(Stmt) of
        '$done' ->
            ok;
        Row when is_list(Row) ->
            step_done(Stmt, Retries);          %% RETURNING row; keep going
        '$busy' ->
            ?SSB_ERROR("ssb_store: SQLITE_BUSY past the ~p ms timeout; "
                       "retrying (~p left)", [?BUSY_TIMEOUT_MS, Retries - 1]),
            timer:sleep(?BUSY_SLEEP_MS),
            step_done(Stmt, Retries - 1);
        {error, _} = E ->
            error({sqlite_step_failed, E})
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    File = db_file(),
    ok = filelib:ensure_dir(File),
    {ok, Db} = esqlite3:open(?b2l(File)),
    %% WAL so readers never block the writer.  synchronous=NORMAL trades a
    %% fsync per commit for one per checkpoint: on a crash the newest
    %% commits may be lost, which is the same bargain the message log
    %% already makes (§3, no fsync on the hot path) and safe for the same
    %% reason — anything lost here is refolded from the log.
    ok = esqlite3:exec(Db, "PRAGMA journal_mode=WAL;"),
    ok = esqlite3:exec(Db, "PRAGMA synchronous=NORMAL;"),
    ok = esqlite3:exec(Db, "PRAGMA foreign_keys=ON;"),
    %% Let SQLite wait out a lock itself instead of handing us SQLITE_BUSY.
    %%
    %% This matters more than it looks.  esqlite3:step/1 can return
    %% '$busy', but esqlite3:q/3 does not handle it — its fetchall loop
    %% matches only a row, '$done' and {error, _}, so a busy statement
    %% raises case_clause.  A busy READ would then be swallowed by the
    %% views' try/catch and read as "no data"; a busy WRITE would take
    %% this server down with it.  With a busy handler installed, SQLite
    %% retries internally and only reports busy after the timeout, which
    %% in practice it never reaches: one connection, one writer.
    %%
    %% The case it does cover is a second connection holding the write
    %% lock — someone opening store.db with the sqlite3 CLI on a running
    %% node, say.
    ok = esqlite3:exec(Db, "PRAGMA busy_timeout=" ++
                           integer_to_list(?BUSY_TIMEOUT_MS) ++ ";"),
    ok = esqlite3:exec(Db,
        "CREATE TABLE IF NOT EXISTS ssb_schema("
        "  name TEXT PRIMARY KEY,"
        "  version INTEGER NOT NULL);"),
    ok = esqlite3:exec(Db,
        "CREATE TABLE IF NOT EXISTS ssb_view_state("
        "  name TEXT PRIMARY KEY,"
        "  complete INTEGER NOT NULL);"),
    persistent_term:put(?HANDLE, Db),
    ?SSB_INFO("ssb_store: open at ~s", [File]),
    {ok, #st{db = Db, file = File}}.

handle_call({declare, Name, Version, DDL}, _From, #st{db = Db} = St) ->
    Key = atom_to_binary(Name, utf8),
    Current = case esqlite3:q(Db, "SELECT version FROM ssb_schema WHERE name=?1",
                              [Key]) of
                  [[V]] -> V;
                  _     -> undefined
              end,
    case Current =:= Version of
        true ->
            {reply, ok, St};
        false ->
            ?SSB_INFO("ssb_store: applying schema for ~p (have ~p, want ~p)",
                      [Name, Current, Version]),
            Res = in_transaction(
                    Db, fun(D) ->
                                [ok = esqlite3:exec(D, S) || S <- DDL],
                                %% esqlite3:exec/2 takes no parameters, so
                                %% a parameterised write goes through q/3
                                %% (which returns [] for an INSERT).
                                [] = esqlite3:q(
                                       D, "INSERT INTO ssb_schema(name,version)"
                                          " VALUES(?1,?2)"
                                          " ON CONFLICT(name) DO UPDATE SET"
                                          " version=excluded.version",
                                       [Key, Version]),
                                ok
                        end),
            {reply, Res, St}
    end;

handle_call({exec, Sql}, _From, #st{db = Db} = St) ->
    {reply, guarded(fun() -> esqlite3:exec(Db, Sql) end), St};

handle_call({write, Sql, Params}, _From, #st{db = Db} = St) ->
    Reply = guarded(
              fun() ->
                      case esqlite3:q(Db, Sql, Params) of
                          []              -> ok;
                          {error, _} = E  -> E;
                          Other           -> Other
                      end
              end),
    {reply, Reply, St};

handle_call({transaction, Fun}, _From, #st{db = Db} = St) ->
    {reply, in_transaction(Db, Fun), St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(_Info, St) ->
    {noreply, St}.

terminate(_Reason, #st{db = Db}) ->
    persistent_term:erase(?HANDLE),
    catch esqlite3:close(Db),
    ok.

code_change(_Old, St, _Extra) ->
    {ok, St}.

%%%===================================================================
%%% Internal
%%%===================================================================

db() ->
    case persistent_term:get(?HANDLE, undefined) of
        undefined -> error(ssb_store_not_running);
        Db        -> Db
    end.

%% Run a statement without letting it take the server down.
%%
%% This server owns the one connection and publishes its handle in
%% persistent_term, so a raise here is not one failed write — it is a
%% store-wide outage that every view then reads as "no data".  esqlite3
%% raises for at least two reachable reasons: a case_clause on '$busy'
%% (its fetchall loop has no clause for it) and a badmatch on malformed
%% SQL.  Both are reported, not fatal.
guarded(Fun) ->
    try Fun()
    catch Class:Reason:Stack ->
            ?SSB_ERROR("ssb_store: statement failed: ~p:~p~n~p",
                       [Class, Reason, Stack]),
            {error, {Class, Reason}}
    end.

%% BEGIN/COMMIT around Fun; ROLLBACK and report on failure.
%%
%% The failure is RETURNED, not re-raised.  This server owns the one
%% connection and publishes its handle in persistent_term, so letting a
%% bad batch kill it would turn one rejected row into a store-wide
%% outage — and esqlite reports constraint violations as {error, Code}
%% return values, which a pattern match in the caller's fun turns into a
%% badmatch.  That is a routine, expected failure here, not a crash.
in_transaction(Db, Fun) ->
    ok = esqlite3:exec(Db, "BEGIN;"),
    try Fun(Db) of
        Result ->
            ok = esqlite3:exec(Db, "COMMIT;"),
            case Result of
                ok -> ok;
                _  -> Result
            end
    catch Class:Reason:Stack ->
            catch esqlite3:exec(Db, "ROLLBACK;"),
            ?SSB_ERROR("ssb_store: transaction rolled back: ~p:~p~n~p",
                       [Class, Reason, Stack]),
            {error, {Class, Reason}}
    end.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

store_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
             [?_test(declare_is_idempotent()),
              ?_test(version_bump_reapplies()),
              ?_test(batch_insert_and_query()),
              ?_test(transaction_rolls_back()),
              ?_test(busy_timeout_is_set()),
              ?_test(bad_sql_does_not_kill_the_store()),
              ?_test(reads_run_concurrently())]
     end}.

setup() ->
    cleanup(ignore),
    Home = filename:join("/tmp", "ssb_store_"
                         ++ integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = start_link(),
    Home.

cleanup(Home) ->
    [catch gen_server:stop(N) || N <- [?MODULE, config]],
    catch persistent_term:erase(?HANDLE),
    case Home of
        ignore -> ok;
        _ -> os:cmd("rm -rf " ++ Home),
             application:unset_env(ssb, ssb_home)
    end,
    ok.

%% Declaring the same version twice must not re-run the DDL (which would
%% make every boot pay for it) — and must not lose data if it did.
declare_is_idempotent() ->
    DDL = ["CREATE TABLE IF NOT EXISTS t_idem(a TEXT PRIMARY KEY);"],
    ok = declare(idem_view, 1, DDL),
    ok = exec("INSERT INTO t_idem VALUES('kept');"),
    ok = declare(idem_view, 1, DDL),
    ?assertEqual([[~"kept"]], q("SELECT a FROM t_idem")).

%% A version bump re-applies the DDL, so a view can add a table or an
%% index without anyone hand-editing the database.
version_bump_reapplies() ->
    ok = declare(bump_view, 1, ["CREATE TABLE IF NOT EXISTS t_bump(a TEXT);"]),
    ?assertEqual([], q("SELECT name FROM sqlite_master WHERE name='ix_bump'")),
    ok = declare(bump_view, 2, ["CREATE TABLE IF NOT EXISTS t_bump(a TEXT);",
                                "CREATE INDEX IF NOT EXISTS ix_bump ON t_bump(a);"]),
    ?assertEqual([[~"ix_bump"]],
                 q("SELECT name FROM sqlite_master WHERE name='ix_bump'")).

batch_insert_and_query() ->
    ok = declare(batch_view, 1,
                 ["CREATE TABLE IF NOT EXISTS t_batch(n INTEGER, s TEXT);"]),
    Rows = [[N, integer_to_binary(N)] || N <- lists:seq(1, 500)],
    ok = insert_many("INSERT INTO t_batch VALUES(?1,?2)", Rows),
    ?assertEqual([[500]], q("SELECT count(*) FROM t_batch")),
    ?assertEqual([[~"250"]], q("SELECT s FROM t_batch WHERE n=?1", [250])),
    %% the empty batch is a no-op, not an error
    ?assertEqual(ok, insert_many("INSERT INTO t_batch VALUES(?1,?2)", [])).

%% A batch that fails part-way leaves nothing behind — the property the
%% ETS-plus-snapshot arrangement could not offer.  And the failure is
%% reported, not fatal: the store survives to serve the next caller.
transaction_rolls_back() ->
    ok = declare(roll_view, 1,
                 ["CREATE TABLE IF NOT EXISTS t_roll(a INTEGER PRIMARY KEY);"]),
    ok = insert_many("INSERT INTO t_roll VALUES(?1)", [[1], [2]]),
    ?assertEqual([[2]], q("SELECT count(*) FROM t_roll")),
    Before = whereis(?SERVER),
    %% row 3 inserts, then row 1 collides with the primary key
    ?assertMatch({error, _},
                 insert_many("INSERT INTO t_roll VALUES(?1)", [[3], [1]])),
    %% neither row survived
    ?assertEqual([[2]], q("SELECT count(*) FROM t_roll")),
    ?assertEqual([], q("SELECT a FROM t_roll WHERE a=3")),
    %% and the store is the same process, still usable
    ?assertEqual(Before, whereis(?SERVER)),
    ?assertEqual(ok, insert_many("INSERT INTO t_roll VALUES(?1)", [[4]])),
    ?assertEqual([[3]], q("SELECT count(*) FROM t_roll")).

%% A busy handler must be installed: without one, esqlite3:q/3 raises a
%% case_clause on '$busy' rather than returning it, which would surface
%% as a silently empty read or a dead store.
busy_timeout_is_set() ->
    ?assertEqual([[?BUSY_TIMEOUT_MS]], q("PRAGMA busy_timeout")).

%% A statement that raises inside the server is reported to the caller,
%% not fatal — the server owns the one connection, so killing it over a
%% single bad statement would take every view's reads down with it.
bad_sql_does_not_kill_the_store() ->
    Before = whereis(?SERVER),
    ?assertMatch({error, _}, exec("THIS IS NOT SQL;")),
    ?assertMatch({error, _}, write("INSERT INTO nosuchtable VALUES(?1)", [1])),
    ?assertEqual(Before, whereis(?SERVER)),
    ?assert(available()),
    %% still usable afterwards
    ok = declare(after_bad_sql, 1,
                 ["CREATE TABLE IF NOT EXISTS t_after(a INTEGER);"]),
    ok = insert_many("INSERT INTO t_after VALUES(?1)", [[1]]),
    ?assertEqual([[1]], q("SELECT count(*) FROM t_after")).

%% Reads run in the caller, so several processes query at once — this is
%% what makes a store-backed view as cheap to read as an ETS-backed one.
reads_run_concurrently() ->
    ok = declare(conc_view, 1,
                 ["CREATE TABLE IF NOT EXISTS t_conc(n INTEGER);"]),
    ok = insert_many("INSERT INTO t_conc VALUES(?1)",
                     [[N] || N <- lists:seq(1, 1000)]),
    Parent = self(),
    Pids = [spawn(fun() ->
                          R = [q("SELECT count(*) FROM t_conc WHERE n > ?1", [K])
                               || _ <- lists:seq(1, 50)],
                          Parent ! {self(), lists:usort(R)}
                  end) || K <- lists:seq(1, 10)],
    Results = [receive {P, R} -> R after 30000 -> timeout end || P <- Pids],
    ?assertNot(lists:member(timeout, Results)),
    %% every reader saw one consistent answer across its 50 queries
    ?assertEqual([1], lists:usort([length(R) || R <- Results])).

-endif.
