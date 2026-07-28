%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Message-type index: every stored public message keyed by its content
%% type (post, about, contact, vote, ...).  Serves `messagesByType`
%% (JS: ssb-db), which clients use for type-scoped scans.
%%
%% An ssb_view over ssb_store — (type, msg) with type first — plus an
%% ssb_plugin, in one gen_server.
%%
%% This index used to be an ETS duplicate_bag, and the reason is worth
%% recording because the port removes it.  Nearly every row shares the one
%% key <<"post">>, and a `bag` insert scans that key's whole bucket
%% looking for an exact duplicate; restoring EarlButt's 263k-row snapshot
%% was quadratic and pinned handle_continue for ~40 minutes, during which
%% the node served no messagesByType and looked like it had failed to
%% register.  The fix was duplicate_bag, which does not check — at the
%% price of letting a crash-window refold insert repeats, deduplicated on
%% every read instead.
%%
%% A primary key on (type, msg) gets uniqueness back without either cost:
%% the dedup is an index probe rather than a bucket scan, so it is
%% O(log n) on insert instead of O(bucket), and reads no longer sort a
%% quarter of a million ids to undo duplicates that can no longer exist.
%%
%% Private (still-encrypted) content has no visible type and is not
%% indexed.  live/old/gt are honoured; a live stream emits a {sync: true}
%% sentinel between the backlog and the live tail (ssb-db convention —
%% the silkpurse search indexer waits for it).
%%
%% `gt` compares against the message's RECEIVED time — the envelope's
%% top-level `timestamp`, the one message:encode/1 emits — and not the
%% self-asserted content timestamp.  That is the field the client's cursor
%% comes from (silkpurse's indexer stores `m.timestamp`), and it is the
%% only one that means anything as a resume point: asserted timestamps are
%% junk and are not monotonic in arrival order.
%%
%% Honouring it is not an optimisation.  Ignoring `gt` meant a reconnecting
%% client asking for "posts since my cursor" was served the entire post
%% backlog, one file read per message, on the connection's single
%% rpc_processor — 45 s muxrpc timeouts, a dropped connection, and blob
%% replication sharing that connection never getting anywhere.  Results are
%% ordered by that timestamp for the same reason: a client advancing a
%% cursor through an id-ordered stream ends up with the last id it saw
%% rather than the highest timestamp, so its next resume point is wrong.
-module(silkpurse_by_type).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).
-behaviour(ssb_view).
-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

%% API
-export([start_link/0]).

%% ssb_view callbacks
-export([view_version/0, view_load/0, view_reset/0, view_save/0,
         view_entry/1]).

%% ssb_plugin callbacks
-export([manifest/0, handle_rpc/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SCHEMA_VERSION, 2).
%% type first: every read is "all messages of this type".  The primary key
%% keeps a message from being indexed twice; the (type, ts) index is the
%% actual access path, since every read is ordered by received time and
%% most are bounded below by a cursor.
-define(DDL,
        ["CREATE TABLE IF NOT EXISTS msg_by_type("
         "  type TEXT NOT NULL,"
         "  msg  TEXT NOT NULL,"
         "  ts   INTEGER NOT NULL DEFAULT 0,"
         "  PRIMARY KEY (type, msg)) WITHOUT ROWID;",
         "CREATE INDEX IF NOT EXISTS ix_by_type_ts"
         "  ON msg_by_type(type, ts);"]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% ssb_view callbacks (run in the view_manager process)
%%%===================================================================

%% 3: rows gained a received timestamp so `gt` can be served.  Existing
%% rows have none, and the column's default of 0 would make them all sort
%% first and pass every cursor — so this must refold rather than migrate.
view_version() -> 3.

view_load() ->
    case ssb_store:complete(?MODULE) of
        true  -> ok;
        false -> empty
    end.

view_reset() ->
    _ = ssb_store:clear_complete(?MODULE),
    _ = ssb_store:exec("DELETE FROM msg_by_type;"),
    ok.

%% Rows are already durable; the only thing to record is that this view's
%% state is complete up to the manager's checkpoints.
view_save() ->
    _ = ssb_store:mark_complete(?MODULE),
    ok.

%% One write per message, since nearly every message carries a type.
%% Measured at ~11 us against ~1.5 us batched, which over a full refold of
%% a 263k-message corpus is 2.8s versus 0.4s — both far below the file
%% reads and signature checks already on this path, so it is not worth the
%% complexity of batching (unlike view_manager's checkpoints, which move
%% several times per message).
view_entry(#message{id = MsgId, received = Recv, content = {Props}}) ->
    case ?pgv(~"type", Props) of
        Type when is_binary(Type) ->
            %% ts is updated on conflict: a refold re-deriving the same row
            %% should correct a 0 left by an older schema, not preserve it.
            catch ssb_store:write("INSERT INTO msg_by_type(type, msg, ts)"
                                  " VALUES(?1, ?2, ?3)"
                                  " ON CONFLICT(type, msg) DO UPDATE SET"
                                  "   ts=excluded.ts",
                                  [Type, MsgId, num(Recv)]),
            {events, [{typed, Type, MsgId}]};
        _ ->
            ok
    end;
view_entry(_) ->
    ok.

%% ts is NOT NULL because it is the sort key; a message with no usable
%% received time floors at 0 rather than propagating null.
num(Ts) when is_integer(Ts) -> Ts;
num(Ts) when is_float(Ts)   -> trunc(Ts);
num(_)                      -> 0.

%%%===================================================================
%%% ssb_plugin callbacks (run in each connection's rpc_processor)
%%%===================================================================

manifest() ->
    [{[~"messagesByType"], source, owner}].

handle_rpc([~"messagesByType"], Args, _Caller) ->
    case type_of(Args) of
        undefined ->
            {error, ~"messagesByType takes a type"};
        Type ->
            %% no dedup step: the primary key already guarantees it
            {GtSql, GtP} = gt_clause(gt_of(Args)),
            Ids = [Id || [Id] <- rows(["SELECT msg FROM msg_by_type"
                                       " WHERE type=?1", GtSql,
                                       " ORDER BY ts ASC, msg ASC"],
                                      [Type | GtP]),
                         is_binary(Id)],
            %% hydrate lazily — one message per sent frame.  Building the
            %% whole [{Id, Bin}] list up front meant a full store's worth
            %% of per-feed fetches before the first byte went out,
            %% wedging the connection's rpc_processor for minutes.
            Pairs = [{Id, fun() -> fetch_encoded(Id) end} || Id <- Ids],
            case flag_of(~"live", Args, false) of
                false ->
                    {source, [F || {_, F} <- Pairs]};
                true ->
                    Snapshot = case flag_of(~"old", Args, true) of
                                   false -> [];
                                   _     -> Pairs
                               end,
                    EventFun =
                        fun({typed, T, MsgId}) when T =:= Type ->
                                case fetch_encoded(MsgId) of
                                    undefined -> skip;
                                    Bin       -> {send, MsgId, Bin}
                                end;
                           (_) -> skip
                        end,
                    {live_source, Snapshot ++ [sync_sentinel()], ?MODULE,
                     EventFun}
            end
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = ensure_ts_column(),
    ok = ssb_store:declare(?MODULE, ?SCHEMA_VERSION, ?DDL),
    {ok, #{}, {continue, register}}.

%% Add `ts` to a table created before it existed.
%%
%% Not part of ?DDL, for two reasons.  SQLite has no ADD COLUMN IF NOT
%% EXISTS, and ssb_store:declare/3 badmatches on a failed statement and
%% rolls the whole schema back — so an unconditional ALTER would break
%% every fresh node.  And it has to happen BEFORE declare/3, because the
%% index that declare creates names the column.
%%
%% A missing table is the fresh-node case: nothing to alter, and the DDL
%% is about to create it with the column already present.
ensure_ts_column() ->
    Cols = [C || [C] <- rows("SELECT name FROM pragma_table_info('msg_by_type')",
                             [])],
    case Cols =:= [] orelse lists:member(~"ts", Cols) of
        true ->
            ok;
        false ->
            _ = ssb_store:exec("ALTER TABLE msg_by_type"
                               " ADD COLUMN ts INTEGER NOT NULL DEFAULT 0"),
            ok
    end.

%% Failures are loud and transient ones retried on a timer
%% (ssb_view:ensure_registered) — the old silent noproc swallow here cost
%% EarlButt its messagesByType method (July 2026).
handle_continue(register, State) ->
    ensure_registered(State).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(ensure_registered, State) ->
    ensure_registered(State);
handle_info(_Info, State) ->
    {noreply, State}.

%% First attempt (from handle_continue) and every timer retry land
%% here; keep trying until every service accepts the registration.
ensure_registered(State) ->
    case ssb_view:ensure_registered(?MODULE) of
        ok    -> ok;
        retry -> erlang:send_after(2000, self(), ensure_registered)
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

rows(Sql, Params) ->
    try ssb_store:q(Sql, Params) of
        L when is_list(L) -> L;
        _                 -> []
    catch _:_ -> []          %% store down: no index, never a crash
    end.

%% Boolean option (live, old) from the request's option object.
flag_of(Key, [{Props}], Default) ->
    case ?pgv(Key, Props) of
        B when is_boolean(B) -> B;
        _                    -> Default
    end;
flag_of(_Key, _Args, Default) ->
    Default.

gt_clause(undefined) -> {"", []};
gt_clause(Gt)        -> {" AND ts > ?2", [Gt]}.

%% The cursor, which arrives as a JSON number or (as silkpurse sends it,
%% straight back out of its own SQLite) a string.  Anything else is no
%% cursor at all rather than a guess — serving the whole backlog is slow,
%% but silently serving none of it would look like data loss.
gt_of([{Props}]) ->
    case ?pgv(~"gt", Props) of
        N when is_integer(N) -> N;
        N when is_float(N)   -> trunc(N);
        B when is_binary(B)  -> try binary_to_integer(B) catch _:_ -> undefined end;
        _                    -> undefined
    end;
gt_of(_) ->
    undefined.

%% JS accepts a bare type string or {type: T, live, ...}.
type_of([Type]) when is_binary(Type) ->
    Type;
type_of([{Props}]) ->
    case ?pgv(~"type", Props) of
        Type when is_binary(Type) -> Type;
        _                         -> undefined
    end;
type_of(_) ->
    undefined.

%% Closes the backlog of a live stream, before the live tail begins.
sync_sentinel() ->
    {make_ref(),
     iolist_to_binary(message:ssb_encoder({[{~"sync", true}]},
                                          fun message:ssb_encoder/3,
                                          [pretty]))}.

fetch_encoded(MsgId) ->
    case mess_auth:get(MsgId) of
        not_found -> undefined;
        Author ->
            try
                Pid = utils:find_or_create_feed_pid(Author),
                case ssb_feed:fetch_msg(Pid, MsgId) of
                    not_found -> undefined;
                    Msg       -> message:encode(Msg)
                end
            catch _:_ -> undefined
            end
    end.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

type_of_test() ->
    ?assertEqual(~"post", type_of([~"post"])),
    ?assertEqual(~"vote", type_of([{[{~"type", ~"vote"}, {~"live", false}]}])),
    ?assertEqual(undefined, type_of([])),
    ?assertEqual(undefined, type_of([{[{~"live", true}]}])).

by_type_test_() ->
    {setup, fun bt_setup/0, fun bt_teardown/1,
     fun(_) -> [?_test(index_and_read_by_type()),
                ?_test(refold_does_not_duplicate()),
                ?_test(survives_a_restart()),
                ?_test(gt_bounds_the_backlog()),
                ?_test(results_are_in_timestamp_order()),
                ?_test(adds_ts_to_an_older_table())] end}.

bt_setup() ->
    bt_teardown(ignore),
    Home = filename:join("/tmp", "bt_" ++
                          integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = ssb_store:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = view_manager:start_link(),
    {ok, _} = silkpurse_by_type:start_link(),
    ok = wait_view_ready(silkpurse_by_type),
    Home.

%% Registration lands after start_link/0 returns, and registering a view
%% whose state is not marked complete resets it — so a test asserting on
%% the index must wait, or the reset arrives mid-test.  caught_up/1 alone
%% answers true for a module that has not registered at all, which is the
%% window being waited out.
wait_view_ready(Mod) ->
    wait_view_ready(Mod, 250).

wait_view_ready(Mod, 0) ->
    error({view_never_ready, Mod});
wait_view_ready(Mod, N) ->
    case lists:member(Mod, view_manager:views())
        andalso view_manager:caught_up(Mod) of
        true  -> ok;
        false -> timer:sleep(20), wait_view_ready(Mod, N - 1)
    end.

bt_teardown(Home) ->
    [catch gen_server:stop(Name)
     || Name <- [silkpurse_by_type, view_manager, ssb_feed_sup,
                 blobs, mess_auth, ssb_store, keys, config]],
    case Home of
        ignore -> ok;
        _ ->
            os:cmd("rm -rf " ++ Home),
            application:unset_env(ssb, ssb_home)
    end,
    ok.

index_and_read_by_type() ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"},
                                         {~"text", ~"a post"}]}),
    #message{id = PostId} = ssb_feed:fetch_last_msg(OwnPid),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"vote"},
                                         {~"vote", {[{~"link", PostId},
                                                     {~"value", 1}]}}]}),
    #message{id = VoteId} = ssb_feed:fetch_last_msg(OwnPid),
    Caller = #{class => owner, feed_id => OwnId},
    %% items hydrate lazily: each is a fun/0 producing the encoded body
    {source, [PostFun]} =
        handle_rpc([~"messagesByType"], [~"post"], Caller),
    #message{id = PostId} = message:decode(PostFun(), false),
    {source, [VoteFun]} =
        handle_rpc([~"messagesByType"], [{[{~"type", ~"vote"}]}], Caller),
    #message{id = VoteId} = message:decode(VoteFun(), false),
    {source, []} = handle_rpc([~"messagesByType"], [~"gathering"], Caller),
    %% live mode: backlog, then the {sync:true} sentinel as the last
    %% snapshot pair
    {live_source, LivePairs, ?MODULE, _Fun} =
        handle_rpc([~"messagesByType"],
                   [{[{~"type", ~"post"}, {~"live", true}]}], Caller),
    [{_, PostFun2}, {_, SyncBin}] = LivePairs,
    #message{id = PostId} = message:decode(PostFun2(), false),
    ?assertEqual({[{~"sync", true}]}, utils:nat_decode(SyncBin)),
    %% old:false still carries the sentinel so the client knows the
    %% (empty) backlog is done
    {live_source, [{_, OnlySync}], ?MODULE, _} =
        handle_rpc([~"messagesByType"],
                   [{[{~"type", ~"post"}, {~"live", true}, {~"old", false}]}],
                   Caller),
    ?assertEqual({[{~"sync", true}]}, utils:nat_decode(OnlySync)).

%% The duplicate the ETS duplicate_bag could hold (and read-time usort
%% had to remove) is now impossible: a refold re-inserting a message it
%% already indexed is a no-op on the primary key.
refold_does_not_duplicate() ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"dup"},
                                         {~"text", ~"indexed twice"}]}),
    Msg = ssb_feed:fetch_last_msg(OwnPid),
    %% deliver the same message a second time, as a crash-window refold
    %% would
    _ = view_entry(Msg),
    ?assertEqual([[1]], ssb_store:q("SELECT count(*) FROM msg_by_type"
                                    " WHERE type=?1", [~"dup"])),
    Caller = #{class => owner, feed_id => OwnId},
    {source, Funs} = handle_rpc([~"messagesByType"], [~"dup"], Caller),
    ?assertEqual(1, length(Funs)).

%% The point of the port: durable as written, with no snapshot step.
survives_a_restart() ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"persisted"},
                                         {~"text", ~"still here"}]}),
    #message{id = Id} = ssb_feed:fetch_last_msg(OwnPid),
    ok = gen_server:stop(ssb_store),
    {ok, _} = ssb_store:start_link(),
    ?assertEqual([[Id]], ssb_store:q("SELECT msg FROM msg_by_type"
                                     " WHERE type=?1", [~"persisted"])).

%% Seed rows directly: the received timestamps have to be controlled, and
%% posting real messages gives them all the same millisecond.
seed(Type, Msg, Ts) ->
    ok = ssb_store:write("INSERT INTO msg_by_type(type, msg, ts)"
                         " VALUES(?1, ?2, ?3)"
                         " ON CONFLICT(type, msg) DO UPDATE SET ts=excluded.ts",
                         [Type, Msg, Ts]).

ids_for(Args) ->
    {source, Funs} = handle_rpc([~"messagesByType"], Args,
                                #{class => owner,
                                  feed_id => keys:pub_key_disp()}),
    length(Funs).

%% The bug this fixes: a reconnecting client that says "I have everything
%% up to here" was served the whole backlog anyway, one file read each,
%% which timed out the connection and starved blob replication with it.
gt_bounds_the_backlog() ->
    T = ~"gttype",
    [seed(T, Id, Ts) || {Id, Ts} <- [{~"%g1.sha256", 100},
                                     {~"%g2.sha256", 200},
                                     {~"%g3.sha256", 300}]],
    %% no cursor: everything
    ?assertEqual(3, ids_for([{[{~"type", T}]}])),
    %% a cursor: strictly newer only
    ?assertEqual(1, ids_for([{[{~"type", T}, {~"gt", 200}]}])),
    ?assertEqual(0, ids_for([{[{~"type", T}, {~"gt", 300}]}])),
    %% silkpurse sends it as a string, out of its own SQLite
    ?assertEqual(1, ids_for([{[{~"type", T}, {~"gt", ~"200"}]}])),
    %% an unusable cursor serves everything rather than nothing — slow
    %% beats looking like data loss
    ?assertEqual(3, ids_for([{[{~"type", T}, {~"gt", ~"not-a-number"}]}])),
    %% and it applies to the live snapshot too, which is where it matters
    {live_source, Snapshot, ?MODULE, _} =
        handle_rpc([~"messagesByType"],
                   [{[{~"type", T}, {~"live", true}, {~"gt", 100}]}],
                   #{class => owner, feed_id => keys:pub_key_disp()}),
    ?assertEqual(3, length(Snapshot)).    %% 2 messages + the sync sentinel

%% A client advancing a cursor through an id-ordered stream keeps the last
%% id it saw, not the highest timestamp, so its next resume point is wrong.
results_are_in_timestamp_order() ->
    T = ~"ordtype",
    %% ids sort opposite to timestamps, so id order cannot pass by luck
    [seed(T, Id, Ts) || {Id, Ts} <- [{~"%a.sha256", 300},
                                     {~"%b.sha256", 200},
                                     {~"%c.sha256", 100}]],
    ?assertEqual([~"%c.sha256", ~"%b.sha256", ~"%a.sha256"],
                 [Id || [Id] <- ssb_store:q(
                                  "SELECT msg FROM msg_by_type WHERE type=?1"
                                  " ORDER BY ts ASC, msg ASC", [T])]).

%% The upgrade path that runs on a node whose table predates the column.
%% ALTER cannot be part of the DDL (no IF NOT EXISTS, and declare/3 rolls
%% back the whole schema on a failed statement), so it is worth proving it
%% works on a table of the old shape.
adds_ts_to_an_older_table() ->
    ok = ssb_store:exec("DROP TABLE IF EXISTS msg_by_type"),
    ok = ssb_store:exec("CREATE TABLE msg_by_type("
                        "  type TEXT NOT NULL,"
                        "  msg  TEXT NOT NULL,"
                        "  PRIMARY KEY (type, msg)) WITHOUT ROWID"),
    ok = ssb_store:write("INSERT INTO msg_by_type(type, msg) VALUES(?1, ?2)",
                         [~"old", ~"%old.sha256"]),
    ok = ensure_ts_column(),
    %% the column is there, the existing row kept, and the index the DDL
    %% creates can now name it
    ?assertEqual([[~"%old.sha256", 0]],
                 ssb_store:q("SELECT msg, ts FROM msg_by_type WHERE type=?1",
                             [~"old"])),
    ok = ssb_store:exec("CREATE INDEX IF NOT EXISTS ix_by_type_ts"
                        " ON msg_by_type(type, ts)"),
    %% and it is idempotent
    ok = ensure_ts_column().

-endif.