%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
%%
%% msg_id -> author.  The index that answers "whose feed is this message
%% in", which every read by message id needs before it can open a feed.
%%
%% It lives in ssb_store (doc/persistence.md).  It was an ETS table with
%% an ets:tab2file snapshot every 60 seconds, and that snapshot is the
%% thing this port is about: its cost is O(total messages), not O(what
%% changed, so a node holding a few million messages rewrote hundreds of
%% megabytes a minute, forever, to persist a handful of new rows.  The
%% whole table also had to fit in memory.
%%
%% The trade is real and worth stating: a point lookup went from about
%% 1 us in ETS to about 5 us here, and roughly none of that difference is
%% statement preparation — it is the NIF round trip, so a statement cache
%% would not recover it.  It is judged acceptable because essentially
%% every caller follows the lookup with ssb_feed:fetch_msg/2, which opens
%% and reads a file; 5 us in front of that is noise.  If a caller ever
%% appears that resolves ids in a tight loop without reading messages, an
%% ETS cache in front of this is the answer, and should be added with
%% that caller as the evidence.
-module(mess_auth).

-compile({no_auto_import,[put/2, get/1]}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("ssb/include/ssb.hrl").

-behaviour(gen_server).

-export([start_link/0,
         put/2,
         get/1,
         close/0,
         sync/0,
         all_auths/0,
         rebuild/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(SCHEMA_VERSION, 1).
-define(DDL,
        ["CREATE TABLE IF NOT EXISTS msg_author("
         "  msg_id TEXT PRIMARY KEY,"
         "  author TEXT NOT NULL) WITHOUT ROWID;"]).

%% Rows per transaction when refolding the whole store.  A rebuild that
%% committed per message would be thousands of times slower than one that
%% batches; this is the difference between seconds and hours.
-define(REBUILD_BATCH, 5000).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Record a message's author.  Called from ssb_feed:store/2, so once per
%% stored message, in its own small transaction.
%% Guarded: this runs inside a feed's gen_server on the store path, so a
%% store that is down or restarting must not take the feed down with it.
%% A lost mapping is recoverable — rebuild/0 refolds it from the logs —
%% whereas a dead feed process is not.
put(Key, Val) when is_binary(Key), is_binary(Val) ->
    try ssb_store:write(
          "INSERT INTO msg_author(msg_id, author) VALUES(?1, ?2)"
          " ON CONFLICT(msg_id) DO UPDATE SET author=excluded.author",
          [Key, Val])
    catch Class:Reason ->
            ?SSB_ERROR("mess_auth: could not record ~s -> ~s: ~p:~p",
                       [Key, Val, Class, Reason]),
            ok
    end;
put(_Key, _Val) ->
    ok.

%% The author of a message, or not_found.  Runs in the calling process.
get(Key) when is_binary(Key) ->
    case rows("SELECT author FROM msg_author WHERE msg_id=?1", [Key]) of
        [[Author]] -> Author;
        _          -> not_found
    end;
get(_Key) ->
    not_found.

%% Every distinct author we hold a message for.  A full scan, and only
%% used by the smoke diagnostics — not something to call on a hot path.
all_auths() ->
    [A || [A] <- rows("SELECT DISTINCT author FROM msg_author", [])].

%% Refold the mapping from the per-feed store, archives included.  The
%% recovery hammer, and what a node with an empty table does on first
%% start.
rebuild() ->
    Start = erlang:monotonic_time(millisecond),
    {N, Pending} =
        feed_store:fold_all(
          fun(MsgData, {Count, Batch}) ->
                  try
                      #message{id = Id, author = Auth} =
                          message:decode(MsgData, false),
                      flush_if_full(Count + 1, [[Id, Auth] | Batch])
                  catch _:_ -> {Count, Batch}
                  end
          end, {0, []}),
    ok = flush(Pending),
    ?SSB_INFO("mess_auth: rebuilt ~p entries in ~p ms",
              [N, erlang:monotonic_time(millisecond) - Start]),
    ok.

%% Writes are durable as they happen, so there is nothing to flush.  Kept
%% because callers (converter) still ask.
close() -> ok.
sync()  -> ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = ssb_store:declare(?MODULE, ?SCHEMA_VERSION, ?DDL),
    {ok, #{}, {continue, maybe_rebuild}}.

%% A fresh store with feeds already on disk needs the mapping refolded.
%% Done in handle_continue rather than init so a large refold does not
%% block the supervisor (and therefore the whole boot) behind it.
handle_continue(maybe_rebuild, State) ->
    case rows("SELECT 1 FROM msg_author LIMIT 1", []) of
        [] ->
            case feed_store:feed_dirs() of
                []    -> ok;                  %% nothing stored yet either
                _Dirs -> rebuild()
            end;
        _ ->
            ok
    end,
    {noreply, State}.

handle_call(sync, _From, State) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

rows(Sql, Params) ->
    try ssb_store:q(Sql, Params) of
        L when is_list(L) -> L;
        _                 -> []
    catch _:_ -> []          %% store down: a miss, never a crash
    end.

flush_if_full(Count, Batch) when length(Batch) >= ?REBUILD_BATCH ->
    ok = flush(Batch),
    {Count, []};
flush_if_full(Count, Batch) ->
    {Count, Batch}.

flush([]) ->
    ok;
flush(Batch) ->
    _ = ssb_store:insert_many(
          "INSERT INTO msg_author(msg_id, author) VALUES(?1, ?2)"
          " ON CONFLICT(msg_id) DO UPDATE SET author=excluded.author",
          lists:reverse(Batch)),
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

mess_auth_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
             [?_test(put_get_roundtrip()),
              ?_test(unknown_is_not_found()),
              ?_test(reassignment_wins()),
              ?_test(survives_a_restart()),
              ?_test(all_auths_is_distinct())]
     end}.

setup() ->
    cleanup(ignore),
    Home = filename:join("/tmp", "mess_auth_"
                         ++ integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = ssb_store:start_link(),
    {ok, _} = start_link(),
    Home.

cleanup(Home) ->
    [catch gen_server:stop(N) || N <- [?SERVER, ssb_store, config]],
    case Home of
        ignore -> ok;
        _ -> os:cmd("rm -rf " ++ Home),
             application:unset_env(ssb, ssb_home)
    end,
    ok.

put_get_roundtrip() ->
    ok = put(~"%m1.sha256", ~"@a1.ed25519"),
    ?assertEqual(~"@a1.ed25519", get(~"%m1.sha256")).

unknown_is_not_found() ->
    ?assertEqual(not_found, get(~"%never-stored.sha256")),
    %% and a non-binary key is a miss, not a crash
    ?assertEqual(not_found, get(undefined)).

%% A message id maps to exactly one author; storing it twice must not
%% fail on the primary key.
reassignment_wins() ->
    ok = put(~"%m2.sha256", ~"@first.ed25519"),
    ok = put(~"%m2.sha256", ~"@second.ed25519"),
    ?assertEqual(~"@second.ed25519", get(~"%m2.sha256")).

%% The point of the port: the mapping is durable as written, with no
%% snapshot step, so a restart that skips any flush still has it.
survives_a_restart() ->
    ok = put(~"%m3.sha256", ~"@a3.ed25519"),
    ok = gen_server:stop(?SERVER),
    {ok, _} = start_link(),
    ?assertEqual(~"@a3.ed25519", get(~"%m3.sha256")).

all_auths_is_distinct() ->
    ok = put(~"%x1.sha256", ~"@same.ed25519"),
    ok = put(~"%x2.sha256", ~"@same.ed25519"),
    ok = put(~"%x3.sha256", ~"@other.ed25519"),
    Auths = all_auths(),
    ?assertEqual(1, length([A || A <- Auths, A =:= ~"@same.ed25519"])),
    ?assert(lists:member(~"@other.ed25519", Auths)).

-endif.
