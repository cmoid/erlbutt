%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Private feed rollup: the thread index behind patchwork's private feed.
%% Like silkpurse_threads but over the private messages we can read — a
%% stored message whose content is a `.box` we decrypt with our key.
%%
%% IMPORTANT: only metadata (ids, counts, timestamps, author) is kept in
%% the view — never the decrypted plaintext.  Bodies are decrypted on
%% demand at query time, so private content is not written to disk beyond
%% the feed's own at-rest encryption.
%%
%% Moving to ssb_store does not change that.  Everything stored here is
%% already public: a message id is a hash of the ENCRYPTED content, and
%% the author and timestamp sit in the clear in the log the message came
%% from.  What must never appear is anything obtained by decrypting, and
%% the only decrypted value that reaches the indexing path is the `root`
%% id used to thread — itself a message id.
%%
%% An ssb_view over ssb_store — `private_thread` (one row per root) and
%% `private_reply` (one per reply) — plus an ssb_plugin serving
%% privateFeed.roots/latest (source, owner).
-module(silkpurse_private).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).
-behaviour(ssb_view).
-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

-export([start_link/0]).
-export([view_version/0, view_load/0, view_reset/0, view_save/0, view_entry/1]).
-export([manifest/0, handle_rpc/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         handle_info/2, terminate/2, code_change/3]).

-define(RECENT_SHOW, 3).

-define(SCHEMA_VERSION, 1).
-define(DDL,
        ["CREATE TABLE IF NOT EXISTS private_thread("
         "  root   TEXT PRIMARY KEY,"
         "  author TEXT,"             %% null until the root itself arrives
         "  ts     INTEGER,"
         "  last   INTEGER NOT NULL DEFAULT 0) WITHOUT ROWID;",
         "CREATE INDEX IF NOT EXISTS ix_private_last"
         "  ON private_thread(last DESC);",
         "CREATE TABLE IF NOT EXISTS private_reply("
         "  root TEXT NOT NULL,"
         "  msg  TEXT NOT NULL,"
         "  ts   INTEGER NOT NULL,"
         "  PRIMARY KEY (root, msg)) WITHOUT ROWID;"]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% ssb_view callbacks
%%%===================================================================

view_version() -> 1.

view_load() ->
    case ssb_store:complete(?MODULE) of
        true  -> ok;
        false -> empty
    end.

view_reset() ->
    _ = ssb_store:clear_complete(?MODULE),
    [ssb_store:exec(["DELETE FROM ", T, ";"])
     || T <- ["private_thread", "private_reply"]],
    ok.

%% Rows are already durable; the only thing to record is that this view's
%% state is complete up to the manager's checkpoints.
view_save() ->
    _ = ssb_store:mark_complete(?MODULE),
    ok.

%% Decrypt just enough to thread the message; index ids/counts only.
view_entry(#message{id = Id, author = Author, timestamp = Ts, content = Box})
  when is_binary(Box) ->
    case decrypt_content(Box) of
        {ok, {Props}} ->
            case classify(?pgv(~"type", Props), ?pgv(~"root", Props)) of
                root ->
                    set_root(Id, Author, Ts),
                    {events, [{priv, Id}]};
                {reply, RootId} ->
                    add_reply(RootId, Author, Id, Ts),
                    {events, [{priv, RootId}]};
                ignore ->
                    ok
            end;
        _ ->
            ok
    end;
view_entry(_) ->
    ok.

%%%===================================================================
%%% ssb_plugin callbacks
%%%===================================================================

manifest() ->
    [{[~"patchwork", ~"privateFeed", ~"roots"],  source, owner},
     {[~"patchwork", ~"privateFeed", ~"latest"], source, owner}].

handle_rpc([~"patchwork", ~"privateFeed", ~"roots"], Args, _Caller) ->
    Opts    = opts(Args),
    Reverse = maps:get(reverse, Opts, true),
    Limit   = maps:get(limit, Opts, undefined),
    Resume  = maps:get(resume, Opts, undefined),
    {ResumeSql, ResumeP} = resume_clause(Resume, Reverse),
    Found = rows(["SELECT t.root, t.last,"
                  " (SELECT count(*) FROM private_reply r WHERE r.root = t.root)"
                  " FROM private_thread t"
                  %% author IS NULL means only replies have been seen, so
                  %% the thread is not showable yet
                  " WHERE t.author IS NOT NULL", ResumeSql,
                  order_sql(Reverse), limit_sql(Limit)], ResumeP),
    {source, [{json, encode_json(Item)}
              || [RootId, Last, Total] <- Found,
                 (Item = item(RootId, Total, Last)) =/= undefined]};

handle_rpc([~"patchwork", ~"privateFeed", ~"latest"], _Args, _Caller) ->
    EventFun =
        fun({priv, RootId}) ->
                case summary(RootId) of
                    {Total, Last} ->
                        case item(RootId, Total, Last) of
                            undefined -> skip;
                            Item      -> {send, encode_json(Item)}
                        end;
                    undefined -> skip
                end
        end,
    {live_source, [], ?MODULE, EventFun}.

%% {Total, Last} for a showable thread, or undefined.
summary(RootId) ->
    case rows("SELECT t.last,"
              " (SELECT count(*) FROM private_reply r WHERE r.root = t.root)"
              " FROM private_thread t"
              " WHERE t.author IS NOT NULL AND t.root = ?1", [RootId]) of
        [[Last, Total]] -> {Total, Last};
        _               -> undefined
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = ssb_store:declare(?MODULE, ?SCHEMA_VERSION, ?DDL),
    {ok, #{}, {continue, register}}.

%% Failures are loud and transient ones retried on a timer
%% (ssb_view:ensure_registered) — the old silent noproc swallow here cost
%% EarlButt its messagesByType method (July 2026).
handle_continue(register, State) ->
    ensure_registered(State).

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
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
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal: indexing
%%%===================================================================

classify(~"post", undefined)               -> root;
classify(~"post", R) when is_binary(R)     -> {reply, R};
classify(~"about", R) when is_binary(R)    -> {reply, R};
classify(_, _)                             -> ignore.

%% A reply may have created the thread row first, so `last` moves up only.
set_root(RootId, Author, Ts) ->
    _ = write("INSERT INTO private_thread(root, author, ts, last)"
              " VALUES(?1, ?2, ?3, ?4)"
              " ON CONFLICT(root) DO UPDATE SET"
              "   author=excluded.author, ts=excluded.ts,"
              "   last=max(private_thread.last, excluded.last)",
              [RootId, Author, num_or_null(Ts), num(Ts)]),
    ok.

add_reply(RootId, _ReplyAuthor, ReplyId, Ts) ->
    %% As in silkpurse_threads: the reply row's primary key is what keeps a
    %% redelivered message from inflating the count, which the old
    %% unconditional counter could not.
    _ = write("INSERT INTO private_reply(root, msg, ts) VALUES(?1, ?2, ?3)"
              " ON CONFLICT(root, msg) DO UPDATE SET ts=excluded.ts",
              [RootId, ReplyId, num(Ts)]),
    _ = write("INSERT INTO private_thread(root, last) VALUES(?1, ?2)"
              " ON CONFLICT(root) DO UPDATE SET"
              "   last=max(private_thread.last, excluded.last)",
              [RootId, num(Ts)]),
    ok.

%% `last` is the sort key and NOT NULL, so a missing or non-numeric
%% self-asserted timestamp floors at 0 rather than propagating null.
num(Ts) when is_integer(Ts) -> Ts;
num(Ts) when is_float(Ts)   -> trunc(Ts);
num(_)                      -> 0.

num_or_null(Ts) when is_integer(Ts) -> Ts;
num_or_null(Ts) when is_float(Ts)   -> trunc(Ts);
num_or_null(_)                      -> undefined.

%%%===================================================================
%%% Internal: query
%%%===================================================================

opts([{Props}]) ->
    lists:foldl(
      fun({~"reverse", V}, Acc) when is_boolean(V) -> Acc#{reverse => V};
         ({~"limit",   V}, Acc) when is_integer(V) -> Acc#{limit => V};
         ({~"resume",  V}, Acc) when is_integer(V) -> Acc#{resume => V};
         (_, Acc) -> Acc
      end, #{}, Props);
opts(_) ->
    #{}.

resume_clause(undefined, _Reverse) -> {"", []};
resume_clause(Resume, true)        -> {" AND t.last < ?1", [Resume]};
resume_clause(Resume, false)       -> {" AND t.last > ?1", [Resume]}.

order_sql(true)  -> " ORDER BY t.last DESC";
order_sql(false) -> " ORDER BY t.last ASC".

%% Interpolated rather than bound: opts/1 only admits an integer here, and
%% SQLite will not take a parameter for LIMIT in every position.
limit_sql(N) when is_integer(N), N >= 0 -> [" LIMIT ", integer_to_list(N)];
limit_sql(_)                            -> "".

%% The newest few reply ids, which the capped in-memory list used to hold
%% directly.
recent_replies(RootId) ->
    [Id || [Id] <- rows(["SELECT msg FROM private_reply WHERE root = ?1"
                         " ORDER BY ts DESC, msg DESC LIMIT ",
                         integer_to_list(?RECENT_SHOW)], [RootId])].

rows(Sql, Params) ->
    try ssb_store:q(Sql, Params) of
        L when is_list(L) -> L;
        _                 -> []
    catch _:_ -> []          %% store down: no index, never a crash
    end.

write(Sql, Params) ->
    catch ssb_store:write(Sql, Params).

%% The roots item with decrypted root + recent replies, or undefined if
%% the root body can no longer be decrypted.
item(RootId, Total, Last) ->
    case decrypted(RootId) of
        {RootProps} ->
            Replies = [R || Id <- recent_replies(RootId),
                            (R = decrypted(Id)) =/= undefined],
            {RootProps ++ [{~"totalReplies", Total},
                           {~"latestReplies", Replies},
                           {~"bumps", []},
                           {~"rts", Last}]};
        undefined ->
            undefined
    end.

%% The stored private message decrypted to a {key, value, timestamp}
%% EJSON envelope (content object, private: true), or undefined.
decrypted(MsgId) ->
    case fetch_raw(MsgId) of
        #message{content = Box} = Msg when is_binary(Box) ->
            case decrypt_content(Box) of
                {ok, ContentObj} ->
                    try utils:nat_decode(message:encode_decrypted(Msg, ContentObj))
                    catch _:_ -> undefined
                    end;
                _ -> undefined
            end;
        _ -> undefined
    end.

fetch_raw(MsgId) ->
    case mess_auth:get(MsgId) of
        not_found -> undefined;
        Author ->
            try
                Pid = utils:find_or_create_feed_pid(Author),
                case ssb_feed:fetch_msg(Pid, MsgId) of
                    not_found -> undefined;
                    Msg       -> Msg
                end
            catch _:_ -> undefined
            end
    end.

%% {ok, ContentObj} when Box is a private message we can read.
decrypt_content(Box) ->
    case private_box:is_private(Box) andalso private_box:decrypt(Box) of
        {ok, Plain} ->
            try {ok, utils:nat_decode(Plain)}
            catch _:_ -> error
            end;
        _ ->
            error
    end.

encode_json(Term) ->
    iolist_to_binary(message:ssb_encoder(Term, fun message:ssb_encoder/3, [pretty])).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

private_test_() ->
    {setup, fun pv_setup/0, fun pv_teardown/1,
     fun(_) -> [?_test(rolls_up_private_thread()),
                ?_test(stores_no_plaintext()),
                ?_test(survives_a_restart())] end}.

pv_setup() ->
    pv_teardown(ignore),
    Home = filename:join("/tmp", "pv_" ++
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
    {ok, _} = silkpurse_private:start_link(),
    ok = wait_view_ready(silkpurse_private),
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

pv_teardown(Home) ->
    [catch gen_server:stop(N)
     || N <- [silkpurse_private, view_manager, ssb_feed_sup, blobs,
              mess_auth, ssb_store, keys, config]],
    case Home of
        ignore -> ok;
        _ -> os:cmd("rm -rf " ++ Home), application:unset_env(ssb, ssb_home)
    end,
    ok.

rolls_up_private_thread() ->
    Me     = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(Me),
    ok = ssb_feed:post_private(
           OwnPid, {[{~"type", ~"post"}, {~"text", ~"dm root"},
                     {~"recps", [Me]}]}, [Me]),
    #message{id = RootId} = ssb_feed:fetch_last_msg(OwnPid),
    ok = ssb_feed:post_private(
           OwnPid, {[{~"type", ~"post"}, {~"text", ~"dm reply"},
                     {~"root", RootId}, {~"recps", [Me]}]}, [Me]),
    %% a public post must NOT enter the private feed
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"}, {~"text", ~"public"}]}),
    {source, Items} =
        handle_rpc([~"patchwork", ~"privateFeed", ~"roots"], [{[]}],
                   #{class => owner, feed_id => Me}),
    Decoded = [utils:nat_decode(B) || {json, B} <- Items],
    ?assertEqual(1, length(Decoded)),
    [{Props}] = Decoded,
    ?assertEqual(RootId, proplists:get_value(~"key", Props)),
    ?assertEqual(1, proplists:get_value(~"totalReplies", Props)),
    {Value} = proplists:get_value(~"value", Props),
    ?assertEqual(true, proplists:get_value(~"private", Value)),
    {Content} = proplists:get_value(~"content", Value),
    ?assertEqual(~"dm root", proplists:get_value(~"text", Content)).

%% The rule this view exists under: decrypted text is never written down.
%% Asserted against the database file itself rather than the tables,
%% because the point is what ends up on disk — and SQLite keeps freed
%% pages around, so a value written and later deleted would still be
%% found here.
stores_no_plaintext() ->
    Me     = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(Me),
    Secret = ~"attack at dawn by the old mill",
    ok = ssb_feed:post_private(
           OwnPid, {[{~"type", ~"post"}, {~"text", Secret},
                     {~"recps", [Me]}]}, [Me]),
    #message{id = RootId} = ssb_feed:fetch_last_msg(OwnPid),
    ok = ssb_feed:post_private(
           OwnPid, {[{~"type", ~"post"}, {~"text", ~"and bring the ladders"},
                     {~"root", RootId}, {~"recps", [Me]}]}, [Me]),
    %% the thread is indexed...
    ?assertMatch([[1]],
                 ssb_store:q("SELECT count(*) FROM private_thread"
                             " WHERE root=?1 AND author IS NOT NULL",
                             [RootId])),
    %% ...but nothing that came out of a decrypt is in the file
    ok = ssb_store:exec("PRAGMA wal_checkpoint(FULL)"),
    {ok, Db} = file:read_file(ssb_store:db_file()),
    %% positive control: the id IS stored, so finding it proves this
    %% search can find things and the nomatch below is a real result
    %% rather than an unflushed or unreadable file
    ?assertNotEqual(nomatch, binary:match(Db, RootId)),
    ?assertEqual(nomatch, binary:match(Db, Secret)),
    ?assertEqual(nomatch, binary:match(Db, ~"and bring the ladders")).

%% The point of the port: durable as written, with no snapshot step.
survives_a_restart() ->
    Me     = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(Me),
    ok = ssb_feed:post_private(
           OwnPid, {[{~"type", ~"post"}, {~"text", ~"persisted dm"},
                     {~"recps", [Me]}]}, [Me]),
    #message{id = RootId} = ssb_feed:fetch_last_msg(OwnPid),
    ok = ssb_feed:post_private(
           OwnPid, {[{~"type", ~"post"}, {~"text", ~"persisted reply"},
                     {~"root", RootId}, {~"recps", [Me]}]}, [Me]),
    ok = gen_server:stop(ssb_store),
    {ok, _} = ssb_store:start_link(),
    ?assertMatch({1, _}, summary(RootId)),
    {source, Items} =
        handle_rpc([~"patchwork", ~"privateFeed", ~"roots"], [{[]}],
                   #{class => owner, feed_id => Me}),
    Decoded = [utils:nat_decode(B) || {json, B} <- Items],
    [{Props}] = [{P} || {P} <- Decoded,
                        proplists:get_value(~"key", P) =:= RootId],
    ?assertEqual(1, proplists:get_value(~"totalReplies", Props)).

-endif.
