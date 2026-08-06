%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Thread rollup: the index behind the public feed.  For each thread
%% root it tracks the reply count, the most recent replies, and the
%% last-activity time used to order (bump) the feed.  This is the
%% erlbutt-native equivalent of patchwork's publicFeed/thread-summary
%% pipeline (JS composed it from createFeedStream + LookupRoots +
%% threadSummary; here it is a single fold over the log).
%%
%% An ssb_view over ssb_store plus an ssb_plugin serving publicFeed.roots
%% (source, owner).  Three tables: `thread` (one row per root),
%% `thread_reply` (one per reply) and `thread_actor` (participants and
%% mentions).
%%
%% This was one ETS map per root holding every field at once, and the
%% decomposition is the point of the port rather than a side effect.  Each
%% feed tab is a filter over threads ordered by activity, and with the
%% summary sealed inside a map every one of them — participating,
%% mentions, profile, channel — had to fold the entire index and filter in
%% Erlang before it could sort and paginate.  As rows they are indexed
%% lookups with ORDER BY and LIMIT, so the work is proportional to the
%% page returned rather than to the number of threads that exist.
%%
%% Replies are kept individually rather than as a capped recent-list with
%% a separate counter.  That cap existed because the list lived in RAM;
%% on disk the reply count is COUNT(*), which also fixes a latent bug —
%% the counter was incremented unconditionally, so a message redelivered
%% in the window between a checkpoint flush and a crash inflated the
%% thread's reply count permanently.  A primary key cannot double-count.
%%
%% Message bodies are NOT stored: the view holds ids, counts and
%% timestamps, and bodies are fetched from the per-feed store at query
%% time (as backlinks/by_type do).
%%
%% A thread root is a type=post message with no `root` field; a reply
%% is a type post|about carrying content.root.
%%
%% Forks are threads too.  Replying to a message part-way down a thread
%% makes patchwork pin `root` to THAT message rather than to the thread's
%% first one, so a fork's root is itself a reply and classify/2 never calls
%% it a root.  Such a thread is created by add_reply/5 with a null author
%% and has to be completed separately — see complete_root/4, which handles
%% both arrival orders.  Before that it stayed null-author forever, which
%% roots/2 reads as "root not replicated yet" and hides.
%%
%% Deferred: the live publicFeed.latest stream, and patchwork's channel /
%% subscription filterResult policy.
-module(silkpurse_threads).

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

-define(RECENT_SHOW, 3).       %% recent replies returned to the client

%% thread_actor.kind
-define(PARTICIPANT, 1).
-define(MENTION,     2).

-define(SCHEMA_VERSION, 1).
-define(DDL,
        ["CREATE TABLE IF NOT EXISTS thread("
         "  root    TEXT PRIMARY KEY,"
         "  author  TEXT,"           %% null until the root itself arrives
         "  ts      INTEGER,"
         "  last    INTEGER NOT NULL DEFAULT 0,"
         "  channel TEXT) WITHOUT ROWID;",
         %% every feed tab orders by activity, so each filter gets an
         %% index whose trailing column is `last` and can be walked in
         %% order rather than sorted after the fact
         "CREATE INDEX IF NOT EXISTS ix_thread_last"
         "  ON thread(last DESC);",
         "CREATE INDEX IF NOT EXISTS ix_thread_author"
         "  ON thread(author, last DESC);",
         "CREATE INDEX IF NOT EXISTS ix_thread_channel"
         "  ON thread(channel, last DESC);",
         "CREATE TABLE IF NOT EXISTS thread_reply("
         "  root TEXT NOT NULL,"
         "  msg  TEXT NOT NULL,"
         "  ts   INTEGER NOT NULL,"
         "  PRIMARY KEY (root, msg)) WITHOUT ROWID;",
         "CREATE TABLE IF NOT EXISTS thread_actor("
         "  root TEXT NOT NULL,"
         "  feed TEXT NOT NULL,"
         "  kind INTEGER NOT NULL,"
         "  PRIMARY KEY (root, feed, kind)) WITHOUT ROWID;",
         %% participating/mentions ask "which threads is this feed in",
         %% which the root-first primary key cannot answer
         "CREATE INDEX IF NOT EXISTS ix_thread_actor_feed"
         "  ON thread_actor(feed, kind);"]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% ssb_view callbacks (run in the view_manager process)
%%%===================================================================

%% 2: summaries gained participants/mentions/channel for the feed
%% rollups (participating/mentions/profile/channel), so upgrading nodes
%% must refold.
%% 3: fork roots are completed (see complete_root/4).  Threads already
%% indexed under a null author would otherwise stay hidden forever, so
%% upgrading nodes must refold to pick them up.
view_version() -> 3.

view_load() ->
    case ssb_store:complete(?MODULE) of
        true  -> ok;
        false -> empty
    end.

view_reset() ->
    _ = ssb_store:clear_complete(?MODULE),
    [ssb_store:exec(["DELETE FROM ", T, ";"])
     || T <- ["thread", "thread_reply", "thread_actor"]],
    ok.

%% Rows are already durable; the only thing to record is that this view's
%% state is complete up to the manager's checkpoints.
view_save() ->
    _ = ssb_store:mark_complete(?MODULE),
    ok.

view_entry(#message{id = Id, author = Author, timestamp = Ts,
                    content = {Props}}) ->
    Type = ?pgv(~"type", Props),
    Root = ?pgv(~"root", Props),
    case classify(Type, Root) of
        root ->
            set_root(Id, Author, Ts, Props),  %% the root's own id keys the thread
            {events, [{thread, Id}]};
        {reply, RootId} ->
            add_reply(RootId, Author, Id, Ts, Props),
            %% This message may itself be something else forked off, in
            %% which case a thread row keyed by OUR id is waiting for an
            %% author that set_root/4 will never supply.
            complete_root(Id, Author, Ts, Props),
            {events, [{thread, RootId}]};
        ignore ->
            ok
    end;
view_entry(_) ->
    ok.

%%%===================================================================
%%% ssb_plugin callbacks (run in each connection's rpc_processor)
%%%===================================================================

%% Every feed tab is thread roots matching a predicate, ordered by
%% activity — publicFeed with a filter.  roots is the paginated history,
%% latest the live prepend.
manifest() ->
    [{[~"patchwork", ~"recentFeeds"], source, owner} |
     lists:flatten(
       [[{[~"patchwork", Feed, ~"roots"],  source, owner},
         {[~"patchwork", Feed, ~"latest"], source, owner}]
        || Feed <- [~"publicFeed", ~"networkFeed", ~"participatingFeed",
                    ~"mentionsFeed", ~"profile", ~"channelFeed"]])].

handle_rpc([~"patchwork", ~"recentFeeds"], Args, _Caller) ->
    recent_feeds(Args);
handle_rpc([~"patchwork", Feed, ~"roots"], Args, _Caller) ->
    roots(Feed, Args);
handle_rpc([~"patchwork", Feed, ~"latest"], _Args, _Caller) ->
    latest(Feed).

%% recentFeeds({since}): feed ids that started a thread since `since`,
%% most recent first — the "recently updated" discovery list.  A snapshot
%% (the live case is not needed: the client polls with live:false).
recent_feeds(Args) ->
    Since = case Args of
                [{Props}] -> case ?pgv(~"since", Props) of
                                 S when is_integer(S) -> S;
                                 _                    -> 0
                             end;
                _ -> 0
            end,
    Rows = rows("SELECT author, max(ts) AS t FROM thread"
                " WHERE author IS NOT NULL AND ts IS NOT NULL"
                " GROUP BY author HAVING t > ? ORDER BY t DESC", [Since]),
    {source, [{json, encode_json(A)} || [A, _Ts] <- Rows]}.

%% Paginated thread roots in Feed's scope, newest activity first.
roots(Feed, Args) ->
    Opts    = opts(Args),
    Reverse = maps:get(reverse, Opts, true),
    Limit   = maps:get(limit, Opts, undefined),
    Resume  = maps:get(resume, Opts, undefined),
    {Join, Where, ScopeP} = feed_scope(Feed, Args),
    {BlockSql, BlockP}    = block_clause(),
    {ResumeSql, ResumeP}  = resume_clause(Resume, Reverse),
    Sql = ["SELECT t.root, t.last,"
           " (SELECT count(*) FROM thread_reply r WHERE r.root = t.root)"
           " FROM thread t", Join,
           %% author IS NULL means only replies have been seen so far, so
           %% the thread is not showable yet
           " WHERE t.author IS NOT NULL", Where, BlockSql, ResumeSql,
           order_sql(Reverse), limit_sql(Limit)],
    Found = rows(Sql, ScopeP ++ BlockP ++ ResumeP),
    {source, [{json, encode_json(Item)}
              || [RootId, Last, Total] <- Found,
                 (Item = item(RootId, Total, Last)) =/= undefined]}.

%% Live prepend: a root item each time a thread in scope gains activity.
latest(Feed) ->
    EventFun =
        fun({thread, RootId}) ->
                case in_scope(Feed, RootId) of
                    {true, Total, Last} ->
                        case item(RootId, Total, Last) of
                            undefined -> skip;
                            Item      -> {send, encode_json(Item)}
                        end;
                    false -> skip
                end
        end,
    {live_source, [], ?MODULE, EventFun}.

%% Does one root pass this feed's scope right now?  The same predicate as
%% roots/2, asked of a single row — expressed as SQL rather than a
%% separate Erlang path so the live tail and the backlog cannot drift.
in_scope(Feed, RootId) ->
    {Join, Where, ScopeP} = feed_scope(Feed, []),
    {BlockSql, BlockP}    = block_clause(),
    Sql = ["SELECT t.last,"
           " (SELECT count(*) FROM thread_reply r WHERE r.root = t.root)"
           " FROM thread t", Join,
           " WHERE t.author IS NOT NULL AND t.root = ?", Where, BlockSql],
    case rows(Sql, [RootId] ++ ScopeP ++ BlockP) of
        [[Last, Total] | _] -> {true, Total, Last};
        _                   -> false
    end.

%% {Join, Where, Params} for each feed tab.  owner-relative feeds use the
%% node's own id; profile/channel take their target from the request
%% options.
%%
%% latest/1 asks with no Args, so profile/channelFeed have no target and
%% match nothing — same as the old predicate comparing against undefined.
feed_scope(Feed, _Args) when Feed =:= ~"publicFeed";
                             Feed =:= ~"networkFeed" ->
    {"", "", []};
feed_scope(~"participatingFeed", _Args) ->
    actor_scope(keys:pub_key_disp(), ?PARTICIPANT);
feed_scope(~"mentionsFeed", _Args) ->
    actor_scope(keys:pub_key_disp(), ?MENTION);
feed_scope(~"profile", Args) ->
    {"", " AND t.author = ?", [arg(~"id", Args)]};
feed_scope(~"channelFeed", Args) ->
    {"", " AND t.channel = ?", [arg(~"channel", Args)]}.

actor_scope(Feed, Kind) ->
    {" JOIN thread_actor a ON a.root = t.root",
     " AND a.feed = ? AND a.kind = ?", [Feed, Kind]}.

%% Feeds the node owner blocks — their threads are hidden.  Asked of
%% ssb_social_graph rather than joined against its table: the block list
%% is small, and an app view reaching into a core view's schema would
%% couple the two in the direction the layering forbids.
block_clause() ->
    case ssb_social_graph:blocks(keys:pub_key_disp()) of
        []      -> {"", []};
        Blocked -> {[" AND t.author NOT IN (",
                     lists:join(",", ["?" || _ <- Blocked]), ")"],
                    Blocked}
    end.

resume_clause(undefined, _Reverse) -> {"", []};
resume_clause(Resume, true)        -> {" AND t.last < ?", [Resume]};
resume_clause(Resume, false)       -> {" AND t.last > ?", [Resume]}.

order_sql(true)  -> " ORDER BY t.last DESC";
order_sql(false) -> " ORDER BY t.last ASC".

%% Interpolated rather than bound: opts/1 only admits an integer here, and
%% SQLite will not take a parameter for LIMIT in every position.
limit_sql(N) when is_integer(N), N >= 0 -> [" LIMIT ", integer_to_list(N)];
limit_sql(_)                            -> "".

arg(Key, [{Props}]) -> ?pgv(Key, Props);
arg(_Key, _)        -> undefined.

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

%% Casts are delivered in order, so a reply to this means every
%% complete_fork_root cast sent before it has already been applied.
%% Exists for the tests; harmless in production.
handle_call(sync, _From, State) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% Deferred out of view_entry/1 — see maybe_complete_fork_root/1 for why
%% the message cannot be read on the ingest path.
handle_cast({complete_fork_root, RootId}, State) ->
    _ = case decoded(RootId) of
            undefined -> ok;   %% not replicated yet; nothing to attribute
            {Props}   -> complete_from_stored(RootId, Props)
        end,
    {noreply, State};
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
%%% Internal: indexing
%%%===================================================================

classify(~"post", undefined) ->
    root;
classify(~"post", Root) when is_binary(Root) ->
    {reply, Root};
classify(~"about", Root) when is_binary(Root) ->
    {reply, Root};
classify(_, _) ->
    ignore.

%% A root message: record its author/ts/channel, bump last activity, and
%% add the author + its mentions as actors.  A reply may have created the
%% thread row first, so this must not clobber `last` downwards — hence
%% max() rather than assignment.
set_root(RootId, Author, Ts, Props) ->
    _ = write("INSERT INTO thread(root, author, ts, last, channel)"
              " VALUES(?, ?, ?, ?, ?)"
              " ON CONFLICT(root) DO UPDATE SET"
              "   author=excluded.author, ts=excluded.ts,"
              "   channel=excluded.channel,"
              "   last=max(thread.last, excluded.last)",
              [RootId, Author, num_or_null(Ts), num(Ts), channel_of(Props)]),
    add_actors(RootId, [Author], ?PARTICIPANT),
    add_actors(RootId, mentions_of(Props), ?MENTION),
    ok.

add_reply(RootId, ReplyAuthor, ReplyId, Ts, Props) ->
    %% The reply row carries the dedup: redelivering a reply is a no-op on
    %% (root, msg), where the old counter incremented regardless.
    _ = write("INSERT INTO thread_reply(root, msg, ts) VALUES(?, ?, ?)"
              " ON CONFLICT(root, msg) DO UPDATE SET ts=excluded.ts",
              [RootId, ReplyId, num(Ts)]),
    _ = write("INSERT INTO thread(root, last) VALUES(?, ?)"
              " ON CONFLICT(root) DO UPDATE SET"
              "   last=max(thread.last, excluded.last)",
              [RootId, num(Ts)]),
    add_actors(RootId, [ReplyAuthor], ?PARTICIPANT),
    add_actors(RootId, mentions_of(Props), ?MENTION),
    maybe_complete_fork_root(RootId),
    ok.

%% Reply to a message part-way down a thread and patchwork pins `root` to
%% THAT message rather than to the thread's first one — a fork.  The row
%% add_reply/5 just created is therefore keyed by a message that is itself
%% a reply, so classify/2 will never call it a root and set_root/4 will
%% never fill in its author.  A null author means "hidden" to roots/2, and
%% for a fork that state is permanent rather than lasting until the root
%% replicates.
%%
%% Two arrival orders, two repairs.  Here the forked-from message is
%% already stored, so complete it from storage; when it arrives later,
%% view_entry/1's reply branch does it instead.
%%
%% The fetch cannot happen inline.  view_entry/1 runs inside view_manager's
%% synchronous ingest, which ssb_feed:store/2 called — so reading the
%% message would gen_server:call back into the feed process that is blocked
%% waiting on this very ingest, and deadlock until the call times out.
%% Hand it to our own process instead, which is free to read.
%%
%% Guarded on the null author so this happens at most once per thread
%% rather than once per reply.
maybe_complete_fork_root(RootId) ->
    case pending_root(RootId) of
        false -> ok;
        true  -> gen_server:cast(?MODULE, {complete_fork_root, RootId})
    end.

complete_from_stored(RootId, Props) ->
    case ?pgv(~"value", Props) of
        {VP} ->
            do_complete_root(RootId, ?pgv(~"author", VP), ?pgv(~"timestamp", VP),
                             content_props(?pgv(~"content", VP)));
        _ ->
            ok
    end.

content_props({CProps}) -> CProps;
content_props(_)        -> [].

%% Finish a thread row a reply created but no root ever claimed.  UPDATE,
%% never INSERT: an ordinary reply must not manufacture a thread of its
%% own, only complete one that something forked off it.
complete_root(RootId, Author, Ts, Props) ->
    case pending_root(RootId) of
        false -> ok;
        true  -> do_complete_root(RootId, Author, Ts, Props)
    end.

do_complete_root(RootId, Author, Ts, Props) ->
    _ = write("UPDATE thread SET author=?, ts=?, channel=?"
              " WHERE root=? AND author IS NULL",
              [Author, num_or_null(Ts), channel_of(Props), RootId]),
    add_actors(RootId, [Author], ?PARTICIPANT),
    add_actors(RootId, mentions_of(Props), ?MENTION),
    ok.

%% A thread row exists but has never been attributed: either its root has
%% not replicated yet, or the root is a fork target and never will be
%% attributed by set_root/4.
pending_root(RootId) ->
    case ssb_store:q("SELECT 1 FROM thread WHERE root=? AND author IS NULL",
                     [RootId]) of
        [_ | _] -> true;
        _       -> false
    end.

add_actors(RootId, Feeds, Kind) ->
    Rows = [[RootId, F, Kind] || F <- Feeds, is_binary(F)],
    _ = ssb_store:insert_many(
          "INSERT INTO thread_actor(root, feed, kind) VALUES(?, ?, ?)"
          " ON CONFLICT(root, feed, kind) DO NOTHING", Rows),
    ok.

%% Timestamps are self-asserted and may be missing or non-numeric; `last`
%% is NOT NULL because it is the sort key, so it floors at 0.
num(Ts) when is_integer(Ts) -> Ts;
num(Ts) when is_float(Ts)   -> trunc(Ts);
num(_)                      -> 0.

num_or_null(Ts) when is_integer(Ts) -> Ts;
num_or_null(Ts) when is_float(Ts)   -> trunc(Ts);
num_or_null(_)                      -> undefined.

%% content.channel, when a plain string.
channel_of(Props) ->
    case ?pgv(~"channel", Props) of
        Ch when is_binary(Ch) -> Ch;
        _                     -> undefined
    end.

%% Feed ids named in content.mentions ([{link, "@..."}]).
mentions_of(Props) ->
    case ?pgv(~"mentions", Props) of
        Ms when is_list(Ms) ->
            [Link || {MProps} <- Ms,
                     is_feed_ref(Link = ?pgv(~"link", MProps))];
        _ -> []
    end.

%% A mention link counts as a feed reference only if it is a binary
%% starting with "@".
%%
%% Matching the prefix rather than binary:part(Link, 0, 1): part/3 raises
%% badarg on <<>>, which is_binary/1 does not exclude, so a mention whose
%% link was the empty string took the whole view down for that message —
%% and view_manager:deliver/2 advances the checkpoint even when
%% view_entry/1 raises, so each one was silently dropped for good.
%% Matching also covers the non-binary case, so no separate guard.
is_feed_ref(<<"@", _/binary>>) -> true;
is_feed_ref(_)                 -> false.


%%%===================================================================
%%% Internal: query
%%%===================================================================

opts([{Props}]) ->
    lists:foldl(
      fun({K, V}, Acc) ->
              case K of
                  ~"reverse" when is_boolean(V) -> Acc#{reverse => V};
                  ~"limit"   when is_integer(V) -> Acc#{limit => V};
                  ~"resume"  when is_integer(V) -> Acc#{resume => V};
                  _ -> Acc
              end
      end, #{}, Props);
opts(_) ->
    #{}.

%% Build the roots item: the root message envelope extended with
%% totalReplies, latestReplies (full messages) and bumps, plus rts (the
%% activity time) as the pagination cursor.
item(RootId, Total, Last) ->
    case decoded(RootId) of
        {RootProps} ->
            Replies = [R || Id <- recent_replies(RootId),
                            (R = decoded(Id)) =/= undefined],
            Bumps = [bump(R) || R <- Replies],
            {RootProps ++ [{~"totalReplies", Total},
                           {~"latestReplies", Replies},
                           {~"bumps", Bumps},
                           {~"rts", Last}]};
        undefined ->
            undefined                    %% root body not fetchable; skip
    end.

%% The newest few reply ids, which the capped in-memory list used to hold
%% directly.
recent_replies(RootId) ->
    [Id || [Id] <- rows(["SELECT msg FROM thread_reply WHERE root = ?"
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

%% The stored message as {key, value, timestamp} EJSON, or undefined.
decoded(MsgId) ->
    case fetch_encoded(MsgId) of
        undefined -> undefined;
        Bin       -> utils:nat_decode(Bin)
    end.

bump({Props}) ->
    Value = ?pgv(~"value", Props),
    Author = case Value of {VP} -> ?pgv(~"author", VP); _ -> undefined end,
    {[{~"type", ~"reply"},
      {~"author", Author},
      {~"id", ?pgv(~"key", Props)}]}.

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

encode_json(Term) ->
    iolist_to_binary(message:ssb_encoder(Term, fun message:ssb_encoder/3, [pretty])).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

%% mentions_of/1 must survive whatever a mention holds: the links come off
%% the wire.  Regression for an empty-string link, which is_binary/1 let
%% through to binary:part(<<>>, 0, 1) -- badarg, killing the view for that
%% message during a bulk import of a real log.
mentions_of_tolerates_junk_links_test() ->
    Mk = fun(Links) ->
        [{~"mentions", [{[{~"link", L}]} || L <- Links]}]
    end,
    Feed = ~"@abc=.ed25519",
    %% the crashing case, alone and mixed with a good one
    ?assertEqual([], mentions_of(Mk([~""]))),
    ?assertEqual([Feed], mentions_of(Mk([~"", Feed]))),
    %% other shapes a link can take
    ?assertEqual([Feed], mentions_of(Mk([Feed, ~"%msg.sha256", ~"&blob.sha256"]))),
    ?assertEqual([], mentions_of(Mk([~"no-sigil"]))),
    ?assertEqual([], mentions_of([{~"mentions", [{[{~"nolink", 1}]}]}])),
    ?assertEqual([], mentions_of([{~"mentions", [{[{~"link", 42}]}]}])),
    %% mentions absent or not a list at all
    ?assertEqual([], mentions_of([])),
    ?assertEqual([], mentions_of([{~"mentions", ~"nope"}])).

classify_test() ->
    ?assertEqual(root, classify(~"post", undefined)),
    ?assertEqual({reply, ~"%r.sha256"}, classify(~"post", ~"%r.sha256")),
    ?assertEqual({reply, ~"%r.sha256"}, classify(~"about", ~"%r.sha256")),
    ?assertEqual(ignore, classify(~"vote", undefined)),
    ?assertEqual(ignore, classify(~"contact", undefined)).

threads_test_() ->
    {foreach, fun th_setup/0, fun th_teardown/1,
     [fun(_) -> ?_test(rollup_counts_and_recent()) end,
      fun(_) -> ?_test(reply_before_root()) end,
      fun(_) -> ?_test(fork_of_stored_message_is_showable()) end,
      fun(_) -> ?_test(fork_before_forked_from_message()) end,
      fun(_) -> ?_test(complete_root_does_not_create_threads()) end,
      fun(_) -> ?_test(block_filtering()) end,
      fun(_) -> ?_test(recent_replies_are_newest_first()) end,
      fun(_) -> ?_test(redelivery_does_not_inflate_the_count()) end,
      fun(_) -> ?_test(scoped_feeds_select_their_threads()) end,
      fun(_) -> ?_test(survives_a_restart()) end]}.

th_setup() ->
    th_teardown(ignore),
    Home = filename:join("/tmp", "th_" ++
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
    {ok, _} = ssb_social_graph:start_link(),
    {ok, _} = silkpurse_threads:start_link(),
    ok = wait_view_ready(silkpurse_threads),
    Home.

%% Registration happens in handle_continue, so start_link/0 returns before
%% it lands — and registering a view whose state is not marked complete
%% resets it.  A test that seeds the index directly must therefore wait,
%% or the reset arrives mid-test and deletes what it just wrote.
%%
%% caught_up/1 alone is not enough: it answers true for a module that has
%% not registered at all, which is exactly the window being waited out.
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

th_teardown(Home) ->
    [catch gen_server:stop(Name)
     || Name <- [silkpurse_threads, ssb_social_graph, view_manager,
                 ssb_store, ssb_feed_sup, blobs, mess_auth,  keys,
                 config]],
    case Home of
        ignore -> ok;
        _ ->
            os:cmd("rm -rf " ++ Home),
            application:unset_env(ssb, ssb_home)
    end,
    ok.

post(Pid, Content) ->
    ok = ssb_feed:post_content(Pid, Content),
    ssb_feed:fetch_last_msg(Pid).

roots() ->
    {source, Items} =
        handle_rpc([~"patchwork", ~"publicFeed", ~"roots"], [{[]}],
                   #{class => owner, feed_id => keys:pub_key_disp()}),
    [utils:nat_decode(B) || {json, B} <- Items].

rollup_counts_and_recent() ->
    OwnPid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    #message{id = RootId} = post(OwnPid, {[{~"type", ~"post"},
                                           {~"text", ~"root post"}]}),
    _ = post(OwnPid, {[{~"type", ~"post"}, {~"text", ~"r1"},
                       {~"root", RootId}]}),
    _ = post(OwnPid, {[{~"type", ~"post"}, {~"text", ~"r2"},
                       {~"root", RootId}]}),
    [{Props}] = roots(),
    ?assertEqual(RootId, proplists:get_value(~"key", Props)),
    ?assertEqual(2, proplists:get_value(~"totalReplies", Props)),
    Replies = proplists:get_value(~"latestReplies", Props),
    ?assertEqual(2, length(Replies)).

%% A reply ingested before its root: the thread row is created with a null
%% author (hidden from every feed) and completed when the root arrives.
%% Tested at the index level because forcing cross-feed ordering with real
%% stored bodies is impractical here.
reply_before_root() ->
    Fake  = ~"%unseenrootxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx=.sha256",
    Reply = ~"%replyaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa=.sha256",
    add_reply(Fake, ~"@replier=.ed25519", Reply, 100, []),
    %% reply-only thread: author unknown, so not showable
    ?assertEqual([], showable()),
    set_root(Fake, keys:pub_key_disp(), 50, []),
    %% now complete, with the earlier reply counted and activity bumped
    %% past the root's own (older) timestamp
    ?assertEqual([[Fake, 100, 1]], showable()).

%% Reply to a message part-way down a thread and patchwork pins `root` to
%% that message, forking a new thread off it.  The fork's root is then a
%% reply itself, so set_root/4 never runs for it — and before this was
%% handled the fork stayed author-null and invisible in every feed.
%%
%% Forked-from message already stored: add_reply/5 completes the row from
%% storage.  End to end, because that path reads a real message body.
fork_of_stored_message_is_showable() ->
    OwnPid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    #message{id = RootId}   = post(OwnPid, {[{~"type", ~"post"},
                                             {~"text", ~"root post"}]}),
    #message{id = TargetId} = post(OwnPid, {[{~"type", ~"post"},
                                             {~"text", ~"a reply"},
                                             {~"root", RootId}]}),
    _ = post(OwnPid, {[{~"type", ~"post"}, {~"text", ~"forked off the reply"},
                       {~"root", TargetId}]}),
    %% completion is deferred off the ingest path, so wait for our own
    %% mailbox to drain before reading the index back
    ok = gen_server:call(?MODULE, sync),
    Keys = [proplists:get_value(~"key", P) || {P} <- roots()],
    %% both the original thread and the fork are listed
    ?assert(lists:member(RootId, Keys)),
    ?assert(lists:member(TargetId, Keys)).

%% The other order: the fork lands before we hold the message it forked
%% from, so there is nothing to complete it from until that message
%% arrives — as a reply, which is view_entry/1's complete_root/4 call.
%% At the index level for the same reason as reply_before_root/0.
fork_before_forked_from_message() ->
    Target   = ~"%forktargetxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx=.sha256",
    Fork     = ~"%forkreplyxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx=.sha256",
    Upstream = ~"%upstreamrootxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx=.sha256",
    add_reply(Target, ~"@forker=.ed25519", Fork, 200, []),
    %% author unknown, so hidden — same state as a root not yet replicated
    ?assertEqual([], showable()),
    %% Now the forked-from message arrives.  Driven through view_entry/1
    %% rather than calling complete_root/4 directly, so that dropping the
    %% call site fails this test too.  Note it is a REPLY (it carries a
    %% root of its own), which is exactly why set_root/4 never sees it.
    view_entry(#message{id = Target, author = keys:pub_key_disp(),
                        timestamp = 150,
                        content = {[{~"type", ~"post"},
                                    {~"text", ~"mid-thread message"},
                                    {~"root", Upstream}]}}),
    ?assertEqual([[Target, 200, 1]], showable()).

%% complete_root/4 must not turn every reply into a thread of its own: with
%% no row waiting on that id it is a no-op.
complete_root_does_not_create_threads() ->
    Orphan = ~"%noonelinkedherexxxxxxxxxxxxxxxxxxxxxxxxxxx=.sha256",
    complete_root(Orphan, keys:pub_key_disp(), 100, []),
    ?assertEqual([], showable()).

%% root, last and reply count for every thread whose root has been seen.
showable() ->
    ssb_store:q("SELECT t.root, t.last,"
                " (SELECT count(*) FROM thread_reply r WHERE r.root = t.root)"
                " FROM thread t WHERE t.author IS NOT NULL"
                " ORDER BY t.last DESC").

%% latestReplies is the newest few, which the capped in-memory list gave
%% by construction and an ORDER BY has to be asked for.
recent_replies_are_newest_first() ->
    Root = ~"%recentrootxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx=.sha256",
    A    = ~"@rr=.ed25519",
    set_root(Root, keys:pub_key_disp(), 1, []),
    [add_reply(Root, A, Id, Ts, [])
     || {Id, Ts} <- [{~"%old.sha256", 10}, {~"%new.sha256", 30},
                     {~"%mid.sha256", 20}, {~"%older.sha256", 5}]],
    %% capped at RECENT_SHOW, newest first
    ?assertEqual([~"%new.sha256", ~"%mid.sha256", ~"%old.sha256"],
                 recent_replies(Root)).

%% The old counter incremented on every delivery, so a message redelivered
%% after a crash (checkpoints flush on a timer) inflated the count for
%% good.  The reply row's primary key makes that impossible.
redelivery_does_not_inflate_the_count() ->
    Root  = ~"%dupsrootxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx=.sha256",
    A     = ~"@dup=.ed25519",
    Reply = ~"%dupreply.sha256",
    set_root(Root, keys:pub_key_disp(), 1, []),
    add_reply(Root, A, Reply, 10, []),
    add_reply(Root, A, Reply, 10, []),
    add_reply(Root, A, Reply, 10, []),
    ?assertEqual([[Root, 10, 1]], showable()).

%% Each feed tab is a different scope over the same threads; the ones
%% keyed on an actor are why thread_actor exists.
scoped_feeds_select_their_threads() ->
    Owner = keys:pub_key_disp(),
    Other = ~"@scopedotherrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr=.ed25519",
    Mine  = ~"%scopedmine.sha256",
    Theirs = ~"%scopedtheirs.sha256",
    set_root(Mine, Owner, 10, [{~"channel", ~"erlang"}]),
    set_root(Theirs, Other, 20, []),
    %% the owner replies to their thread: now a participant, not an author
    add_reply(Theirs, Owner, ~"%scopedreply.sha256", 30, []),
    %% and is mentioned in a third
    Third = ~"%scopedthird.sha256",
    set_root(Third, Other, 40,
             [{~"mentions", [{[{~"link", Owner}]}]}]),
    ?assertEqual([Mine], scope_roots(~"profile", [{[{~"id", Owner}]}])),
    ?assertEqual([Mine], scope_roots(~"channelFeed",
                                     [{[{~"channel", ~"erlang"}]}])),
    %% participating: authored one, replied to another
    ?assertEqual([Theirs, Mine], scope_roots(~"participatingFeed", [{[]}])),
    ?assertEqual([Third], scope_roots(~"mentionsFeed", [{[]}])).

%% Root ids a feed tab selects, newest activity first — the query roots/2
%% runs, without hydrating bodies (these roots have none stored).
scope_roots(Feed, Args) ->
    {Join, Where, ScopeP} = feed_scope(Feed, Args),
    [R || [R] <- ssb_store:q(["SELECT t.root FROM thread t", Join,
                              " WHERE t.author IS NOT NULL", Where,
                              " ORDER BY t.last DESC"], ScopeP)].

%% The point of the port: durable as written, with no snapshot step.
survives_a_restart() ->
    Root = ~"%persistrootxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx=.sha256",
    set_root(Root, keys:pub_key_disp(), 5, [{~"channel", ~"kept"}]),
    add_reply(Root, ~"@p=.ed25519", ~"%persistreply.sha256", 25, []),
    ok = gen_server:stop(ssb_store),
    {ok, _} = ssb_store:start_link(),
    ?assertEqual([[Root, 25, 1]], showable()),
    ?assertEqual([Root], scope_roots(~"channelFeed",
                                     [{[{~"channel", ~"kept"}]}])).

block_filtering() ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    #message{id = MineRoot} = post(OwnPid, {[{~"type", ~"post"},
                                             {~"text", ~"mine"}]}),
    %% a root from another author, whom we block
    Other = ~"@blockedauthorrrrrrrrrrrrrrrrrrrrrrrrrrrrr=.ed25519",
    set_root(~"%theirroot0000000000000000000000000000000=.sha256", Other, 999, []),
    %% block through the view rather than its storage (store-backed now)
    _ = ssb_social_graph:view_entry(
          #message{author = OwnId, sequence = 1,
                   content = {[{~"type", ~"contact"},
                               {~"contact", Other},
                               {~"blocking", true}]}}),
    Keys = [proplists:get_value(~"key", P) || {P} <- roots()],
    ?assert(lists:member(MineRoot, Keys)),
    ?assertNot(lists:member(~"%theirroot0000000000000000000000000000000=.sha256",
                            Keys)).

-endif.