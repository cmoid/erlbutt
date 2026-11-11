%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% What feeds say about themselves, for feeds we have and feeds we have
%% not.
%%
%% A glimpse is an ordinary signed message of type `glimpse` naming a
%% blob: the author's own summary of their feed, written so that somebody
%% at the edge of their graph can decide whether to replicate it.  See
%% doc/research/feed-glimpses.md.
%%
%% TWO TABLES, AND THE SPLIT IS THE WHOLE DESIGN.
%%
%% `glimpses` is a view.  It holds the glimpse of every feed we replicate,
%% folded out of the log like any other index, and it is what we serve to
%% peers.  It can be dropped and rebuilt from the feeds at any time, which
%% is exactly what view_reset/0 does.
%%
%% `glimpse_edge` is not a view and must never be treated as one.  It
%% holds glimpses for feeds we have deliberately NOT replicated — there is
%% no ssb_feed process for those authors, mess_auth has never seen them,
%% and nothing in the log mentions them.  A rebuild would erase these rows
%% and have no way to recreate them; they come back only from a peer, on
%% the next discovery round.  So view_reset/0 leaves them alone.
%%
%% WE SERVE THE VIEW, NOT THE EDGE.  A glimpse we hold for a feed we
%% replicate is one we have chain-validated in the ordinary way, and
%% carrying it for a peer is the sealed-envelope case the design doc
%% argues for.  A staged edge glimpse is unanchored hearsay we are holding
%% for one local decision; relaying it would spread self-descriptions for
%% feeds nobody at either end replicates, which is how a bounded feature
%% becomes a gossip network of its own.  A feed's glimpse reaches a new
%% node when somebody who actually replicates that feed offers it.
%%
%% NEWEST WINS, unlike archive boundaries.  A boundary is a choice about
%% how much history to keep, so the most conservative offer is the right
%% one; a glimpse is a self-description, and an old one is simply out of
%% date.  Only the newest sequence is kept per feed, here and at the edge.
-module(ssb_glimpses).

-behaviour(gen_server).
-behaviour(ssb_view).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/0,
         offers/0,
         for_feed/1,
         mine/0,
         stage/1,
         edge/0,
         forget/1]).

-export([view_version/0,
         view_class/0,
         view_load/0,
         view_reset/0,
         view_save/0,
         view_entry/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SCHEMA_VERSION, 1).

-define(DDL,
        [%% Glimpses of feeds we replicate: rebuildable, and what we serve.
         "CREATE TABLE IF NOT EXISTS glimpses("
         "  feed    TEXT PRIMARY KEY,"
         "  seq     INTEGER NOT NULL,"
         "  blob    TEXT NOT NULL,"
         "  size    INTEGER,"
         %% the author's own claim about when they wrote it; advisory, and
         %% never used to order anything (see seq)
         "  updated INTEGER,"
         %% The signed message in the value-only form EBT puts on the
         %% wire.  Serving is then a pure SELECT, and a peer verifies the
         %% AUTHOR's signature rather than taking our word for any column
         %% beside it.
         "  raw     BLOB NOT NULL) WITHOUT ROWID;",

         %% Glimpses collected from peers for feeds at our boundary, which
         %% by definition we do not replicate.  NOT rebuildable.
         "CREATE TABLE IF NOT EXISTS glimpse_edge("
         "  feed       TEXT PRIMARY KEY,"
         "  seq        INTEGER NOT NULL,"
         "  blob       TEXT NOT NULL,"
         "  size       INTEGER,"
         "  updated    INTEGER,"
         "  raw        BLOB NOT NULL,"
         %% When we first heard of this feed and when a peer last offered
         %% it: the first is how a client can show what is new at the
         %% edge, the second is how a staged glimpse for a feed nobody
         %% carries any more can eventually be aged out.
         "  first_seen INTEGER NOT NULL,"
         "  last_seen  INTEGER NOT NULL) WITHOUT ROWID;"]).

-define(COLS, "feed,seq,blob,size,updated,raw").

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Every glimpse we are willing to carry for a peer: one per replicated
%% feed, newest only.
offers() ->
    [to_map(R, feed) || R <- q("SELECT " ?COLS " FROM glimpses ORDER BY feed",
                               [])].

%% What this node can show about one feed, whether or not it replicates
%% it.  The replicated glimpse wins when both exist: it arrived through
%% chain validation, where the edge row arrived on a peer's say-so.
for_feed(FeedId) ->
    case one("SELECT " ?COLS " FROM glimpses WHERE feed = ?", [FeedId], feed) of
        {ok, _} = Found -> Found;
        none ->
            one("SELECT " ?COLS " FROM glimpse_edge WHERE feed = ?",
                [FeedId], edge)
    end.

%% Our own current glimpse, if we have published one.
mine() ->
    for_feed(keys:pub_key_disp()).

%% Stage a glimpse for a feed at our boundary.  Newest sequence wins; an
%% older offer for a feed we have already staged is dropped, which is what
%% makes a round's worth of offers order-independent.
stage(#message{author = Feed, sequence = Seq, content = {Props}} = Msg) ->
    Now = erlang:system_time(millisecond),
    _ = ssb_store:write(
          "INSERT INTO glimpse_edge(feed,seq,blob,size,updated,raw,"
          "                         first_seen,last_seen)"
          " VALUES(?,?,?,?,?,?,?,?)"
          " ON CONFLICT(feed) DO UPDATE SET"
          "   seq=excluded.seq, blob=excluded.blob, size=excluded.size,"
          "   updated=excluded.updated, raw=excluded.raw,"
          "   last_seen=excluded.last_seen"
          " WHERE excluded.seq > glimpse_edge.seq",
          [Feed, Seq, ?pgv(~"blob", Props), ?pgv(~"size", Props),
           ?pgv(~"updated", Props), message:encode_value(Msg), Now, Now]),
    %% A re-offer of the glimpse we already hold is still news: it says
    %% somebody is still carrying this feed.  The UPDATE above is guarded
    %% on sequence, so touch last_seen separately.
    _ = ssb_store:write("UPDATE glimpse_edge SET last_seen = ? WHERE feed = ?",
                        [Now, Feed]),
    ok.

%% Everything staged at the boundary, newest first by when we last heard
%% it offered.
edge() ->
    [to_map(R, edge)
     || R <- q("SELECT " ?COLS " FROM glimpse_edge ORDER BY last_seen DESC",
               [])].

%% Drop a staged glimpse.  Called when a feed is promoted — it is about to
%% be replicated, so the real thing is on its way and the stand-in has
%% done its job.
forget(FeedId) ->
    _ = ssb_store:write("DELETE FROM glimpse_edge WHERE feed = ?", [FeedId]),
    ok.

%%%===================================================================
%%% ssb_view
%%%===================================================================

view_version() -> 1.

view_class() -> core.

view_load() ->
    case ssb_store:complete(?MODULE) of
        true  -> ok;
        false -> empty
    end.

%% Only the rebuildable half.  glimpse_edge holds what no rebuild could
%% recover; see the note at the top.
view_reset() ->
    _ = ssb_store:clear_complete(?MODULE),
    _ = ssb_store:exec("DELETE FROM glimpses;"),
    ok.

view_save() ->
    _ = ssb_store:mark_complete(?MODULE),
    ok.

view_entry(#message{author = Author, sequence = Seq,
                    content = {Props}} = Msg) ->
    case ?pgv(~"type", Props) of
        ~"glimpse" -> put_glimpse(Author, Seq, Props, Msg);
        _          -> ok
    end;
view_entry(_) ->
    ok.

%% A glimpse without a blob reference is not one: there is nothing to
%% show and nothing to fetch, so indexing it would only offer peers a
%% pointer to nowhere.
put_glimpse(Feed, Seq, Props, Msg) ->
    case ?pgv(~"blob", Props) of
        <<"&", _/binary>> = Blob ->
            _ = ssb_store:write(
                  "INSERT INTO glimpses(" ?COLS ") VALUES(?,?,?,?,?,?)"
                  " ON CONFLICT(feed) DO UPDATE SET"
                  "   seq=excluded.seq, blob=excluded.blob,"
                  "   size=excluded.size, updated=excluded.updated,"
                  "   raw=excluded.raw"
                  " WHERE excluded.seq > glimpses.seq",
                  [Feed, Seq, Blob, ?pgv(~"size", Props),
                   ?pgv(~"updated", Props), message:encode_value(Msg)]),
            ok;
        _ ->
            ok
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

to_map([Feed, Seq, Blob, Size, Updated, Raw], Source) ->
    #{feed => Feed, seq => Seq, blob => Blob, size => Size,
      updated => Updated, raw => Raw, source => Source}.

one(Sql, Params, Source) ->
    case q(Sql, Params) of
        [Row] -> {ok, to_map(Row, Source)};
        _     -> none
    end.

%% A question about glimpses must not take down its asker.  Every caller
%% has a perfectly good answer for "no glimpse": show the feed id, which
%% is what every client does today.
q(Sql, Params) ->
    try ssb_store:q(Sql, Params) of
        L when is_list(L) -> L;
        _                 -> []      %% no table yet, or the store said no
    catch _:_ -> []
    end.

%%%===================================================================
%%% gen_server
%%%===================================================================

init([]) ->
    ok = ssb_store:declare(?MODULE, ?SCHEMA_VERSION, ?DDL),
    {ok, #{}, {continue, register_view}}.

handle_continue(register_view, State) ->
    ensure_registered(State).

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State)            -> {noreply, State}.

handle_info(ensure_registered, State) -> ensure_registered(State);
handle_info(_Info, State)             -> {noreply, State}.

terminate(_Reason, _State)       -> ok.
code_change(_Old, State, _Extra) -> {ok, State}.

%% Keep retrying until accepted: a silent skip means glimpses quietly stop
%% being indexed and we start offering peers nothing.
ensure_registered(State) ->
    case ssb_view:ensure_registered(?MODULE, [view]) of
        ok    -> ok;
        retry -> erlang:send_after(2000, self(), ensure_registered)
    end,
    {noreply, State}.

-ifdef(TEST).

glimpses_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
             [?_test(indexes_a_glimpse()),
              ?_test(ignores_ordinary_messages()),
              ?_test(ignores_a_glimpse_with_no_blob()),
              ?_test(newest_glimpse_replaces_the_old_one()),
              ?_test(an_older_glimpse_does_not_win()),
              ?_test(staging_takes_the_newest_too()),
              ?_test(a_replicated_glimpse_beats_a_staged_one()),
              ?_test(a_rebuild_keeps_the_edge()),
              ?_test(forgetting_drops_only_the_edge_row()),
              ?_test(is_a_core_view())]
     end}.

setup() ->
    cleanup(ignore),
    Home = filename:join("/tmp", "glimpses_"
                         ++ integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = ssb_store:start_link(),
    {ok, _} = keys:start_link(),
    ok = ssb_store:declare(?MODULE, ?SCHEMA_VERSION, ?DDL),
    Home.

cleanup(Home) ->
    [catch gen_server:stop(N) || N <- [?MODULE, keys, ssb_store, config]],
    case Home of
        ignore -> ok;
        _ -> os:cmd("rm -rf " ++ Home),
             application:unset_env(ssb, ssb_home)
    end,
    ok.

-define(A, ~"@aaa.ed25519").
-define(B, ~"@bbb.ed25519").

glimpse_msg(Author, Seq, Blob) ->
    #message{author = Author, sequence = Seq, previous = ~"%p=.sha256",
             content = {[{~"type",    ~"glimpse"},
                         {~"blob",    Blob},
                         {~"size",    4242},
                         {~"updated", 1788233201436}]}}.

indexes_a_glimpse() ->
    ok = view_entry(glimpse_msg(?A, 10, ~"&one.sha256")),
    ?assertMatch({ok, #{feed := ?A, seq := 10, blob := ~"&one.sha256",
                        size := 4242, updated := 1788233201436,
                        source := feed}},
                 for_feed(?A)),
    %% and it is on offer to peers, as the author signed it
    [#{feed := ?A, raw := Raw}] = offers(),
    ?assert(is_binary(Raw)).

ignores_ordinary_messages() ->
    ok = view_entry(#message{author = ?B, sequence = 1,
                             content = {[{~"type", ~"post"},
                                         {~"text", ~"hello"}]}}),
    ?assertEqual(none, for_feed(?B)).

%% Nothing to show and nothing to fetch: indexing it would only offer a
%% peer a pointer to nowhere.
ignores_a_glimpse_with_no_blob() ->
    ok = view_entry(#message{author = ?B, sequence = 2,
                             content = {[{~"type", ~"glimpse"},
                                         {~"size", 10}]}}),
    ?assertEqual(none, for_feed(?B)).

newest_glimpse_replaces_the_old_one() ->
    ok = view_entry(glimpse_msg(?A, 11, ~"&two.sha256")),
    ?assertMatch({ok, #{seq := 11, blob := ~"&two.sha256"}}, for_feed(?A)),
    %% one row per feed, not a history of self-descriptions
    ?assertEqual(1, length(offers())).

%% Replay after a crash can redeliver an earlier message; it must not
%% roll the feed's self-description backwards.
an_older_glimpse_does_not_win() ->
    ok = view_entry(glimpse_msg(?A, 3, ~"&stale.sha256")),
    ?assertMatch({ok, #{seq := 11, blob := ~"&two.sha256"}}, for_feed(?A)).

staging_takes_the_newest_too() ->
    ok = stage(glimpse_msg(?B, 50, ~"&edge-old.sha256")),
    ok = stage(glimpse_msg(?B, 90, ~"&edge-new.sha256")),
    ok = stage(glimpse_msg(?B, 70, ~"&edge-mid.sha256")),
    ?assertMatch({ok, #{seq := 90, blob := ~"&edge-new.sha256",
                        source := edge}},
                 for_feed(?B)),
    ?assertMatch([#{feed := ?B}], edge()),
    %% staged glimpses are ours to look at, not ours to relay
    ?assertEqual([?A], [F || #{feed := F} <- offers()]).

%% Both halves can hold the same feed — a feed can be promoted, or a peer
%% can offer one we already replicate.  The chain-validated copy wins.
a_replicated_glimpse_beats_a_staged_one() ->
    ok = stage(glimpse_msg(?A, 99, ~"&hearsay.sha256")),
    ?assertMatch({ok, #{seq := 11, source := feed}}, for_feed(?A)).

%% The point of the two tables: a rebuild refolds the log, and nothing in
%% the log mentions a boundary feed.
a_rebuild_keeps_the_edge() ->
    ok = view_reset(),
    ?assertEqual([], offers()),
    ?assertMatch({ok, #{seq := 99, source := edge}}, for_feed(?A)),
    ?assertMatch({ok, #{seq := 90, source := edge}}, for_feed(?B)),
    %% and the view refills from the log as usual
    ok = view_entry(glimpse_msg(?A, 11, ~"&two.sha256")),
    ?assertMatch({ok, #{seq := 11, source := feed}}, for_feed(?A)).

forgetting_drops_only_the_edge_row() ->
    ok = forget(?A),
    ?assertMatch({ok, #{seq := 11, source := feed}}, for_feed(?A)),
    ok = forget(?B),
    ?assertEqual(none, for_feed(?B)),
    ?assertEqual([], edge()).

is_a_core_view() ->
    ?assertEqual(core, view_class()).

-endif.
