%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Core view: every archive boundary we have seen, from any feed.
%%
%% An author who archives publishes a message of type `archive` naming the
%% blob that holds the frozen segment, the sequence range it covers, its
%% size and its timestamp span.  This view is that set of statements,
%% indexed by feed.
%%
%% WHAT IT IS FOR.  Two readers, pulling in opposite directions:
%%
%%   Serving.  A peer asks which of the feeds we hold have a boundary it
%%   could start from.  Answering that by asking each feed in turn is a
%%   pread per feed; here it is one SELECT.
%%
%%   Offering.  A client asks what a feed's earlier history would cost to
%%   fetch — which is why `size` and the timestamp range are in the
%%   message at all.
%%
%% WHY THIS IS A VIEW AND feed_floor IS NOT.  Both concern archives; they
%% sit on opposite sides of the truth-vs-derivation seam
%% (doc/persistence.md).  This view is a deterministic fold over the
%% ingest journal — what AUTHORS said — so replaying the journal
%% reproduces it exactly and a rebuild is free.  A floor is a record of
%% what WE decided not to fetch, is implied by nothing in any feed, and
%% must survive a rebuild untouched.
%%
%% NEWEST BOUNDARY WINS PER FEED.  A long-lived feed archives repeatedly,
%% so it has several boundaries.  The rows are kept per {feed, sequence}
%% rather than collapsed, because "the newest" and "the most conservative"
%% are both wanted and by different callers: serving advertises what we
%% have, while a node CHOOSING a floor deliberately prefers a lower
%% boundary, retaining more history and staying a witness for longer.
-module(ssb_archives).

-behaviour(gen_server).
-behaviour(ssb_view).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/0,
         boundaries/0,
         for_feed/1,
         newest/1,
         lowest/1]).

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
        ["CREATE TABLE IF NOT EXISTS archive_boundaries("
         "  feed      TEXT NOT NULL,"
         %% sequence of the archive message itself: the first sequence a
         %% reader starting here would hold
         "  seq       INTEGER NOT NULL,"
         %% id of the message below the boundary — what a floor seeds
         %% last_msg to, and what the blob must hash to at the seam
         "  prev_id   TEXT NOT NULL,"
         "  blob      TEXT NOT NULL,"
         "  size      INTEGER,"
         "  from_seq  INTEGER,"
         "  to_seq    INTEGER,"
         "  from_ts   INTEGER,"
         "  to_ts     INTEGER,"
         %% The signed message itself, in the value-only form EBT puts on
         %% the wire.  Kept so that serving a boundary is a pure SELECT:
         %% a peer must verify the AUTHOR's signature, not take our word
         %% for the fields above, and re-reading it from the feed would
         %% put back the per-feed lookup this view exists to remove.
         %% Archive messages are rare, so the duplication is negligible.
         "  raw       BLOB NOT NULL,"
         "  PRIMARY KEY (feed, seq)) WITHOUT ROWID;"]).

-define(COLS, "feed,seq,prev_id,blob,size,from_seq,to_seq,from_ts,to_ts,raw").

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Every boundary we could offer a peer, newest first per feed.
boundaries() ->
    [to_map(R) || R <- q("SELECT " ?COLS " FROM archive_boundaries"
                         " ORDER BY feed, seq DESC", [])].

for_feed(FeedId) ->
    [to_map(R) || R <- q("SELECT " ?COLS " FROM archive_boundaries"
                         " WHERE feed = ? ORDER BY seq DESC", [FeedId])].

%% The boundary that skips the most.
newest(FeedId) ->
    one("SELECT " ?COLS " FROM archive_boundaries WHERE feed = ?"
        " ORDER BY seq DESC LIMIT 1", [FeedId]).

%% The boundary that skips the least — the conservative choice when
%% picking a floor, since it keeps us a witness to more of the feed.
lowest(FeedId) ->
    one("SELECT " ?COLS " FROM archive_boundaries WHERE feed = ?"
        " ORDER BY seq ASC LIMIT 1", [FeedId]).

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

view_reset() ->
    _ = ssb_store:clear_complete(?MODULE),
    _ = ssb_store:exec("DELETE FROM archive_boundaries;"),
    ok.

view_save() ->
    _ = ssb_store:mark_complete(?MODULE),
    ok.

%% Record an archive message as a boundary — but only one a reader could
%% actually start from.  A boundary needs a predecessor to chain onto and
%% a sequence above 1; a feed whose very first message is typed `archive`
%% satisfies neither, and indexing it would only offer peers something
%% they must then refuse (cf. ssb_feed:boundary_if_usable/1).
view_entry(#message{author = Author, sequence = Seq, previous = Prev,
                    content = {Props}} = Msg) when Seq > 1 ->
    case proplists:get_value(~"type", Props) of
        ~"archive" ->
            case message:is_null_ref(Prev) of
                true  -> ok;
                false -> put_boundary(Author, Seq, Prev, Props, Msg)
            end;
        _ ->
            ok
    end;
view_entry(_) ->
    ok.

put_boundary(Feed, Seq, Prev, Props, Msg) ->
    _ = ssb_store:write(
          "INSERT INTO archive_boundaries(" ?COLS ")"
          " VALUES(?,?,?,?,?,?,?,?,?,?)"
          " ON CONFLICT(feed,seq) DO NOTHING",
          [Feed, Seq, Prev,
           proplists:get_value(~"archive", Props),
           proplists:get_value(~"size", Props),
           proplists:get_value(~"from_sequence", Props),
           proplists:get_value(~"to_sequence", Props),
           proplists:get_value(~"from_timestamp", Props),
           proplists:get_value(~"to_timestamp", Props),
           message:encode_value(Msg)]),
    ok.

%%%===================================================================
%%% Internal
%%%===================================================================

to_map([Feed, Seq, Prev, Blob, Size, FromSeq, ToSeq, FromTs, ToTs, Raw]) ->
    #{feed => Feed, seq => Seq, prev_id => Prev, blob => Blob, size => Size,
      from_seq => FromSeq, to_seq => ToSeq, from_ts => FromTs, to_ts => ToTs,
      raw => Raw}.

one(Sql, Params) ->
    case q(Sql, Params) of
        [Row] -> {ok, to_map(Row)};
        _     -> none
    end.

%% A question about boundaries must not take down its asker: callers are
%% replication paths that have a perfectly good fallback — no boundary
%% means replicate from the beginning, which is what we do today.
q(Sql, Params) ->
    try ssb_store:q(Sql, Params)
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

%% Keep retrying until accepted; a silent skip means boundaries quietly
%% stop being indexed and we start offering peers nothing.
ensure_registered(State) ->
    case ssb_view:ensure_registered(?MODULE, [view]) of
        ok    -> ok;
        retry -> erlang:send_after(2000, self(), ensure_registered)
    end,
    {noreply, State}.
