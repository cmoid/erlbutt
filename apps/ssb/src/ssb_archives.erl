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
         lowest/1,
         boundary_at/2]).

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

%% How often a pinning node re-checks that it still holds the blob behind
%% every boundary it knows.  want_refs/1 already wants one when the
%% boundary message is stored, so this sweep is for the cases that path
%% misses: boundaries learned before pinning was switched on, and wants
%% that aged out before any peer could answer them.
-define(PIN_FIRST_MS, 120_000).
-define(PIN_SWEEP_MS, 3_600_000).
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

%% The boundary we can offer a peer for each feed: the NEWEST, and only
%% the newest.
%%
%% Not a preference — the others are unusable.  A node serves peers from
%% its live log alone (ssb_feed:foldl/3, which both EBT and
%% createHistoryStream fold), and the live log begins at the newest
%% boundary.  Everything below that is in frozen segments nobody serves.
%%
%% Offering an older boundary therefore invites a peer to start somewhere
%% we cannot feed it: it floors there, asks for the next sequence, and we
%% send the only thing we have — a message far above it, whose previous
%% does not match, so its chain check refuses every one.  It sits stuck at
%% a floor with no way forward and nothing in the logs but chain breaks.
%%
%% A receiver still takes the LOWEST of what several peers offer.  That
%% remains the conservative choice; it just chooses between peers now,
%% which is the disagreement it was meant for.
boundaries() ->
    [to_map(R) || R <- q("SELECT " ?COLS " FROM archive_boundaries b"
                         " WHERE b.seq = (SELECT MAX(b2.seq)"
                         "                  FROM archive_boundaries b2"
                         "                 WHERE b2.feed = b.feed)"
                         " ORDER BY b.feed", [])].

for_feed(FeedId) ->
    [to_map(R) || R <- q("SELECT " ?COLS " FROM archive_boundaries"
                         " WHERE feed = ? ORDER BY seq DESC", [FeedId])].

%% The boundary that skips the most.
newest(FeedId) ->
    one("SELECT " ?COLS " FROM archive_boundaries WHERE feed = ?"
        " ORDER BY seq DESC LIMIT 1", [FeedId]).

%% The boundary sitting exactly at Seq, if we know one.
%%
%% Used after importing a segment: archives CHAIN, each one freezing only
%% the live log of its day, so recovering one layer exposes the boundary
%% below it — the first message of the restored segment is the previous
%% archive genesis.  Finding it is what lets a reader keep walking back.
boundary_at(FeedId, Seq) ->
    one("SELECT " ?COLS " FROM archive_boundaries"
        " WHERE feed = ? AND seq = ?", [FeedId, Seq]).

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
    _ = erlang:send_after(?PIN_FIRST_MS, self(), pin_sweep),
    {ok, #{}, {continue, register_view}}.

handle_continue(register_view, State) ->
    ensure_registered(State).

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State)            -> {noreply, State}.

handle_info(pin_sweep, State) ->
    _ = spawn(fun pin_sweep/0),
    _ = erlang:send_after(?PIN_SWEEP_MS, self(), pin_sweep),
    {noreply, State};
handle_info(ensure_registered, State) -> ensure_registered(State);
handle_info(_Info, State)             -> {noreply, State}.

terminate(_Reason, _State)       -> ok.
code_change(_Old, State, _Extra) -> {ok, State}.

%% Want the blob behind every boundary we know and do not already hold.
%%
%% Only on a node that has opted in.  Archiving moves history out of the
%% feed, which every peer replicates, and into a blob, which nobody is
%% obliged to keep — and boundaries propagate faster than the blobs behind
%% them, because a node that adopted one re-advertises it.  Somebody has
%% to be the one that keeps them, and this is how that node says so.
pin_sweep() ->
    case config:pin_archives() of
        false -> ok;
        true  -> lists:foreach(fun pin/1, boundaries())
    end.

pin(#{blob := Blob}) when is_binary(Blob) ->
    case blobs:has(Blob) of
        true  -> ok;
        false -> blob_fetcher:want(Blob)
    end;
pin(_) ->
    ok.

%% Keep retrying until accepted; a silent skip means boundaries quietly
%% stop being indexed and we start offering peers nothing.
ensure_registered(State) ->
    case ssb_view:ensure_registered(?MODULE, [view]) of
        ok    -> ok;
        retry -> erlang:send_after(2000, self(), ensure_registered)
    end,
    {noreply, State}.

-ifdef(TEST).

archives_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
             [?_test(indexes_an_archive_boundary()),
              ?_test(ignores_ordinary_messages()),
              ?_test(ignores_unusable_boundaries()),
              ?_test(keeps_every_boundary_of_a_feed()),
              ?_test(newest_skips_most_lowest_skips_least()),
              ?_test(stored_value_still_verifies()),
              ?_test(is_a_core_view())]
     end}.

setup() ->
    cleanup(ignore),
    Home = filename:join("/tmp", "archives_"
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

archive_msg(Author, Seq, Prev, FromSeq, ToSeq) ->
    #message{author = Author, sequence = Seq, previous = Prev,
             content = {[{~"type",           ~"archive"},
                         {~"archive",        ~"&seg.sha256"},
                         {~"from_sequence",  FromSeq},
                         {~"to_sequence",    ToSeq},
                         {~"size",           4242},
                         {~"from_timestamp", 1000},
                         {~"to_timestamp",   2000}]}}.

%% Everything a peer or a client needs is carried across, including the
%% predecessor a floor seeds from and the size a UI quotes as the price.
indexes_an_archive_boundary() ->
    Id = ~"@arc1.ed25519",
    ok = view_entry(archive_msg(Id, 101, ~"%prev=.sha256", 1, 100)),
    {ok, B} = newest(Id),
    ?assertEqual(101,               maps:get(seq, B)),
    ?assertEqual(~"%prev=.sha256",  maps:get(prev_id, B)),
    ?assertEqual(~"&seg.sha256",    maps:get(blob, B)),
    ?assertEqual(4242,              maps:get(size, B)),
    ?assertEqual(1,                 maps:get(from_seq, B)),
    ?assertEqual(100,               maps:get(to_seq, B)),
    ?assertEqual(1000,              maps:get(from_ts, B)),
    ?assertEqual(2000,              maps:get(to_ts, B)).

ignores_ordinary_messages() ->
    Id = ~"@arc2.ed25519",
    ok = view_entry(#message{author = Id, sequence = 2, previous = ~"%p=.sha256",
                             content = {[{~"type", ~"post"},
                                         {~"text", ~"hello"}]}}),
    ?assertEqual(none, newest(Id)),
    %% content that is not even a property list must not crash the fold
    ok = view_entry(#message{author = Id, sequence = 3, previous = ~"%p=.sha256",
                             content = ~"an encrypted string"}),
    ?assertEqual(none, newest(Id)).

%% A boundary a reader could not start from is not indexed at all, so it is
%% never offered to a peer who would only have to refuse it.
ignores_unusable_boundaries() ->
    Nulled = ~"@arc3.ed25519",
    ok = view_entry(archive_msg(Nulled, 101, null, 1, 100)),
    ?assertEqual(none, newest(Nulled)),

    First = ~"@arc4.ed25519",
    ok = view_entry(archive_msg(First, 1, null, 1, 1)),
    ?assertEqual(none, newest(First)).

%% A long-lived feed archives repeatedly; every boundary is kept, because
%% serving wants the newest and choosing a floor wants the lowest.
keeps_every_boundary_of_a_feed() ->
    Id = ~"@arc5.ed25519",
    ok = view_entry(archive_msg(Id, 101,  ~"%a=.sha256", 1,   100)),
    ok = view_entry(archive_msg(Id, 201,  ~"%b=.sha256", 101, 200)),
    ok = view_entry(archive_msg(Id, 301,  ~"%c=.sha256", 201, 300)),
    ?assertEqual(3, length(for_feed(Id))),
    %% re-folding the same message is idempotent
    ok = view_entry(archive_msg(Id, 201, ~"%b=.sha256", 101, 200)),
    ?assertEqual(3, length(for_feed(Id))).

%% The conservative choice retains the most history: a node picking a floor
%% takes `lowest` and stays a witness to everything above it.
newest_skips_most_lowest_skips_least() ->
    Id = ~"@arc6.ed25519",
    ok = view_entry(archive_msg(Id, 501, ~"%x=.sha256", 401, 500)),
    ok = view_entry(archive_msg(Id, 901, ~"%y=.sha256", 501, 900)),
    {ok, Newest} = newest(Id),
    {ok, Lowest} = lowest(Id),
    ?assertEqual(901, maps:get(seq, Newest)),
    ?assertEqual(501, maps:get(seq, Lowest)),
    ?assert(lists:any(fun(#{feed := F}) -> F =:= Id end, boundaries())).

%% What we serve a peer is the AUTHOR's signed message, so the stored copy
%% must survive the round trip through the view and still verify.  If it
%% did not, a peer would reject every boundary we offered — and it would
%% look like a replication fault rather than an encoding one.
stored_value_still_verifies() ->
    FeedId = keys:pub_key_disp(),
    Signed = message:new_msg(~"%prev=.sha256", 77,
                             {[{~"type",           ~"archive"},
                               {~"archive",        ~"&seg.sha256"},
                               {~"from_sequence",  1},
                               {~"to_sequence",    76},
                               {~"size",           99},
                               {~"from_timestamp", 1},
                               {~"to_timestamp",   2}]},
                             {FeedId, keys:priv_key()}),
    ok = view_entry(Signed),
    {ok, #{raw := Raw}} = newest(FeedId),
    Decoded = message:decode_value(Raw, true),
    ?assertEqual(true,   Decoded#message.validated),
    ?assertEqual(FeedId, Decoded#message.author),
    ?assertEqual(77,     Decoded#message.sequence),
    ?assertEqual(Signed#message.id, Decoded#message.id).

is_a_core_view() ->
    ?assertEqual(core, view_class()),
    ?assertEqual(core, ssb_view:class(?MODULE)).

-endif.
