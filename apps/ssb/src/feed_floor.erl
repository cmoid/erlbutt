%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Per-feed validation floors: the sequence from which we accept a feed's
%% chain, and what we chose not to hold below it.
%%
%% A feed we replicate from the beginning has floor 1 and is not recorded
%% here at all — absence means "no floor", which is the ordinary case.  A
%% feed onboarded from an archive boundary has floor K > 1: we hold K
%% onward, we have never seen 1..K-1, and the author's archive blob is
%% where that history lives if we ever want it.
%%
%% WHY THIS IS NOT A VIEW.  Views are deterministic folds over the ingest
%% journal — replay the journal, get the same view.  A floor is the
%% opposite: it is a record of a LOCAL DECISION about what we declined to
%% fetch, and nothing in any feed's contents implies it.  Rebuilding views
%% from scratch must not disturb it (doc/persistence.md's truth-vs-
%% derivation seam: this is local truth, not derivation).
%%
%% WHAT A FLOOR COSTS.  Adopting one means accepting the author's claim
%% about their own past without having witnessed it.  A malicious author
%% could fabricate 1..K-1 at archive time and sign a genesis over it, and
%% fetching the blob would verify perfectly, because they fabricated
%% consistently.  What catches that is WITNESSES — peers who replicated
%% all along hold the real history and would see the mismatch.  So a floor
%% is safest on a well-witnessed feed and weakest on an obscure one, which
%% is where a trust metric would eventually decide rather than an operator.
-module(feed_floor).

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/0,
         set/2,
         get/1,
         all/0,
         clear/1,
         describe/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SCHEMA_VERSION, 1).
-define(DDL,
        ["CREATE TABLE IF NOT EXISTS feed_floors("
         "  feed      TEXT PRIMARY KEY,"
         %% first sequence we accept, i.e. the archive genesis's own seq
         "  floor_seq INTEGER NOT NULL,"
         %% id of the message immediately below the floor.  This is what
         %% lets ordinary chain validation work unchanged: seeding a feed
         %% with {floor_seq - 1, prev_id} makes the genesis an in-chain
         %% successor rather than a special case.
         "  prev_id   TEXT NOT NULL,"
         "  blob      TEXT NOT NULL,"
         "  size      INTEGER,"
         "  from_seq  INTEGER,"
         "  to_seq    INTEGER,"
         "  from_ts   INTEGER,"
         "  to_ts     INTEGER,"
         %% floored — skipped, never fetched
         %% verified — blob fetched and joined at the seam
         %% failed   — blob fetched and did NOT join: evidence of alteration
         "  state     TEXT NOT NULL) WITHOUT ROWID;"]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Record a floor for FeedId, derived from that feed's archive genesis.
%%
%% The genesis is self-authenticating — signed by the feed's own author —
%% so it does not matter which peer handed it over.  What is checked here
%% is that it is genuinely an archive boundary for THIS feed, and that it
%% carries the predecessor id the seed depends on.
set(FeedId, #message{author = Author}) when Author =/= FeedId ->
    {error, {wrong_author, Author}};
set(_FeedId, #message{validated = V}) when V =/= true ->
    %% Never take a floor on an unverified signature: the whole point is
    %% that the boundary is the author's own statement.
    {error, unverified};
set(FeedId, #message{sequence = Seq, previous = Prev, content = Content}) ->
    case archive_fields(Content) of
        not_archive ->
            {error, not_an_archive};
        _ when Seq =< 1 ->
            %% A floor at seq 1 skips nothing, and a feed whose very first
            %% message is typed "archive" has no predecessor to seed from.
            {error, {bad_sequence, Seq}};
        Fields ->
            %% The null test goes through message's predicate rather than
            %% repeating its list of spellings — `null`, `nil`, `undefined`
            %% are all "no reference", and a second copy would drift.
            case message:is_null_ref(Prev) of
                true ->
                    %% A boundary with no predecessor cannot be seeded from
                    %% and cannot be joined to its blob.  Pre-Aug-2026
                    %% archives look like this; they are unusable as floors
                    %% by construction.
                    {error, no_previous};
                false ->
                    insert(FeedId, Seq, Prev, Fields)
            end
    end.

%% The write result is CHECKED, not discarded.  A floor that silently fails
%% to persist is worse than one that errors: the caller seeds its in-memory
%% state, believes it succeeded, and then forgets the floor on restart —
%% advertising seq 0 and pulling the entire history the floor existed to skip.
insert(FeedId, Seq, Prev,
       #{blob := Blob, size := Size, from_seq := FromSeq,
         to_seq := ToSeq, from_ts := FromTs, to_ts := ToTs}) ->
    case ssb_store:write(
           "INSERT INTO feed_floors"
           "(feed,floor_seq,prev_id,blob,size,from_seq,to_seq,"
           " from_ts,to_ts,state) VALUES(?,?,?,?,?,?,?,?,?,?)"
           " ON CONFLICT(feed) DO UPDATE SET"
           "  floor_seq=excluded.floor_seq, prev_id=excluded.prev_id,"
           "  blob=excluded.blob, size=excluded.size,"
           "  from_seq=excluded.from_seq, to_seq=excluded.to_seq,"
           "  from_ts=excluded.from_ts, to_ts=excluded.to_ts",
           [FeedId, Seq, Prev, Blob, Size, FromSeq, ToSeq,
            FromTs, ToTs, <<"floored">>]) of
        ok             -> ok;
        {error, _} = E -> E;
        Other          -> {error, Other}
    end.

%% The floor for FeedId, or `none` for the ordinary full-history case.
get(FeedId) ->
    case q("SELECT feed,floor_seq,prev_id,blob,size,from_seq,to_seq,"
           "from_ts,to_ts,state FROM feed_floors WHERE feed = ?", [FeedId]) of
        [Row] -> {ok, to_map(Row)};
        _     -> none
    end.

all() ->
    [to_map(R) || R <- q("SELECT feed,floor_seq,prev_id,blob,size,from_seq,"
                         "to_seq,from_ts,to_ts,state FROM feed_floors", [])].

clear(FeedId) ->
    _ = ssb_store:write("DELETE FROM feed_floors WHERE feed = ?", [FeedId]),
    ok.

%% What a client needs to offer "fetch the earlier history" with a price
%% attached — the reason size and the timestamp range are in the archive
%% message at all.
describe(FeedId) ->
    case ?MODULE:get(FeedId) of
        none      -> none;
        {ok, Map} -> {ok, maps:with([floor_seq, blob, size, from_seq, to_seq,
                                     from_ts, to_ts, state], Map)}
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

%% Pull the archive descriptor out of a message's content, or `not_archive`
%% if this is not one.  Tolerates a missing size/timestamp range so that an
%% archive written before those fields existed still yields a usable floor —
%% the client just cannot name the price.
archive_fields({Props}) ->
    case proplists:get_value(~"type", Props) of
        ~"archive" ->
            #{blob     => proplists:get_value(~"archive", Props),
              size     => proplists:get_value(~"size", Props),
              from_seq => proplists:get_value(~"from_sequence", Props),
              to_seq   => proplists:get_value(~"to_sequence", Props),
              from_ts  => proplists:get_value(~"from_timestamp", Props),
              to_ts    => proplists:get_value(~"to_timestamp", Props)};
        _ ->
            not_archive
    end;
archive_fields(_) ->
    not_archive.

to_map([Feed, FloorSeq, PrevId, Blob, Size, FromSeq, ToSeq, FromTs, ToTs, St]) ->
    #{feed => Feed, floor_seq => FloorSeq, prev_id => PrevId, blob => Blob,
      size => Size, from_seq => FromSeq, to_seq => ToSeq,
      from_ts => FromTs, to_ts => ToTs, state => St}.

%% Reads must not take down a caller that is only asking a question — a
%% feed process consults this during init, before ssb_store is guaranteed
%% up in every embedding (isolated eunit setups have none).  No floor is
%% always a safe answer: it means "replicate from the beginning".
q(Sql, Params) ->
    try ssb_store:q(Sql, Params)
    catch _:_ -> []
    end.

%%%===================================================================
%%% gen_server
%%%===================================================================

init([]) ->
    ok = ssb_store:declare(?MODULE, ?SCHEMA_VERSION, ?DDL),
    {ok, #{}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(_Info, State)           -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_Old, State, _Extra)    -> {ok, State}.
