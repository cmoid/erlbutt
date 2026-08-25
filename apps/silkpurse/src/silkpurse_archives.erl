%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% What a client needs to show, and act on, a feed whose earlier history
%% this node does not hold.
%%
%% `archives.history` answers "is there history below what I can see, and
%% what would it cost" — which is why the archive message carries a size
%% and a timestamp range at all.  Without those a client could only name
%% the price by paying it.
%%
%% `archives.fetch` is the user saying yes.  It is deliberately TWO STEPS
%% when the blob is not already here: blob transfer is want-driven and
%% asynchronous, so a call that blocked until a multi-megabyte segment
%% arrived would sit on the connection's rpc_processor for as long as it
%% took.  The first call records the want and reports `fetching`; a later
%% call, once the blob has landed, does the import and reports `imported`.
%% The client polls `history` in between, where `held` flips to true.
-module(silkpurse_archives).

-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([manifest/0, handle_rpc/3]).

%% exported for tests
-export([history/1, fetch/1]).

manifest() ->
    [{[~"archives", ~"history"], async, owner},
     {[~"archives", ~"fetch"],   async, owner}].

handle_rpc([~"archives", ~"history"], Args, _Caller) ->
    case feed_arg(Args) of
        {ok, FeedId} -> {reply, history(FeedId)};
        error        -> {error, ~"archives.history needs a feed id"}
    end;

handle_rpc([~"archives", ~"fetch"], Args, _Caller) ->
    case feed_arg(Args) of
        {ok, FeedId} -> {reply, fetch(FeedId)};
        error        -> {error, ~"archives.fetch needs a feed id"}
    end.

%% Accepts either a bare id or {feedId: ...}, since patchwork-era clients
%% pass options objects and newer callers pass the id.
feed_arg([FeedId]) when is_binary(FeedId) ->
    {ok, FeedId};
feed_arg([{Opts}]) when is_list(Opts) ->
    case ?pgv(~"feedId", Opts) of
        FeedId when is_binary(FeedId) -> {ok, FeedId};
        _                             -> error
    end;
feed_arg(_) ->
    error.

%%%===================================================================
%%% history
%%%===================================================================

history(FeedId) ->
    {Floor, FloorState} =
        case feed_floor:get(FeedId) of
            {ok, #{floor_seq := Seq, state := St}} -> {Seq, St};
            none                                   -> {1, ~"none"}
        end,
    {[{~"feed",     FeedId},
      %% The lowest sequence this node holds.  1 means nothing was skipped.
      {~"floor",    Floor},
      {~"state",    FloorState},
      {~"archives", [descriptor(B) || B <- ssb_archives:for_feed(FeedId)]}]}.

descriptor(#{seq := Seq, blob := Blob, size := Size, from_seq := FromSeq,
             to_seq := ToSeq, from_ts := FromTs, to_ts := ToTs}) ->
    {[{~"blob",          Blob},
      {~"boundary",      Seq},
      {~"size",          Size},
      {~"fromSequence",  FromSeq},
      {~"toSequence",    ToSeq},
      {~"fromTimestamp", FromTs},
      {~"toTimestamp",   ToTs},
      %% Whether fetching is a download or already local — the difference
      %% between offering a wait and offering an instant.
      {~"held",          Blob =/= undefined andalso blobs:has(Blob)}]}.

%%%===================================================================
%%% fetch
%%%===================================================================

fetch(FeedId) ->
    case feed_floor:get(FeedId) of
        none ->
            %% Nothing was skipped, so there is nothing below to recover.
            result(~"nothing_to_fetch", FeedId, undefined);
        {ok, Floor} ->
            fetch_floor(FeedId, Floor)
    end.

fetch_floor(FeedId, #{blob := Blob} = Floor) ->
    case blobs:has(Blob) of
        false ->
            %% Record the want and let the ordinary blob machinery find
            %% it; the client asks again when `held` turns true.
            blob_fetcher:want(Blob),
            result(~"fetching", FeedId, Blob);
        true ->
            import(FeedId, Floor)
    end.

import(FeedId, #{blob := Blob, prev_id := PrevId, floor_seq := FloorSeq}) ->
    case blobs:fetch(Blob) of
        {ok, Gz} -> verify_and_import(FeedId, Blob, PrevId, FloorSeq, Gz);
        _        -> result(~"blob_unreadable", FeedId, Blob)
    end.

verify_and_import(FeedId, Blob, PrevId, FloorSeq, Gz) ->
    try zlib:gunzip(Gz) of
        Raw ->
            case archive_verify:check(Raw, FeedId, PrevId, FloorSeq) of
                {ok, FromSeq, ToSeq} ->
                    ok = archive_verify:install(FeedId, Gz, Raw, FromSeq, ToSeq),
                    ok = feed_floor:clear(FeedId),
                    %% The recovered messages sit below every view's
                    %% checkpoint, so ingest cannot see them; a targeted
                    %% refold of this one feed is what makes them appear.
                    _ = view_manager:refold_feed(FeedId),
                    ?SSB_INFO("archives.fetch: imported ~p..~p for ~s",
                              [FromSeq, ToSeq, FeedId]),
                    result(~"imported", FeedId, Blob);
                {error, Reason} ->
                    %% Not a network failure.  The blob does not join the
                    %% chain it claims to, which is evidence the history
                    %% was altered, and must not look like a retry.
                    ?SSB_ERROR("archives.fetch: ~s failed verification: ~p",
                               [FeedId, Reason]),
                    result(~"failed", FeedId, Blob, Reason)
            end
    catch _:_ ->
        result(~"failed", FeedId, Blob, ~"not a gzip archive")
    end.

result(Status, FeedId, Blob) ->
    result(Status, FeedId, Blob, undefined).

result(Status, FeedId, Blob, Reason) ->
    {[{~"feed",   FeedId},
      {~"status", Status},
      {~"blob",   Blob},
      {~"reason", detail(Reason)},
      {~"floor",  case feed_floor:get(FeedId) of
                      {ok, #{floor_seq := S}} -> S;
                      none                    -> 1
                  end}]}.

detail(undefined)            -> null;
detail(R) when is_binary(R)  -> R;
detail(R)                    -> ?l2b(io_lib:format("~p", [R])).

-ifdef(TEST).

archives_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun history_of_an_unfloored_feed/1,
      fun history_prices_the_archive/1,
      fun fetch_restores_skipped_history/1,
      fun fetch_without_a_floor_is_a_noop/1]}.

setup() ->
    cleanup(ignore),
    Home = filename:join("/tmp", "sparch_"
                         ++ integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = ssb_store:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = feed_floor:start_link(),
    ok = ssb_store:declare(ssb_archives, 1, ssb_archives_ddl()),
    FeedId = keys:pub_key_disp(),
    {ok, Pid} = ssb_feed:start_link(FeedId),
    {Pid, FeedId, Home}.

ssb_archives_ddl() ->
    ["CREATE TABLE IF NOT EXISTS archive_boundaries("
     "  feed TEXT NOT NULL, seq INTEGER NOT NULL, prev_id TEXT NOT NULL,"
     "  blob TEXT NOT NULL, size INTEGER, from_seq INTEGER, to_seq INTEGER,"
     "  from_ts INTEGER, to_ts INTEGER, raw BLOB NOT NULL,"
     "  PRIMARY KEY (feed, seq)) WITHOUT ROWID;"].

cleanup(ignore) ->
    [catch gen_server:stop(N)
     || N <- [feed_floor, blobs, mess_auth, ssb_store, keys, config]],
    ok;
cleanup({Pid, _, Home}) ->
    catch gen_server:stop(Pid),
    cleanup(ignore),
    os:cmd("rm -rf " ++ Home),
    application:unset_env(ssb, ssb_home),
    ok.

verified(#message{} = M) -> message:decode(message:encode(M), true).

%% A feed nothing was skipped from reports floor 1, so a client knows there
%% is nothing to offer.
history_of_an_unfloored_feed({Pid, FeedId, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"hello"),
        {Props} = history(FeedId),
        ?assertEqual(1, ?pgv(~"floor", Props)),
        ?assertEqual(~"none", ?pgv(~"state", Props))
    end.

%% The whole reason size and the timestamp range are in the archive
%% message: a client can quote the cost without fetching anything.
history_prices_the_archive({Pid, FeedId, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"one"),
        ok = ssb_feed:post_content(Pid, ~"two"),
        {ok, _Blob} = ssb_feed:archive(Pid),
        Genesis = verified(ssb_feed:fetch_last_msg(Pid)),
        ok = ssb_archives:view_entry(Genesis),
        ok = feed_floor:set(FeedId, Genesis),

        {Props} = history(FeedId),
        ?assertEqual(3, ?pgv(~"floor", Props)),
        [{Arc}] = ?pgv(~"archives", Props),
        ?assert(is_integer(?pgv(~"size", Arc))),
        ?assertEqual(1, ?pgv(~"fromSequence", Arc)),
        ?assertEqual(2, ?pgv(~"toSequence", Arc)),
        %% we still hold this blob, so fetching it is instant
        ?assertEqual(true, ?pgv(~"held", Arc))
    end.

%% The paid-off floor: the segment is gone from disk, the node holds only
%% the boundary onward, and fetching puts the history back where
%% feed_store can find it again.
fetch_restores_skipped_history({Pid, FeedId, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"early one"),
        #message{id = EarlyId} = ssb_feed:fetch_last_msg(Pid),
        ok = ssb_feed:post_content(Pid, ~"early two"),
        {ok, _} = ssb_feed:archive(Pid),
        Genesis = verified(ssb_feed:fetch_last_msg(Pid)),
        ok = feed_floor:set(FeedId, Genesis),

        %% Stand in for a node that adopted the floor and never held the
        %% segment: take it off disk, leaving only the blob and the
        %% boundary that names it.
        Dir = ?b2l(utils:feed_dir(FeedId)),
        [Seg] = filelib:wildcard(filename:join(Dir, "log.offset.*.gz")),
        ok = file:delete(Seg),
        ?assertEqual(not_found, ssb_feed:fetch_msg(Pid, EarlyId)),

        {Res} = fetch(FeedId),
        ?assertEqual(~"imported", ?pgv(~"status", Res)),
        ?assertEqual(1, ?pgv(~"floor", Res)),

        %% the skipped history is readable again, and the floor is gone
        ?assertMatch(#message{content = ~"early one"},
                     ssb_feed:fetch_msg(Pid, EarlyId)),
        ?assertEqual(none, feed_floor:get(FeedId))
    end.

fetch_without_a_floor_is_a_noop({Pid, FeedId, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"nothing skipped"),
        {Res} = fetch(FeedId),
        ?assertEqual(~"nothing_to_fetch", ?pgv(~"status", Res))
    end.

-endif.
