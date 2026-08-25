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
