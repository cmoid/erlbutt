%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Read-side helpers over the on-disk per-feed store.  A feed directory
%% holds archived segments (log.offset.<From>-<To>.gz — the gzipped raw
%% log file, oldest ranges first) and the live log.offset; together they
%% are the feed's full history in sequence order.  Everything here is a
%% plain function — no process, no state beyond the cursors callers hold.
%%
%% Record framing (shared with utils:fold_log_file):
%%   <<Len:32, Msg:Len/binary, Len:32, NextOffset:32>>
-module(feed_store).

-include_lib("ssb/include/ssb.hrl").
-compile({no_auto_import, [size/1]}).

-export([feed_dirs/0,
         fold_feed/3,
         fold_all/2,
         cursor_open/1,
         cursor_next/1,
         cursor_close/1]).

%%%===================================================================
%%% Folds
%%%===================================================================

%% Every feed directory under the configured feed store.
feed_dirs() ->
    Loc = ?b2l(config:feed_loc()),
    [D || D <- filelib:wildcard(filename:join(Loc, "*/*")),
          filelib:is_dir(D)].

%% Fold Fun(MsgBinary, Acc) over one feed's full history in sequence
%% order: archived segments oldest first, then the live log.
fold_feed(Fun, Acc0, Dir) ->
    Acc1 = lists:foldl(fun(Gz, Acc) -> fold_archive(Fun, Acc, Gz) end,
                       Acc0, archive_segments(Dir)),
    utils:fold_log_file(Fun, Acc1, ?l2b(filename:join(Dir, "log.offset"))).

%% Fold Fun(MsgBinary, Acc) over every feed's full history, feed by feed
%% (per-feed sequence order; no cross-feed ordering guarantee).
fold_all(Fun, Acc0) ->
    lists:foldl(fun(Dir, Acc) -> fold_feed(Fun, Acc, Dir) end,
                Acc0, feed_dirs()).

%%%===================================================================
%%% Sequential cursor (archives, then live log)
%%%===================================================================

%% A pull-based reader over one feed's history, for callers that
%% interleave several feeds (e.g. the ingest journal's arrival-order
%% stream).  cursor_next/1 returns {MsgBinary, Cursor} | eof.
cursor_open(Dir) ->
    {segments, archive_segments(Dir), ?l2b(filename:join(Dir, "log.offset"))}.

cursor_next({segments, [Gz | Rest], Live}) ->
    case read_archive(Gz) of
        {ok, Bin} -> cursor_next({bin, Bin, {segments, Rest, Live}});
        error     -> cursor_next({segments, Rest, Live})
    end;
cursor_next({segments, [], Live}) ->
    case file:open(Live, [read, binary, read_ahead]) of
        {ok, IoDev} -> cursor_next({live, IoDev});
        {error, _}  -> eof
    end;
cursor_next({bin, <<Len:32, Msg:Len/binary, Len:32, _Next:32, Rest/binary>>, K}) ->
    {Msg, {bin, Rest, K}};
cursor_next({bin, _Rest, K}) ->
    cursor_next(K);
cursor_next({live, IoDev}) ->
    case read_record(IoDev) of
        {ok, Msg} -> {Msg, {live, IoDev}};
        eof       -> file:close(IoDev), eof
    end;
cursor_next(eof) ->
    eof.

cursor_close({live, IoDev})       -> file:close(IoDev), ok;
cursor_close({bin, _, K})         -> cursor_close(K);
cursor_close({segments, _, _})    -> ok;
cursor_close(eof)                 -> ok.

%%%===================================================================
%%% Internal
%%%===================================================================

%% Archived segment files, oldest first.  Names are
%% log.offset.<From>-<To>.gz; sort numerically on From (lexicographic
%% ordering breaks once sequence numbers gain a digit).
archive_segments(Dir) ->
    Segs = filelib:wildcard(filename:join(Dir, "log.offset.*.gz")),
    [S || {_From, S} <- lists:sort([{archive_from(S), S} || S <- Segs])].

archive_from(Path) ->
    ["gz", Range | _] =
        lists:reverse(string:split(filename:basename(Path), ".", all)),
    [From | _] = string:split(Range, "-"),
    list_to_integer(From).

read_archive(GzFile) ->
    try
        {ok, GzData} = file:read_file(GzFile),
        {ok, zlib:gunzip(GzData)}
    catch C:R ->
            ?SSB_ERROR("feed_store: unreadable archive ~s: ~p:~p",
                       [GzFile, C, R]),
            error
    end.

fold_archive(Fun, Acc, GzFile) ->
    case read_archive(GzFile) of
        {ok, Bin} -> fold_bin(Fun, Acc, Bin);
        error     -> Acc
    end.

fold_bin(Fun, Acc, <<Len:32, Msg:Len/binary, Len:32, _Next:32, Rest/binary>>) ->
    fold_bin(Fun, Fun(Msg, Acc), Rest);
fold_bin(_Fun, Acc, _Rest) ->
    Acc.

read_record(IoDev) ->
    case file:read(IoDev, 4) of
        {ok, <<Len:32>>} ->
            case file:read(IoDev, Len + 8) of
                {ok, <<Msg:Len/binary, _Trailer:8/binary>>} -> {ok, Msg};
                _ -> eof
            end;
        _ -> eof
    end.
