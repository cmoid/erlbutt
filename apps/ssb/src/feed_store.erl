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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([feed_dirs/0,
         fold_feed/3,
         fold_all/2,
         last_frame/1,
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

%% The raw binary of a feed's most recent message, read cheaply from the
%% tail of the live log (last frame is <<Len:32, Msg:Len, Len:32,
%% Next:32>>, so the final 8 bytes give the message length).  Returns
%% `unknown` whenever the last message can't be read confidently without
%% a full fold — an empty or torn live log, an archives-only feed, or any
%% io error — so callers can fall back to folding the whole feed.
last_frame(Dir) ->
    Live = ?l2b(filename:join(Dir, "log.offset")),
    case file:open(Live, [read, binary, raw]) of
        {ok, Fd} ->
            R = try tail_msg(Fd) catch _:_ -> unknown end,
            file:close(Fd),
            R;
        _ ->
            unknown
    end.

tail_msg(Fd) ->
    {ok, Size} = file:position(Fd, eof),
    case Size >= 12 of
        false ->
            unknown;                       %% empty or too small to frame
        true ->
            {ok, <<Len:32/integer, _Next:32/integer>>} =
                file:pread(Fd, Size - 8, 8),
            MsgStart = Size - 8 - Len,
            case MsgStart >= 4 of
                false ->
                    unknown;               %% length overruns the file
                true ->
                    case file:pread(Fd, MsgStart, Len) of
                        {ok, Msg} when byte_size(Msg) =:= Len -> {ok, Msg};
                        _                                     -> unknown
                    end
            end
    end.

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

-ifdef(TEST).

%% On-disk frame: <<Len:32, Msg:Len, Len:32, NextOffset:32>>.
frame(Msg) ->
    Len = byte_size(Msg),
    <<Len:32, Msg/binary, Len:32, 0:32>>.

last_frame_test_() ->
    Setup = fun() ->
                    Dir = filename:join(
                            ["/tmp",
                             "feed_store_lf_" ++
                                 integer_to_list(erlang:unique_integer([positive]))]),
                    ok = filelib:ensure_dir(filename:join(Dir, "x")),
                    Dir
            end,
    Teardown = fun(Dir) ->
                       file:delete(filename:join(Dir, "log.offset")),
                       file:del_dir(Dir)
               end,
    Log = fun(Dir) -> filename:join(Dir, "log.offset") end,
    {foreach, Setup, Teardown,
     [fun(Dir) ->
              {"reads the last of several frames",
               fun() ->
                       ok = file:write_file(Log(Dir),
                                            [frame(~"one"), frame(~"two"),
                                             frame(~"three")]),
                       ?assertEqual({ok, ~"three"}, last_frame(Dir))
               end}
      end,
      fun(Dir) ->
              {"single frame",
               fun() ->
                       ok = file:write_file(Log(Dir), frame(~"only")),
                       ?assertEqual({ok, ~"only"}, last_frame(Dir))
               end}
      end,
      fun(Dir) ->
              {"empty log is unknown",
               fun() ->
                       ok = file:write_file(Log(Dir), <<>>),
                       ?assertEqual(unknown, last_frame(Dir))
               end}
      end,
      fun(Dir) ->
              {"missing log is unknown",
               fun() -> ?assertEqual(unknown, last_frame(Dir)) end}
      end,
      fun(Dir) ->
              {"torn tail is unknown, not a crash",
               fun() ->
                       ok = file:write_file(Log(Dir),
                                            [frame(~"good"), <<7:32, "trunc">>]),
                       ?assertEqual(unknown, last_frame(Dir))
               end}
      end]}.

-endif.
