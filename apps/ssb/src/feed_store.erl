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
         cursor_close/1,
         hint_file/1,
         write_hint/2,
         read_hint/1,
         find_in_archives/2,
         archive_segments/1,
         lowest_archived_seq/1]).

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
    cursor_next({live, Live, 0});
cursor_next({bin, <<Len:32, Msg:Len/binary, Len:32, _Next:32, Rest/binary>>, K}) ->
    {Msg, {bin, Rest, K}};
cursor_next({bin, _Rest, K}) ->
    cursor_next(K);
%% Read the live log positionally (open/pread/close per record) rather than
%% holding an open file handle.  A fold that interleaves many feeds (the ingest
%% journal behind createLogStream) would otherwise accumulate one open fd per
%% feed and exhaust the process's descriptor limit — emfile then crashes
%% heartbeat and cascades to a node shutdown.  This keeps concurrent fds ~1.
cursor_next({live, Path, Offset}) ->
    case pread_record(Path, Offset) of
        {ok, Msg, Next} -> {Msg, {live, Path, Next}};
        eof             -> eof
    end;
cursor_next(eof) ->
    eof.

cursor_close({live, _Path, _Offset}) -> ok;   % nothing held open
cursor_close({bin, _, K})            -> cursor_close(K);
cursor_close({segments, _, _})       -> ok;
cursor_close(eof)                    -> ok.

%%%===================================================================
%%% Hint files
%%%===================================================================
%%
%% A hint file sits beside each archived segment and says what is in it
%% without anyone having to decompress it:
%%
%%   log.offset.<From>-<To>.gz    the frozen frames, gzipped
%%   log.offset.<From>-<To>.hint  [{MsgId, Seq, Offset, Len}]
%%
%% Offset is the position of the record's leading Len field within the
%% *uncompressed* frame stream, so a hit extracts with one gunzip and a
%% binary:part — no fold.  Borrowed from bitcask, where hint files let
%% the keydir be rebuilt at startup without reading values
%% (doc/persistence.md §6).
%%
%% Written when do_archive/1 freezes a segment, and built on demand for
%% segments archived before this existed, so an upgrading node heals
%% itself one segment at a time rather than needing a migration.

%% The hint path for a segment: log.offset.1-2.gz -> log.offset.1-2.hint.
%% Segment globs match "*.gz" only, so hints are never mistaken for data.
hint_file(GzPath) ->
    filename:rootname(GzPath, ".gz") ++ ".hint".

%% Index Data (a segment's uncompressed frames) and write it beside
%% GzPath.  Temp-then-rename so a crash mid-write cannot leave a
%% truncated hint that would later parse as a short segment.
write_hint(GzPath, Data) ->
    File = hint_file(GzPath),
    Tmp  = File ++ ".tmp",
    Bin  = term_to_binary({hint, 1, index_of(Data)}, [compressed]),
    case file:write_file(Tmp, Bin) of
        ok ->
            case file:rename(Tmp, File) of
                ok -> ok;
                {error, R} ->
                    ?SSB_ERROR("feed_store: hint rename failed ~s: ~p",
                               [File, R]),
                    _ = file:delete(Tmp),
                    error
            end;
        {error, R} ->
            ?SSB_ERROR("feed_store: hint write failed ~s: ~p", [Tmp, R]),
            error
    end.

%% The index for a segment: read the hint if there is a usable one,
%% otherwise build it from the segment and write it for next time.
%% Returns {ok, [{MsgId, Seq, Offset, Len}]} or error.
read_hint(GzPath) ->
    case load_hint(hint_file(GzPath)) of
        {ok, Index} ->
            {ok, Index};
        error ->
            case read_archive(GzPath) of
                {ok, Data} ->
                    _ = write_hint(GzPath, Data),   %% best effort
                    {ok, index_of(Data)};
                error ->
                    error
            end
    end.

%% A corrupt, truncated or old-format hint is not an error — it just
%% means "no hint", and read_hint/1 rebuilds it.
load_hint(File) ->
    try
        {ok, Bin} = file:read_file(File),
        {hint, 1, Index} = binary_to_term(Bin),
        true = is_list(Index),
        {ok, Index}
    catch _:_ ->
            error
    end.

%% [{MsgId, Seq, Offset, Len}] for a segment's uncompressed frames.
index_of(Data) ->
    index_of(Data, 0, []).

index_of(<<Len:32, Msg:Len/binary, Len:32, _Next:32, Rest/binary>>,
         Offset, Acc) ->
    Entry = try
                #message{id = Id, sequence = Seq} = message:decode(Msg, false),
                [{Id, Seq, Offset, Len}]
            catch _:_ -> []      %% undecodable record: absent from the hint
            end,
    index_of(Rest, Offset + 4 + Len + 8, Entry ++ Acc);
index_of(_Rest, _Offset, Acc) ->
    lists:reverse(Acc).

%% The raw bytes of MsgId from Dir's archived segments, or not_found.
%%
%% Hints make this a search over small sidecar files instead of a
%% decompress-and-scan of every segment: only the segment whose hint
%% names MsgId is ever gunzipped.
find_in_archives(Dir, MsgId) ->
    find_in_archives_1(archive_segments(Dir), MsgId).

find_in_archives_1([], _MsgId) ->
    not_found;
find_in_archives_1([Gz | Rest], MsgId) ->
    case read_hint(Gz) of
        {ok, Index} ->
            case lists:keyfind(MsgId, 1, Index) of
                {MsgId, _Seq, Offset, Len} -> extract(Gz, Offset, Len, MsgId);
                false                      -> find_in_archives_1(Rest, MsgId)
            end;
        error ->
            find_in_archives_1(Rest, MsgId)
    end.

%% Pull one record out of a segment at a hinted offset.  A hint that does
%% not line up with the segment (hand-edited archive, truncated file) is
%% treated as absent rather than trusted.
extract(Gz, Offset, Len, MsgId) ->
    case read_archive(Gz) of
        {ok, Data} when byte_size(Data) >= Offset + 4 + Len ->
            case Data of
                <<_:Offset/binary, Len:32, Msg:Len/binary, _/binary>> ->
                    {ok, Msg};
                _ ->
                    ?SSB_ERROR("feed_store: hint for ~s in ~s does not match "
                               "the segment; ignoring it", [MsgId, Gz]),
                    not_found
            end;
        _ ->
            not_found
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

%% Archived segment files, oldest first.  Names are
%% log.offset.<From>-<To>.gz; sort numerically on From (lexicographic
%% ordering breaks once sequence numbers gain a digit).
%% The lowest sequence held in this feed's frozen segments, or `none` when
%% there are none.
%%
%% Read from the filenames, which do not lie: do_archive/7 derives the range
%% from the segment's own first record (an earlier version guessed it at
%% restart and produced archives whose names disagreed with their contents).
%% That keeps this a directory listing rather than a decompress of every
%% segment, which matters because it is asked on the serving path.
lowest_archived_seq(Dir) ->
    case archive_segments(Dir) of
        []           -> none;
        [Oldest | _] -> try archive_from(Oldest) catch _:_ -> none end
    end.

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

%% Read one framed record (<<Len:32, Msg:Len, Len:32, Next:32>>) at Offset
%% without holding the file open, returning the byte offset of the next record.
pread_record(Path, Offset) ->
    case file:open(Path, [read, binary, raw]) of
        {ok, Fd} ->
            Res = case file:pread(Fd, Offset, 4) of
                      {ok, <<Len:32>>} ->
                          case file:pread(Fd, Offset + 4, Len + 8) of
                              {ok, <<Msg:Len/binary, _Trailer:8/binary>>} ->
                                  {ok, Msg, Offset + 4 + Len + 8};
                              _ -> eof
                          end;
                      _ -> eof
                  end,
            ok = file:close(Fd),
            Res;
        {error, _} ->
            eof
    end.

-ifdef(TEST).

%% On-disk frame: <<Len:32, Msg:Len, Len:32, NextOffset:32>>.
frame(Msg) ->
    Len = byte_size(Msg),
    <<Len:32, Msg/binary, Len:32, 0:32>>.

hint_naming_test() ->
    ?assertEqual("/x/log.offset.1-2.hint", hint_file("/x/log.offset.1-2.gz")),
    %% hints must not look like segments to the "*.gz" glob
    ?assertNotEqual(".gz", filename:extension(hint_file("a/log.offset.3-9.gz"))).

%% The whole point of a hint is that it answers without the segment being
%% decompressed.  Write one for a .gz that does not exist at all: if
%% read_hint/1 still returns the index, it cannot have read the segment.
hint_is_authoritative_test() ->
    Dir = "/tmp/feed_store_hint_" ++
          integer_to_list(erlang:unique_integer([positive])),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Gz = filename:join(Dir, "log.offset.1-2.gz"),
    Index = [{~"%one.sha256", 1, 0, 40}, {~"%two.sha256", 2, 52, 40}],
    ok = file:write_file(hint_file(Gz),
                         term_to_binary({hint, 1, Index}, [compressed])),
    ?assertNot(filelib:is_file(Gz)),
    ?assertEqual({ok, Index}, read_hint(Gz)),
    %% and with neither hint nor segment there is nothing to report
    ok = file:delete(hint_file(Gz)),
    ?assertEqual(error, read_hint(Gz)),
    os:cmd("rm -rf " ++ Dir),
    ok.

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
