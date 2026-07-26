#!/usr/bin/env escript
%%! -pa _build/default/lib/ssb/ebin
%%
%% truncate_feed.escript — surgically cut a feed's log.offset back to a chosen
%% sequence, discarding everything after it, and correct the surviving anchor
%% record's stored key to the canonical (latin1) id if it drifted.
%%
%% Use case: a feed whose tail is invalid (e.g. messages authored with a
%% non-canonical `previous` link) needs to be reset to its last-good sequence
%% so the node can re-author forward with a correct chain.
%%
%% Runs OFFLINE — stop erlbutt first so the log isn't being written.
%% DRY RUN by default: it only reports what it would change.  Add --apply to
%% actually rewrite the file (it backs the original up first).
%%
%%   ./truncate_feed.escript <path/to/feeds/<feed>/log.offset> <keep-seq>
%%   ./truncate_feed.escript <path/to/.../log.offset> <keep-seq> --apply
%%
%% NOTE: this only edits log.offset.  erlbutt's side indexes (mess_auth.ets,
%% ingest.journal, views/, references) may still reference the dropped
%% sequences; they are harmless for replication but can be rebuilt separately
%% if stale lookups bother you.

-include_lib("ssb/include/ssb.hrl").
-mode(compile).

main([Path, KeepStr]) ->
    run(Path, list_to_integer(KeepStr), dry);
main([Path, KeepStr, "--apply"]) ->
    run(Path, list_to_integer(KeepStr), apply);
main(_) ->
    io:format(
      "Usage: truncate_feed.escript <path/to/log.offset> <keep-seq> [--apply]~n"
      "  Dry-run by default; reports what it would change.~n"
      "  Keeps records with sequence =< keep-seq, drops the rest, and fixes~n"
      "  the keep-seq record's stored key to the canonical id if needed.~n"),
    halt(1).

run(Path, Keep, Mode) ->
    case file:read_file(Path) of
        {error, R} ->
            io:format("Cannot read ~s: ~p~n", [Path, R]), halt(1);
        {ok, File} ->
            Records = parse(File, 0),
            io:format("Parsed ~p records from ~s (~p bytes); keep-seq = ~p~n~n",
                      [length(Records), Path, byte_size(File), Keep]),
            Rows = [analyze(Rec) || Rec <- Records],
            lists:foreach(fun(Row) -> report(Row, Keep) end, Rows),
            Kept    = [Row || Row <- Rows, maps:get(seq, Row) =< Keep],
            Dropped = [Row || Row <- Rows, maps:get(seq, Row) >  Keep],
            io:format("~n~p records kept (seq =< ~p), ~p dropped (seq > ~p)~n",
                      [length(Kept), Keep, length(Dropped), Keep]),
            case [Row || Row <- Kept, maps:get(seq, Row) =:= Keep] of
                [Anchor] ->
                    summarize_anchor(Anchor),
                    case Mode of
                        dry ->
                            io:format("~nDRY RUN — nothing written. "
                                      "Re-run with --apply to perform.~n");
                        apply ->
                            do_apply(Path, File, Anchor)
                    end;
                [] ->
                    io:format("~nERROR: no record at seq ~p; refusing to act.~n",
                              [Keep]),
                    halt(1)
            end
    end.

%% Split the offset log into {Offset, Len, Msg} records.
%% Frame: <<Len:32, Msg:Len, Len:32, NextOffset:32>> (record size = Len + 12).
parse(<<Len:32, Msg:Len/binary, _Len2:32, _Next:32, Rest/binary>>, Off) ->
    [{Off, Len, Msg} | parse(Rest, Off + Len + 12)];
parse(<<>>, _Off) ->
    [];
parse(Other, Off) ->
    io:format("WARNING: ~p trailing bytes at offset ~p are not a valid frame; "
              "ignoring~n", [byte_size(Other), Off]),
    [].

analyze({Off, Len, Msg}) ->
    {EnvProps} = utils:nat_decode(Msg),
    StoredKey  = proplists:get_value(<<"key">>, EnvProps),
    ValueTerm  = proplists:get_value(<<"value">>, EnvProps),
    ValueJson  = iolist_to_binary(
                   message:ssb_encoder(ValueTerm, fun message:ssb_encoder/3,
                                       [use_nil])),
    #message{id = CanonKey, sequence = Seq, previous = Prev} =
        message:decode_value(ValueJson, false),
    #{off => Off, len => Len, seq => Seq,
      stored => StoredKey, canon => CanonKey, prev => Prev}.

report(#{seq := Seq, stored := Stored, canon := Canon, prev := Prev}, Keep) ->
    Fate = case Seq =< Keep of true -> "keep"; false -> "DROP" end,
    Flag = case Stored =:= Canon of
               true  -> "";
               false -> "  <-- stored key != canonical!"
           end,
    io:format("  seq ~4w [~s]  key=~s  prev=~s~s~n",
              [Seq, Fate, short(Stored), short(Prev), Flag]).

summarize_anchor(#{seq := Seq, stored := Stored, canon := Canon}) ->
    io:format("~nAnchor (seq ~p):~n  stored key : ~s~n  canonical  : ~s~n",
              [Seq, Stored, Canon]),
    case Stored =:= Canon of
        true  -> io:format("  anchor key already canonical; only truncation needed.~n");
        false -> io:format("  anchor key will be corrected to canonical.~n")
    end.

do_apply(Path, File, #{off := Off, len := Len, stored := Stored, canon := Canon}) ->
    TruncPos = Off + Len + 12,
    Prefix0  = binary:part(File, 0, TruncPos),
    NewData  =
        case Stored =:= Canon of
            true -> Prefix0;
            false ->
                %% same-length key => in-place replace inside the anchor's Msg,
                %% frame lengths and NextOffset stay valid.
                Before = binary:part(Prefix0, 0, Off + 4),
                MsgBin = binary:part(Prefix0, Off + 4, Len),
                Tail   = binary:part(Prefix0, Off + 4 + Len,
                                     TruncPos - (Off + 4 + Len)),
                Patched = binary:replace(MsgBin, Stored, Canon),
                case Len =:= byte_size(Patched) of
                    true  -> ok;
                    false ->
                        io:format("ABORT: key replacement changed record "
                                  "length; not writing.~n"),
                        halt(1)
                end,
                <<Before/binary, Patched/binary, Tail/binary>>
        end,
    Bak = Path ++ ".bak." ++ integer_to_list(erlang:system_time(second)),
    ok  = file:write_file(Bak, File),
    ok  = file:write_file(Path, NewData),
    io:format("~nAPPLIED.~n  backup : ~s (~p bytes)~n  new    : ~s (~p bytes)~n"
              "Restart erlbutt, verify the feed ends at the keep-seq, then "
              "author forward.~n",
              [Bak, byte_size(File), Path, byte_size(NewData)]).

short(undefined) -> "null";
short(null)      -> "null";
short(Bin) when is_binary(Bin), byte_size(Bin) >= 12 ->
    binary_to_list(binary:part(Bin, 0, 12)) ++ "...";
short(Bin) when is_binary(Bin) -> binary_to_list(Bin);
short(Other) -> io_lib:format("~p", [Other]).
