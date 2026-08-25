%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Checking an archive segment before believing it, and installing it once
%% believed.
%%
%% This is where a validation floor is paid off.  A node that adopted one
%% holds a feed from some sequence K, and the author's signed statement
%% that everything below K is in a particular blob.  Fetching that blob
%% and running it through here is the "validate further" half of the
%% bargain: the messages are checked individually, checked as a chain, and
%% checked to JOIN the chain we already hold.
%%
%% WHAT THE SEAM PROVES, AND WHAT IT DOES NOT.  The last message in the
%% segment must hash to the boundary's `previous`.  That makes the segment
%% this feed's own history rather than a plausible-looking run of messages
%% — you cannot reference a hash you have not seen, so the author
%% committed to exactly these bytes when they published the boundary.
%%
%% It does NOT prove the author never rewrote their past.  They could have
%% fabricated the whole segment at archive time and signed a boundary over
%% it, and everything here would pass, because they fabricated
%% consistently.  Only a witness who replicated the feed before it was
%% archived can catch that.  The floor trades that detection away, which
%% is why direct follows are never floored.
-module(archive_verify).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([check/4,
         install/5]).

%% Walk a decompressed segment and decide whether to believe it.
%%
%% Every message is decoded WITH signature verification.  That is the
%% expensive part and it is the point: this runs because a person asked
%% for the history to be validated, not on any hot path.
check(Raw, FeedId, PrevId, FloorSeq) ->
    try walk(Raw, FeedId, undefined, undefined) of
        {ok, FromSeq, ToSeq, LastId} ->
            if
                LastId =/= PrevId ->
                    %% The segment does not join the chain we hold.  Not a
                    %% transfer fault — evidence the history was altered.
                    {error, seam_mismatch};
                ToSeq + 1 =/= FloorSeq ->
                    %% Internally sound but it stops short of, or runs
                    %% past, the boundary it is supposed to sit under.
                    {error, {wrong_range, ToSeq, FloorSeq}};
                true ->
                    {ok, FromSeq, ToSeq}
            end;
        {error, _} = E ->
            E
    catch _:_ ->
        {error, malformed_segment}
    end.

%% Frame is write_msg/2's <<Len:32, Msg:Len/binary, Len:32, Next:32>>.
%% Binding the trailing length to the same Len rejects a torn segment
%% before anything is decoded.
walk(<<>>, _FeedId, undefined, _Prev) ->
    {error, empty_segment};
walk(<<>>, _FeedId, {FromSeq, ToSeq, LastId}, _Prev) ->
    {ok, FromSeq, ToSeq, LastId};
walk(<<Len:32, Bin:Len/binary, Len:32, _Next:32, Rest/binary>>,
     FeedId, Acc, PrevId) ->
    case message:decode(Bin, true) of
        #message{validated = true, author = FeedId, id = Id,
                 sequence = Seq, previous = Prev} ->
            case chains(Seq, Prev, Acc, PrevId) of
                ok ->
                    NewAcc = case Acc of
                                 undefined      -> {Seq, Seq, Id};
                                 {From, _, _}   -> {From, Seq, Id}
                             end,
                    walk(Rest, FeedId, NewAcc, Id);
                {error, _} = E ->
                    E
            end;
        #message{author = Other} when Other =/= FeedId ->
            {error, {wrong_author, Other}};
        _ ->
            {error, bad_signature}
    end;
walk(_, _, _, _) ->
    {error, malformed_segment}.

%% The first message may sit at any sequence — an archive of an archive
%% starts partway down — but every one after it must follow its
%% predecessor exactly.
chains(_Seq, _Prev, undefined, _PrevId) ->
    ok;
chains(Seq, Prev, {_From, LastSeq, LastId}, _PrevId) ->
    if
        Seq =/= LastSeq + 1 -> {error, {sequence_gap, LastSeq, Seq}};
        Prev =/= LastId     -> {error, {broken_chain, Seq}};
        true                -> ok
    end.

%% Put a verified segment where feed_store already knows to look for it:
%% beside the live log, named for the range it covers, with a hint so
%% later lookups need no decompress-and-scan.
%%
%% The blob's bytes are written verbatim.  A segment's internal offsets
%% are not file positions — feed_store addresses records through the hint
%% — so the bytes the author froze are the bytes we keep, and the blob
%% hash still describes what is on disk.
install(FeedId, Gz, Raw, FromSeq, ToSeq) ->
    Dir  = ?b2l(utils:feed_dir(FeedId)),
    Name = "log.offset." ++ integer_to_list(FromSeq)
           ++ "-" ++ integer_to_list(ToSeq) ++ ".gz",
    Path = filename:join(Dir, Name),
    case filelib:is_file(Path) of
        true ->
            %% Already installed; importing twice is not an error.
            ok;
        false ->
            ok = filelib:ensure_dir(Path),
            ok = file:write_file(Path, Gz),
            _  = feed_store:write_hint(Path, Raw),
            ok
    end.
