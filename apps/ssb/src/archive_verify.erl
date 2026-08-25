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

-ifdef(TEST).

verify_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun accepts_a_real_archive/1,
      fun rejects_a_seam_mismatch/1,
      fun rejects_a_broken_chain/1,
      fun rejects_a_foreign_author/1,
      fun install_is_idempotent/1]}.

setup() ->
    cleanup(ignore),
    Home = filename:join("/tmp", "archverify_"
                         ++ integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = ssb_store:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    FeedId = keys:pub_key_disp(),
    {ok, Pid} = ssb_feed:start_link(FeedId),
    {Pid, FeedId, Home}.

cleanup(ignore) ->
    [catch gen_server:stop(N)
     || N <- [blobs, mess_auth, ssb_store, keys, config]],
    ok;
cleanup({Pid, _, Home}) ->
    catch gen_server:stop(Pid),
    cleanup(ignore),
    os:cmd("rm -rf " ++ Home),
    application:unset_env(ssb, ssb_home),
    ok.

%% Produce a real segment the only way one is ever produced, then read
%% back exactly what a fetching node would have: the blob and the
%% boundary that names it.
archived(Pid) ->
    ok = ssb_feed:post_content(Pid, ~"one"),
    ok = ssb_feed:post_content(Pid, ~"two"),
    {ok, Blob} = ssb_feed:archive(Pid),
    #message{sequence = FloorSeq, previous = PrevId} =
        ssb_feed:fetch_last_msg(Pid),
    {ok, Gz} = blobs:fetch(Blob),
    {Gz, zlib:gunzip(Gz), PrevId, FloorSeq}.

%% The ordinary case: every signature checks, the chain is unbroken, and
%% the last message hashes to the boundary's previous.
accepts_a_real_archive({Pid, FeedId, _}) ->
    fun() ->
        {_Gz, Raw, PrevId, FloorSeq} = archived(Pid),
        ?assertEqual({ok, 1, 2}, check(Raw, FeedId, PrevId, FloorSeq))
    end.

%% The seam is the whole proof that a segment is THIS feed's history and
%% not a plausible-looking run of messages, so a wrong previous must be
%% refused even though every message in the segment is genuine.
rejects_a_seam_mismatch({Pid, FeedId, _}) ->
    fun() ->
        {_Gz, Raw, _PrevId, FloorSeq} = archived(Pid),
        Wrong = <<"%", (binary:copy(~"A", 43))/binary, "=.sha256">>,
        ?assertEqual({error, seam_mismatch},
                     check(Raw, FeedId, Wrong, FloorSeq))
    end.

%% A segment whose records do not follow one another is refused before
%% the seam is even reached.
%%
%% The gap has to be in the MIDDLE: dropping the first record just leaves
%% a shorter segment that starts higher up, which is legitimate — an
%% archive of an archive does exactly that — and check/4 accepts it.
rejects_a_broken_chain({Pid, FeedId, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"one"),
        ok = ssb_feed:post_content(Pid, ~"two"),
        ok = ssb_feed:post_content(Pid, ~"three"),
        {ok, Blob} = ssb_feed:archive(Pid),
        #message{sequence = FloorSeq, previous = PrevId} =
            ssb_feed:fetch_last_msg(Pid),
        {ok, Gz} = blobs:fetch(Blob),
        [R1, _R2, R3] = frames(zlib:gunzip(Gz)),
        ?assertMatch({error, {sequence_gap, 1, 3}},
                     check(<<R1/binary, R3/binary>>, FeedId, PrevId, FloorSeq))
    end.

%% Split a segment back into whole frames, so a test can drop one.
frames(<<>>) ->
    [];
frames(<<Len:32, Msg:Len/binary, Len:32, Next:32, Rest/binary>>) ->
    [<<Len:32, Msg/binary, Len:32, Next:32>> | frames(Rest)].

rejects_a_foreign_author({Pid, _FeedId, _}) ->
    fun() ->
        {_Gz, Raw, PrevId, FloorSeq} = archived(Pid),
        Other = ~"@somebodyelse.ed25519",
        ?assertMatch({error, {wrong_author, _}},
                     check(Raw, Other, PrevId, FloorSeq))
    end.

%% Importing twice must not error or duplicate the segment — a client that
%% retries a fetch should land in the same place.
install_is_idempotent({Pid, FeedId, _}) ->
    fun() ->
        {Gz, Raw, _PrevId, _FloorSeq} = archived(Pid),
        ok = install(FeedId, Gz, Raw, 1, 2),
        ok = install(FeedId, Gz, Raw, 1, 2),
        Dir = ?b2l(utils:feed_dir(FeedId)),
        ?assertEqual(1, length(filelib:wildcard(
                                 filename:join(Dir, "log.offset.1-2.gz"))))
    end.

-endif.
