%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Reading a bundle written by feed_export back into a node.
%%
%% A bundle is untrusted input — it arrived on a USB stick, or from
%% someone who says it is their medical record — so nothing in it is
%% believed until it has been checked, and the checks are the ones SSB
%% already relies on.  No new crypto:
%%
%%   every message's signature verifies
%%   every message's id is RECOMPUTED from its value; the `key` the
%%     bundle claims must match (an envelope key is not signed, and a
%%     wrong one stored would break the next real message's chain)
%%   every message is by the feed the manifest says, in sequence order
%%     from 1, each one's `previous` the id of the one before
%%   where the bundle overlaps what we already hold, it is the SAME
%%     chain — a different message at a sequence we hold is a fork, and
%%     is reported, never stored
%%   every blob hashes to its id
%%
%% New messages go in through ssb_feed:store_msg_checked/2, the same door
%% EBT uses, so they are indexed, journalled, put in mess_auth and
%% offered onward exactly like replicated ones.
%%
%% STOPPING HALFWAY IS SAFE.  A feed is imported in one pass, storing as
%% it goes, and the first bad line stops it.  Everything stored before
%% that line was individually checked and chains from 1, so what is left
%% is just a feed at a lower sequence — the same state an interrupted
%% replication leaves, and one the next import or peer simply continues.
%%
%% PATHS ARE DERIVED, NOT READ.  The manifest's `file` fields are
%% ignored: where a feed or blob sits in the bundle follows from its id
%% (feed_export:feed_file/1, blob_file/1), so a manifest cannot point the
%% importer anywhere outside the bundle.
%%
%% NOT YET: a feed we hold from a validation floor.  The bundle would
%% supply exactly the history below the floor, which is
%% archive_verify:check/4's job (with the seam check), not an append.
%% Such feeds are refused with that reason for now.
-module(feed_import).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([import/1,
         report_json/1]).

%% {ok, Report} | {error, Reason}
%%
%% Report is #{feeds => [FeedReport], blobs => BlobReport}; see
%% import_feed/2 and import_blobs/2 for their shapes.
import(Dir0) ->
    Dir = filename:absname(Dir0),
    case read_manifest(Dir) of
        {ok, Manifest} ->
            Feeds = [import_feed(Dir, F) || F <- list(~"feeds", Manifest)],
            Blobs = import_blobs(Dir, list(~"blobs", Manifest)),
            {ok, #{feeds => Feeds, blobs => Blobs}};
        {error, _} = E ->
            E
    end.

%% The report as JSON text, for admin.import's reply.
report_json(Report) ->
    iolist_to_binary(json:encode(Report)).

read_manifest(Dir) ->
    case file:read_file(filename:join(Dir, "manifest.json")) of
        {ok, Bin} ->
            try json:decode(Bin) of
                #{~"format" := ~"erlbutt-export", ~"version" := 1} = M ->
                    {ok, M};
                #{~"format" := ~"erlbutt-export", ~"version" := V} ->
                    {error, {unsupported_version, V}};
                _ ->
                    {error, not_a_bundle}
            catch _:_ ->
                    {error, bad_manifest}
            end;
        {error, enoent} ->
            {error, not_a_bundle};
        {error, R} ->
            {error, {manifest, R}}
    end.

list(Key, Manifest) ->
    case maps:get(Key, Manifest, []) of
        L when is_list(L) -> [E || E <- L, is_map(E)];
        _                 -> []
    end.

%%%===================================================================
%%% Feeds
%%%===================================================================

-record(imp, {feed,
              pid,
              held,             %% our sequence before the import
              held_id,          %% our id at `held`, or null
              seq   = 0,
              last  = null,
              added = 0}).

%% #{id, status, added, held, reason?}, status one of
%%   imported     new messages stored
%%   up_to_date   the bundle holds nothing we did not already have
%%   stopped      stored what checked out, then hit a bad line
%%   refused      nothing stored
import_feed(Dir, Entry) ->
    Id = maps:get(~"id", Entry, null),
    try
        Path = filename:join(Dir, feed_export:feed_file(Id)),
        ok   = check_file(Path, maps:get(~"sha256", Entry, null)),
        none = floor_of(Id),
        Pid  = feed_pid(Id),
        Held = ssb_feed:current_seq(Pid),
        S0   = #imp{feed = Id, pid = Pid, held = Held,
                    held_id = last_id(Pid, Held)},
        {Status, S} = stream(Path, S0),
        finish(Status, S, Entry)
    catch
        throw:{refused, Why} ->
            #{id => Id, status => ~"refused", added => 0,
              reason => reason(Why)};
        error:_ when not is_binary(Id) ->
            #{id => null, status => ~"refused", added => 0,
              reason => ~"manifest entry has no feed id"};
        error:{badmatch, {ok, _}} ->
            #{id => Id, status => ~"refused", added => 0,
              reason => reason(floored)};
        error:_ ->
            #{id => Id, status => ~"refused", added => 0,
              reason => reason(bad_id)}
    end.

check_file(Path, Sha) ->
    case file:read_file(Path) of
        {ok, _} when Sha =:= null ->
            ok;
        {ok, Bin} ->
            case binary:encode_hex(crypto:hash(sha256, Bin), lowercase) of
                Sha -> ok;
                _   -> throw({refused, sha256_mismatch})
            end;
        {error, _} ->
            throw({refused, no_feed_file})
    end.

floor_of(Id) ->
    feed_floor:get(Id).

feed_pid(Id) ->
    case utils:find_or_create_feed_pid(Id) of
        Pid when is_pid(Pid) -> Pid;
        _                    -> throw({refused, bad_id})
    end.

last_id(_Pid, 0) ->
    null;
last_id(Pid, _Held) ->
    #message{id = Id} = ssb_feed:fetch_last_msg(Pid),
    Id.

%% {ok | {stopped, Seq, Why}, #imp{}}
stream(Path, S0) ->
    {ok, Fd} = file:open(Path, [read, binary, compressed]),
    try loop(Fd, S0)
    after file:close(Fd)
    end.

loop(Fd, S) ->
    case file:read_line(Fd) of
        {ok, Line} ->
            Next = S#imp.seq + 1,
            try line(string:trim(Line, trailing, "\n"), S) of
                S1 -> loop(Fd, S1)
            catch
                throw:{bad, Why} -> {{stopped, Next, Why}, S};
                _:_              -> {{stopped, Next, malformed}, S}
            end;
        eof ->
            {ok, S};
        {error, _} ->
            {{stopped, S#imp.seq + 1, unreadable}, S}
    end.

line(Line, #imp{feed = Feed, seq = Seq, last = Last,
                held = Held, held_id = HeldId} = S) ->
    {Env} = utils:nat_decode(Line),
    {Value} = ?pgv(~"value", Env),
    #message{id = Id, author = Author, sequence = MsgSeq,
             previous = Prev, validated = Valid} = Msg =
        message:from_value(Value, true),
    if
        Valid =/= true          -> throw({bad, bad_signature});
        Author =/= Feed         -> throw({bad, {wrong_author, Author}});
        MsgSeq =/= Seq + 1      -> throw({bad, {out_of_sequence, MsgSeq}});
        true                    -> ok
    end,
    case ?pgv(~"key", Env) of
        Id -> ok;
        _  -> throw({bad, key_mismatch})
    end,
    case Seq of
        0 -> message:is_null_ref(Prev) orelse throw({bad, broken_chain});
        _ -> Prev =:= Last orelse throw({bad, broken_chain})
    end,
    Added = if
                MsgSeq < Held ->
                    0;
                MsgSeq =:= Held ->
                    %% the join: the bundle and our store must agree here,
                    %% or one of them is a fork of the other
                    Id =:= HeldId orelse throw({bad, fork}),
                    0;
                true ->
                    case ssb_feed:store_msg_checked(S#imp.pid, Msg) of
                        stored -> 1;
                        _      -> throw({bad, not_stored})
                    end
            end,
    S#imp{seq = MsgSeq, last = Id, added = S#imp.added + Added}.

%% A bundle that ends below what we hold never reached the join, so the
%% fork check has not run yet: our message at its last sequence must be
%% its last message.
finish(ok, #imp{seq = Seq, held = Held, last = Last} = S, Entry)
  when Seq > 0, Seq < Held ->
    case our_id_at(S#imp.feed, Seq) of
        Last -> finish_ok(S, Entry);
        _    -> stopped(S, Seq, fork)
    end;
finish(ok, #imp{seq = 0} = S, _Entry) ->
    stopped(S, 1, empty);
finish(ok, S, Entry) ->
    finish_ok(S, Entry);
finish({stopped, Seq, Why}, S, _Entry) ->
    stopped(S, Seq, Why).

%% Every line checked out.  The manifest's own account of the feed is
%% compared last: a file that verifies but stops short of what the
%% manifest promised was truncated, and that is worth saying even though
%% nothing stored is wrong.
finish_ok(#imp{seq = Seq, last = Last, added = Added} = S, Entry) ->
    case {maps:get(~"to", Entry, Seq), maps:get(~"latest", Entry, Last)} of
        {Seq, Last} ->
            #{id => S#imp.feed, held => S#imp.held, added => Added,
              to => Seq,
              status => case Added of
                            0 -> ~"up_to_date";
                            _ -> ~"imported"
                        end};
        {To, _} ->
            stopped(S, Seq + 1, {short, To})
    end.

stopped(#imp{feed = Feed, held = Held, added = Added, seq = Last}, Seq, Why) ->
    #{id => Feed, held => Held, added => Added, to => Last,
      status => case Added of
                    0 -> ~"refused";
                    _ -> ~"stopped"
                end,
      reason => iolist_to_binary([reason(Why),
                                  io_lib:format(" (at sequence ~b)", [Seq])])}.

our_id_at(FeedId, Seq) ->
    Dir = ?b2l(utils:feed_dir(FeedId)),
    try feed_store:fold_feed(
          fun(Data, Acc) ->
                  case message:decode(Data, false) of
                      #message{sequence = Seq, id = Id} -> throw({found, Id});
                      _                                 -> Acc
                  end
          end, null, Dir)
    catch throw:{found, Id} -> Id
    end.

reason(floored)               -> <<"held here from a validation floor; "
                                  "filling history below a floor is not "
                                  "supported yet">>;
reason(bad_id)                -> ~"not a well-formed feed id";
reason(no_feed_file)          -> ~"feed file missing from the bundle";
reason(sha256_mismatch)       -> <<"feed file does not match the manifest's "
                                  "sha256 (damaged or altered)">>;
reason(empty)                 -> ~"feed file holds no messages";
reason(bad_signature)         -> ~"signature does not verify";
reason(key_mismatch)          -> ~"message key is not the hash of its value";
reason({wrong_author, A})     -> <<"message by another author: ", A/binary>>;
reason({out_of_sequence, N})  -> iolist_to_binary(
                                   io_lib:format("out of order (sequence ~b)",
                                                 [N]));
reason(broken_chain)          -> ~"previous does not link the message before";
reason(fork)                  -> ~"FORK: differs from the copy held here";
reason(not_stored)            -> ~"feed refused the message";
reason(malformed)             -> ~"not a valid message record";
reason(unreadable)            -> ~"feed file could not be read";
reason({short, To})           -> iolist_to_binary(
                                   io_lib:format("file ends before the "
                                                 "manifest's sequence ~b",
                                                 [To])).

%%%===================================================================
%%% Blobs
%%%===================================================================

%% #{stored => N, present => N, missing => [Id], bad => [Id]}
%%   present  we already had it
%%   missing  listed in the manifest, not in the bundle
%%   bad      in the bundle, but its bytes do not hash to its id
import_blobs(Dir, Entries) ->
    lists:foldl(
      fun(Entry, Acc) ->
              Id = maps:get(~"id", Entry, null),
              Key = blob_outcome(Dir, Id),
              case Key of
                  stored  -> Acc#{stored  := maps:get(stored, Acc) + 1};
                  present -> Acc#{present := maps:get(present, Acc) + 1};
                  _       -> Acc#{Key := [Id | maps:get(Key, Acc)]}
              end
      end, #{stored => 0, present => 0, missing => [], bad => []}, Entries).

blob_outcome(Dir, Id) ->
    try
        Path = filename:join(Dir, feed_export:blob_file(Id)),
        case blobs:has(Id) of
            true ->
                present;
            false ->
                case file:read_file(Path) of
                    {ok, Data} ->
                        case blobs:store_verified(Id, Data) of
                            ok -> stored;
                            _  -> bad
                        end;
                    {error, _} ->
                        missing
                end
        end
    catch _:_ -> bad
    end.
