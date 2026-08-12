%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
%%
%% Per-feed gen_server.  Each SSB feed gets one instance, managed by
%% ssb_feed_sup.  Owns one append-only file: log.offset, this feed's
%% messages, plus the archived segments and hints beside it.
%%
%% It used to own three more, all of them indexes wearing a log's clothes
%% (doc/persistence.md §3):
%%
%%   profile     every `about` message, duplicated
%%   contacts    every `contact` message, duplicated
%%   references  tangle arcs, written into the TARGET author's directory
%%
%% profile and contacts fed a lazy loader that went away when the follow
%% graph became an ssb_view; they were being written on every store and
%% read by nobody.  references was a tangle-shaped index maintained by
%% the foundation — the layering error §5 describes — and is now the
%% ssb_links core view, which records references by shape and knows no
%% message type at all.
%%
%% Stale profile/contacts/references files in existing feed directories
%% are inert: nothing globs them, and they can be removed at leisure.
-module(ssb_feed).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("ssb/include/ssb.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([whoami/1,
         post_content/2,
         post_private/3,
         store_msg/2,
         store_msg_checked/2,
         fetch_msg/2,
         fetch_last_msg/1,
         foldl/3,
         archive/1,
         reset_log_slots/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile({no_auto_import,[size/1]}).
-import(utils, [load_term/1,
                 size/1]).

%% How long a feed may hold its log write handle without writing before
%% giving the fd back.  Long enough to span a replication burst, short
%% enough that an idle node settles back to no open logs.
-define(LOG_IDLE_MS, 30_000).

%% Ceiling on log write handles held across the whole node, and the thing
%% that actually bounds fd use — see log_fd/1.  Deliberately well under any
%% plausible RLIMIT_NOFILE, because these compete with sockets, blobs and
%% SQLite for the same budget: the handles are an optimisation, and running
%% out of fds is not a degradation but an outage.  Override with
%% {max_open_feed_logs, N} in the ssb app env.
-define(MAX_OPEN_LOGS, 64).
-define(LOG_SLOTS, {?MODULE, open_log_slots}).


-record(state, {id,
                last_msg = null,
                last_seq = 0,
                feed,
                msg_cache,
                %% has the whole live log been indexed into msg_cache in
                %% this run?  Reset whenever the live log is replaced.
                indexed = false,
                %% write handle for the live log, opened on first write and
                %% held while the feed stays busy.  undefined whenever the
                %% log it pointed at has been replaced (see close_log/1),
                %% or after an idle period (see close_idle_log).
                fd,
                %% has fd been written to since the last idle tick?  Drives
                %% the release of a quiet feed's handle.
                fd_used = false,
                %% undefined, or {FirstRejectedSeq, Count} while a peer is
                %% offering messages that do not link our tail.  Keeps the
                %% rejection log to one line per stall instead of one per
                %% message.
                chain_break,
                %% same shape, for messages that failed the signature check
                bad_sig}).
%%%===================================================================
%%% API
%%%===================================================================

start_link(FeedId) ->
    gen_server:start_link(?MODULE, [FeedId], []).

whoami(FeedPid) ->
    gen_server:call(FeedPid, whoami).

post_content(FeedPid, Content) ->
    gen_server:call(FeedPid, {post, Content}, infinity).

%% Encrypt Content as a private-box message addressed to RecipientIds
%% (list of <<"@pubkey.ed25519">> strings) and post it to the feed.
post_private(FeedPid, Content, RecipientIds) ->
    JsonContent = iolist_to_binary(message:ssb_encoder(Content, fun message:ssb_encoder/3, [])),
    Encrypted = private_box:encrypt(JsonContent, RecipientIds),
    gen_server:call(FeedPid, {post, Encrypted}, infinity).

store_msg(FeedPid, Msg) ->
    gen_server:call(FeedPid, {store, Msg}, infinity).

%% Like store_msg/2 but rejects a message that does not continue the feed's
%% chain (wrong `previous`, or a gap).  Used by the untrusted EBT ingest path;
%% trusted/local callers use store_msg/2.
store_msg_checked(FeedPid, Msg) ->
    gen_server:call(FeedPid, {store_checked, Msg}, infinity).

%% The message with id Key from this feed's history — the live log first,
%% then the archived segments — or `not_found` if the feed does not hold it.
fetch_msg(FeedPid, Key) ->
    gen_server:call(FeedPid, {fetch, Key}).

fetch_last_msg(FeedPid) ->
    gen_server:call(FeedPid, {fetch_last_msg}).

foldl(FeedPid, Fun, Acc) ->
    gen_server:call(FeedPid, {foldl, Fun, Acc}, infinity).

archive(FeedPid) ->
    gen_server:call(FeedPid, archive, infinity).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([FeedId]) ->
    process_flag(trap_exit, true),
    Feed = init_directories(FeedId),
    State = #state{id = FeedId,
                   feed = Feed,
                   msg_cache = ets:new(messages, [])},
    %% Register in the global feed registry when running under ssb_feed_sup.
    %% The guard keeps direct start_link/1 calls (e.g. in unit tests) working.
    case ets:info(ssb_feed_registry) of
        undefined -> ok;
        _         -> ets:insert(ssb_feed_registry, {FeedId, self()})
    end,
    {ok, check_owner_feed(State)}.

handle_call(archive, _From, #state{id = Id} = State) ->
    CanPost = Id == keys:pub_key_disp(),
    if CanPost ->
            {NewState, BlobId} = do_archive(State),
            {reply, {ok, BlobId}, NewState};
       true ->
            {reply, {error, not_owner}, State}
    end;

handle_call(whoami, _From, #state{id = Id} = State) ->
    {reply, Id, State};

handle_call({post, Content}, _From, #state{id = Id} = State) ->
    %% A given peer can only post to the feed it owns
    CanPost = Id == keys:pub_key_disp(),
    if CanPost ->
            NewState = post(Content, State),
            {reply, ok, NewState};
       true ->
            {reply, no_post, State}
    end;

handle_call({store, Msg}, _From, #state{last_seq = Before} = State) ->
    NewState = store(Msg, State),
    %% store/2 skips sequences we already have; report which happened so EBT
    %% only acks (and re-invites the peer) for genuinely new messages.
    Status = case NewState#state.last_seq > Before of
        true  -> stored;
        false -> skipped
    end,
    {reply, Status, NewState};

%% Ingest from an untrusted peer: verify the signature, then the chain.
%%
%% Two independent guards, and neither subsumes the other.  The chain
%% check stops a hole being spliced into a feed and served onward; the
%% signature check stops content being attributed to an author who never
%% wrote it.  A forged message can chain perfectly, and a genuine message
%% can arrive out of order, so both are needed.
handle_call({store_checked, Msg}, _From,
            #state{last_seq = Before, last_msg = LastMsg, id = FeedId} = State) ->
    case signature_ok(Msg, FeedId, State) of
        {false, State1} ->
            {reply, skipped, State1};
        {true, State1} ->
            store_if_chained(Msg, LastMsg, Before, FeedId, State1)
    end;


handle_call({fetch, Key}, _From, State) ->
    {Reply, NewState} = do_fetch(Key, State),
    {reply, Reply, NewState};

handle_call({fetch_last_msg}, _From, #state{feed = Feed,
                                           msg_cache = Messages} = State) ->
    Resp = feed_get_last(Feed),
    case Resp of
        {Pos, Msg, Key} ->
            ets:insert(Messages, {Key, Pos}),
            {reply, message:decode(Msg, false), State};
        Else ->
            {reply, Else, State}
    end;

handle_call({foldl, Fun, Acc}, _From, #state{feed = Feed} = State) ->
    {reply, utils:fold_log_file(Fun, Acc, Feed), State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% info

%% Idle tick for the live log's write handle.  A feed written since the
%% last tick keeps its handle and re-arms; a quiet one gives the fd back.
%% Nothing re-arms once the handle is gone — the next write opens it and
%% starts a fresh timer — so this cannot accumulate timers.
handle_info(close_idle_log, #state{fd = undefined} = State) ->
    {noreply, State};
handle_info(close_idle_log, #state{fd_used = true} = State) ->
    erlang:send_after(?LOG_IDLE_MS, self(), close_idle_log),
    {noreply, State#state{fd_used = false}};
handle_info(close_idle_log, State) ->
    {noreply, close_log(State)};

handle_info(Info, State) ->
    ?LOG_INFO("WTF: ~p ~n",[Info]),
    {noreply, State}.

%%

terminate(Reason, #state{id = FeedId} = State) ->
    _ = close_log(State),
    ?LOG_INFO("Closed gen_server: ~p ~n", [Reason]),
    case ets:info(ssb_feed_registry) of
        undefined -> ok;
        _         -> ets:delete(ssb_feed_registry, FeedId)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

post(Content, #state{id = FeedId, last_msg = Prev,
                     last_seq = Seq} = State) ->
    #message{id = Id} = Msg =
        message:new_msg(Prev, Seq + 1, Content,
                        {FeedId, keys:priv_key()}),
    State1 = store(Msg, State),
    State2 = State1#state{last_msg = Id, last_seq = Seq + 1},
    maybe_archive(State2).

maybe_archive(#state{last_seq = Seq} = State) ->
    case archive_length() of
        undefined ->
            State;
        Len when Seq rem Len =:= 0 ->
            {NewState, _} = do_archive(State),
            NewState;
        _ ->
            State
    end.

archive_length() ->
    config:archive_length().

do_archive(#state{id = FeedId, last_seq = LastSeq,
                  feed = FeedFile, msg_cache = Messages} = State) ->
    {ok, LogData} = file:read_file(FeedFile),
    %% Every indexed offset points into the live log we are about to
    %% delete, and the messages themselves are moving into the segment
    %% (where the hint file addresses them).  Drop the index and mark the
    %% new live log un-indexed; store/2 below re-seeds it with the
    %% archive-genesis message.
    ets:delete_all_objects(Messages),
    %% The segment's range comes from its own content: the first record
    %% of the live log.  (A tracked segment_start used to be guessed at
    %% restart and produced archives whose filenames lied about their
    %% ranges — content cannot.)
    From = first_seq(LogData),
    GzData = zlib:gzip(LogData),
    ArchiveFile = archive_filename(FeedFile, From, LastSeq),
    ok = file:write_file(ArchiveFile, GzData),
    %% Index the segment while its uncompressed bytes are still in hand,
    %% so later lookups need no decompress-and-scan (feed_store hints).
    _ = feed_store:write_hint(?b2l(ArchiveFile), LogData),
    BlobId = blobs:store(GzData),
    ok = file:delete(FeedFile),
    %% The handle we hold points at the log just deleted; an append to it
    %% would land on the unlinked inode and vanish.  Drop it so store/2
    %% below opens the new live log.
    State0 = close_log(State),
    Content = {[{~"type",          ~"archive"},
                {~"archive",       BlobId},
                {~"from_sequence", From},
                {~"to_sequence",   LastSeq}]},
    NewSeq = LastSeq + 1,
    #message{id = NewId} = Msg =
        message:new_msg(null, NewSeq, Content, {FeedId, keys:priv_key()}),
    State1 = store(Msg, State0#state{indexed = true}),
    {State1#state{last_msg = NewId,
                  last_seq = NewSeq}, BlobId}.

first_seq(<<Len:32, Msg:Len/binary, _/binary>>) ->
    #message{sequence = Seq} = message:decode(Msg, false),
    Seq.

archive_filename(FeedFile, From, To) ->
    <<FeedFile/binary, ".",
      (integer_to_binary(From))/binary, "-",
      (integer_to_binary(To))/binary, ".gz">>.

store_if_chained(Msg, LastMsg, Before, FeedId, State) ->
    case chain_continues(Msg, State) of
        false ->
            {reply, skipped, note_chain_break(Msg, LastMsg, Before, FeedId, State)};
        true ->
            NewState = store(Msg, State),
            Status = case NewState#state.last_seq > Before of
                true  -> stored;
                false -> skipped
            end,
            {reply, Status, clear_chain_break(Status, FeedId, NewState)}
    end.

%% Does this message carry a signature we verified?
%%
%% `#message.validated` is set by message:decode_value/2 and, until now,
%% was written by every ingest path and read by none — so a message with
%% a bad or absent signature was stored as readily as a good one, and
%% then re-served to other peers.
%%
%% Three outcomes, and the third is the one worth naming: `not_checked`
%% means the ingest site decoded without asking for verification, which
%% is a bug at that call site rather than an attack, and is reported
%% differently so it cannot hide among genuine failures.
%%
%% Default is to COUNT AND WARN, not reject.  Turning rejection on is a
%% config decision ({require_valid_sigs, true}) to be taken once the rate
%% on a real corpus is known — erlbutt re-encodes canonically to check a
%% signature, and if that ever disagrees with what was originally signed
%% the messages at stake are genuine ones.
signature_ok(#message{validated = true}, _FeedId, State) ->
    {true, State};
signature_ok(#message{sequence = Seq, validated = V}, FeedId, State) ->
    Reason = case V of
                 false -> ~"signature did not verify";
                 _     -> ~"ingest path did not verify the signature"
             end,
    count_bad_signature(),
    State1 = note_bad_signature(Seq, Reason, FeedId, State),
    {not config:require_valid_sigs(), State1}.

%% One line per feed, with the rest counted — a peer replaying a bad feed
%% would otherwise flood the log exactly when it is least readable.
note_bad_signature(Seq, Reason, FeedId, #state{bad_sig = undefined} = State) ->
    ?SSB_ERROR("feed ~s: seq ~p REJECTED BY SIGNATURE CHECK — ~s.  Mode is "
               "~s.  Further occurrences for this feed are counted, not "
               "logged; the running total is in admin.status as "
               "invalidSignatures.~n",
               [FeedId, Seq, Reason,
                case config:require_valid_sigs() of
                    true  -> ~"reject";
                    false -> ~"warn only, message stored"
                end]),
    State#state{bad_sig = {Seq, 1}};
note_bad_signature(_Seq, _Reason, _FeedId,
                   #state{bad_sig = {First, N}} = State) ->
    State#state{bad_sig = {First, N + 1}}.

%% Node-wide count, kept in the feed registry table so it survives any
%% one feed process and needs no owner of its own.
count_bad_signature() ->
    try ets:update_counter(ssb_feed_registry, '$invalid_sigs', {2, 1},
                           {'$invalid_sigs', 0})
    catch error:badarg -> 0          %% no registry (bare eunit): don't count
    end.

%% A peer offered a message that does not link our tail.
%%
%% One line per stall, not per message.  A peer whose EBT clock has run
%% ahead of us re-offers every message from its position onward, and at
%% one log line each that is thousands of lines that bury the one fact
%% worth knowing.  The first line carries the diagnosis — where the hole
%% is, how big, and why it will not close by itself — and the rest are
%% counted silently until the feed recovers.
note_chain_break(#message{sequence = Seq, previous = Prev}, LastMsg, Tail,
                 FeedId, #state{chain_break = undefined} = State) ->
    ?SSB_INFO("feed ~s: STALLED at seq ~p — a peer is offering seq ~p, a gap "
              "of ~p message(s).  Its EBT clock is ahead of ours and will not "
              "rewind, so nothing will close this gap until that peer's clock "
              "for us is reset (stop the peer, delete its stored clock for our "
              "id, restart).  Tail id ~p; offered previous ~p.  Further "
              "rejections for this feed are counted, not logged.~n",
              [FeedId, Tail, Seq, max(Seq - Tail - 1, 0), LastMsg, Prev]),
    State#state{chain_break = {Seq, 1}};
note_chain_break(_Msg, _LastMsg, _Tail, _FeedId,
                 #state{chain_break = {First, N}} = State) ->
    State#state{chain_break = {First, N + 1}}.

%% A store succeeded, so the stall is over — report what it cost.
clear_chain_break(stored, FeedId, #state{chain_break = {First, N},
                                         last_seq = Seq} = State) ->
    ?SSB_INFO("feed ~s: resumed at seq ~p after rejecting ~p message(s) "
              "from seq ~p~n", [FeedId, Seq, N, First]),
    State#state{chain_break = undefined};
clear_chain_break(_Status, _FeedId, State) ->
    State.

%% Whether a received message may be stored: a sequence we already hold (it
%% will be dedup-skipped), or the very next sequence whose `previous` matches
%% the id of our current tail.  Guards the received-message path against
%% chain-broken junk; local authoring (post/2) bypasses this.
chain_continues(#message{sequence = Seq}, #state{last_seq = LastSeq})
  when Seq =< LastSeq ->
    true;
chain_continues(#message{sequence = Seq, previous = Prev},
                #state{last_seq = LastSeq, last_msg = LastMsg}) ->
    Seq =:= LastSeq + 1 andalso same_ref(Prev, LastMsg).

%% Message-id equality that treats every "no previous" spelling (genesis) as
%% equal to an empty tail, so a genuine genesis (previous = null) is accepted.
same_ref(A, A) -> true;
same_ref(A, B) -> is_null_ref(A) andalso is_null_ref(B).

is_null_ref(null)      -> true;
is_null_ref(nil)       -> true;
is_null_ref(undefined) -> true;
is_null_ref(_)         -> false.

store(#message{sequence = Seq},
      #state{last_seq = LastSeq} = State) when Seq =< LastSeq ->
    %% Already have this sequence or earlier — skip silently.
    State;
store(#message{id = Id, sequence = Seq, author = Auth} = Msg,
      #state{msg_cache = Messages} = State0) ->
    mess_auth:put(Id, Auth),
    {Offset, State} = write_msg(Msg, State0),
    %% Keep the offset index current on the write path, so a message just
    %% stored is readable by id without re-scanning the live log.
    ets:insert(Messages, {Id, Offset}),
    %% arrival-order ref; the message body lives only in the feed's own log
    ingest_journal:append(Auth, Seq),
    social_msg:dispatch(Msg),
    view_manager:ingest(Msg),
    State#state{last_msg = Id, last_seq = Seq}.

write_msg(#message{} = DecMsg, State) ->
    write_msg(message:encode(DecMsg), State);

%% On-disk frame: <<Len:32, Msg:Len/binary, Len:32, NextOffset:32>>
%% Trailing Len enables backward seek to find the last record.
%% NextOffset is the absolute file position of the following record's Len field,
%% used by scan/3 to step forward without re-reading the leading length.
write_msg(Msg, State0) ->
    DataSiz = size(Msg),
    {Fd, State} = log_fd(State0),
    %% The record starts where the file currently ends; returned so the
    %% caller can index it without re-reading what it just wrote.  An lseek
    %% on the handle we already hold, where this was a path-based stat.
    {ok, Offset} = file:position(Fd, eof),
    %% NextOffset is arithmetic rather than a second stat: the record is
    %% the message plus its two 4-byte lengths and this 4-byte field.
    Next = Offset + DataSiz + 12,
    %% One write, not two: the frame is contiguous, so splitting it only
    %% doubled the syscalls.
    ok = file:write(Fd, <<DataSiz:32, Msg/binary, DataSiz:32, Next:32>>),
    %% A handle that could not be cached (the node is at its open-log cap)
    %% is given back immediately; a cached one is left for the next write.
    case State#state.fd of
        undefined -> ok = file:close(Fd);
        _         -> ok
    end,
    {Offset, State}.

%% The live log's write handle, opened on first use and kept.
%%
%% This used to be an open and a close around every single message, with a
%% filelib:file_size/1 stat on either side of the write — six syscalls per
%% stored message, on the hottest path in the system.  Holding the handle
%% costs one fd per live feed and removes all of it.
%%
%% `raw` matters as much as the reuse: a non-raw handle routes every
%% operation through the file server process, making each write a message
%% round trip rather than a syscall.  Raw handles may only be used by the
%% process that opened them, which is exactly the case here — the feed
%% gen_server is the sole writer of its own log.
%%
%% NOTE: do NOT add the `sync` flag.  It forces an fsync on every
%% file:write, and a stored message can hit more than one file (the
%% per-feed log, plus a references entry in each linked feed) — i.e.
%% several fsyncs per stored message.  On Linux that is ~60ms each, which
%% throttled EBT replication to ~4 msgs/sec and left peers stuck in
%% "Downloading new messages"/"Scuttling…" during a full-DB sync.  Plain
%% [append] still writes through to the OS, so the on-disk frame layout
%% stays correct; the OS flushes lazily, and any messages lost in a crash
%% are recovered by re-replication.
%%
%% Nor `delayed_write`: read_at/3 and scan/3 open their own handles, and a
%% buffered write would be invisible to them, so a fetch straight after a
%% store could miss the message it just wrote.
%% The handle is held for a burst, not forever: one fd per live feed would
%% otherwise be one fd per feed the node has ever touched, and a bulk
%% import touching thousands of feeds exhausts the limit in seconds —
%% emfile, mid-write, taking the feed down with it.  An idle timer alone
%% does NOT bound this: the handles are all claimed long before the first
%% tick fires.  So the handles are a bounded pool, and the timer is only
%% what returns a quiet feed's slot early.
%%
%% Past the cap a feed still writes — it just opens and closes around each
%% write, which is where this code started, except still ~2x faster than
%% the original thanks to `raw` and the dropped stats.  So the degradation
%% is graceful: a working set that fits keeps the handles (replication,
%% which writes a handful of feeds at a time), one that does not falls
%% back per write (a full conversion).
log_fd(#state{fd = Fd} = State) when Fd =/= undefined ->
    {Fd, State#state{fd_used = true}};
log_fd(#state{feed = Feed} = State) ->
    Fd = open_log(Feed),
    case claim_log_slot() of
        true ->
            erlang:send_after(?LOG_IDLE_MS, self(), close_idle_log),
            {Fd, State#state{fd = Fd, fd_used = true}};
        false ->
            %% not cached: write_msg/2 closes it again straight away
            {Fd, State}
    end.

open_log(Feed) ->
    case file:open(Feed, [append, raw, binary]) of
        {ok, Fd}        -> Fd;
        {error, Reason} -> error({log_open_failed, Feed, Reason})
    end.

%% Zero the open-handle count.  Called by ssb_feed_sup when it (re)starts,
%% at which point no feed it supervises is alive to hold a handle.
reset_log_slots() ->
    Ref = log_slots(),
    counters:put(Ref, 1, 0),
    ok.

%% Global count of cached log handles, as a lock-free counter: this is
%% consulted on every cache miss, so it must not serialise feeds through a
%% process.  Created by ssb_feed_sup before any feed exists; the lazy
%% branch is for tests that start a feed on its own.
log_slots() ->
    case persistent_term:get(?LOG_SLOTS, undefined) of
        undefined ->
            Ref = counters:new(1, [write_concurrency]),
            persistent_term:put(?LOG_SLOTS, Ref),
            persistent_term:get(?LOG_SLOTS);
        Ref ->
            Ref
    end.

claim_log_slot() ->
    Ref = log_slots(),
    Max = application:get_env(ssb, max_open_feed_logs, ?MAX_OPEN_LOGS),
    case counters:get(Ref, 1) < Max of
        true  -> counters:add(Ref, 1, 1), true;
        false -> false
    end.

release_log_slot() ->
    counters:sub(log_slots(), 1, 1).

%% Drop the write handle.  Called wherever the live log is replaced: an
%% append handle on a deleted inode still accepts writes, and they go
%% nowhere visible, so the next write must reopen.
close_log(#state{fd = undefined} = State) ->
    State;
close_log(#state{fd = Fd} = State) ->
    _ = file:close(Fd),
    release_log_slot(),
    State#state{fd = undefined, fd_used = false}.

init_directories(FeedId) ->
    FeedDir = utils:feed_dir(FeedId),
    Feed = <<FeedDir/binary,~"/"/binary,~"log.offset"/binary>>,
    filelib:ensure_dir(Feed),
    Feed.

%% Only feed corresponding to the owner of the peer can post.
%% All the other feeds are only meant to be read
check_owner_feed(#state{feed = Feed,
                       msg_cache = Messages} = State) ->
    Resp = feed_get_last(Feed),
    case Resp of
        no_file ->
            %% No live log.  Normally a brand-new feed — but if archives
            %% exist, this is the crash window in do_archive (old log
            %% deleted, genesis not yet stored): recover last_seq from
            %% the newest archive's content so we do not restart at 0
            %% and re-store duplicate sequences.
            recover_from_archives(State);
        done ->
            State;
        {Pos, Msg, Key} ->
            ets:insert(Messages, {Key, Pos}),
            #message{sequence = Seq} = message:decode(Msg, false),
            State#state{last_msg = Key,
                        last_seq = Seq}
    end.

recover_from_archives(#state{id = FeedId, feed = Feed} = State) ->
    Dir = filename:dirname(?b2l(Feed)),
    case filelib:wildcard(filename:join(Dir, "log.offset.*.gz")) of
        [] ->
            State;
        _Archives ->
            %% Content-derived (filenames of old archives can lie):
            %% the highest sequence anywhere in the archived history.
            {LastSeq, LastId} = feed_store:fold_feed(
                fun(Data, {SeqAcc, IdAcc}) ->
                        try message:decode(Data, false) of
                            #message{sequence = S, id = Id} when S > SeqAcc ->
                                {S, Id};
                            _ -> {SeqAcc, IdAcc}
                        catch _:_ -> {SeqAcc, IdAcc}
                        end
                end, {0, null}, Dir),
            ?SSB_INFO("feed ~s: no live log but archives present; "
                      "recovered last_seq ~p", [FeedId, LastSeq]),
            State#state{last_seq = LastSeq, last_msg = LastId}
    end.

%% Fetch a message by id: the live log's offset index first, then the
%% archived segments.
%%
%% msg_cache maps MsgId -> byte offset in log.offset.  It used to be a
%% read-through cache in front of a linear scan, so a cold lookup walked
%% the whole live log and a miss walked it to the end.  It is now an
%% index: the first miss indexes the entire live log in one pass, and
%% every lookup after that is a pread.  `indexed` records whether that
%% pass has happened in this run.  A message this feed does not hold
%% returns not_found rather than crashing the process, which is shared by
%% every caller of the feed.
do_fetch(Key, #state{feed = Feed, msg_cache = Messages} = State) ->
    case ets:lookup(Messages, Key) of
        [{Key, Offset}] ->
            case read_at(Feed, Offset, Key) of
                {ok, Msg} ->
                    {message:decode(Msg, false), State};
                stale ->
                    %% The log moved under an offset we recorded (an
                    %% external truncate or rewrite; do_archive resets the
                    %% index itself).  Drop the entry AND clear `indexed`,
                    %% so the retry re-indexes rather than concluding the
                    %% message is only in the archives — every other
                    %% offset is suspect for the same reason.
                    ets:delete(Messages, Key),
                    fetch_uncached(Key, State#state{indexed = false})
            end;
        [] ->
            fetch_uncached(Key, State)
    end.

fetch_uncached(Key, #state{indexed = true} = State) ->
    {fetch_archived(Key, State#state.feed), State};
fetch_uncached(Key, #state{feed = Feed, msg_cache = Messages} = State) ->
    build_index(Feed, Messages),
    do_fetch(Key, State#state{indexed = true}).

%% Index every record in the live log as MsgId -> Offset.  One pass, held
%% in memory: the live log is bounded by config:archive_length() (10k by
%% default), so this is a few MB, and it only runs for feeds someone
%% actually reads from — doing it in init/1 instead would put a full scan
%% of every feed on the boot path.
build_index(Feed, Messages) ->
    case file:read_file(Feed) of
        {ok, Bin} -> index_frames(Bin, 0, Messages);
        _         -> ok            %% no live log yet: nothing to index
    end.

index_frames(<<Len:32, Msg:Len/binary, Len:32, _Next:32, Rest/binary>>,
             Offset, Tab) ->
    try ets:insert(Tab, {extract_key(Msg), Offset})
    catch _:_ -> ok                %% undecodable record: not addressable
    end,
    index_frames(Rest, Offset + 4 + Len + 8, Tab);
index_frames(_Rest, _Offset, _Tab) ->
    ok.

%% Read the record at Offset and confirm it is the one we expect.  The
%% verification is what makes a stale index detectable rather than a
%% source of wrong answers.
read_at(Feed, Offset, Key) ->
    case file:open(Feed, [read, binary, raw]) of
        {ok, Fd} ->
            Res = try
                      {ok, <<Len:32>>} = file:pread(Fd, Offset, 4),
                      {ok, Msg} = file:pread(Fd, Offset + 4, Len),
                      Len = byte_size(Msg),
                      Key = extract_key(Msg),
                      {ok, Msg}
                  catch _:_ -> stale
                  end,
            ok = file:close(Fd),
            Res;
        {error, _} ->
            stale
    end.

%% Look Key up in the feed's archived segments.  feed_store consults each
%% segment's hint file, so only a segment that actually contains Key is
%% decompressed.  Offsets inside an archive are not live-log positions, so
%% a hit here is deliberately not added to msg_cache.
fetch_archived(Key, Feed) ->
    Dir = filename:dirname(?b2l(Feed)),
    case feed_store:find_in_archives(Dir, Key) of
        {ok, Msg}  -> message:decode(Msg, false);
        not_found  -> not_found
    end.

feed_get_last(Feed) ->
    case filelib:is_file(Feed) of
        true ->
            case file:open(Feed, [read, binary]) of
                {ok, IoDev} ->
                    %% Last 8 bytes = trailing Len(4) + NextOffset(4) of final record.
                    %% Read trailing Len, then seek back Len+4 to reach record start.
                    Beg = filelib:file_size(Feed) - 8,
                    file:position(IoDev, Beg),
                    case file:read(IoDev, 4) of
                        {ok, <<TermLenInt:32/integer>>} ->
                            file:position(IoDev, Beg - (TermLenInt + 4)),
                            {ok, Data} = load_term(IoDev),
                            file:close(IoDev),
                            Key = extract_key(Data),
                            {Beg - (TermLenInt + 4), Data, Key};
                        _Else ->
                            file:close(IoDev),
                            done
                    end;
               {error, Error} ->
                    ?LOG_INFO("Probably bad input ~p ~n",[{Error, Feed}]),
                    done
            end;
        false ->
            no_file
    end.

extract_key(Data) ->
    {DataProps} = utils:nat_decode(Data),
    ?pgv(~"key", DataProps).



-ifdef(TEST).

feed_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [fun post_and_fetch_test/1,
      fun sequence_increments_test/1,
      fun fetch_last_msg_test/1,
      fun store_msg_dedup_test/1,
      fun store_msg_checked_chain_test/1,
      fun chain_break_is_counted_then_cleared_test/1,
      fun bad_signature_warns_but_stores_test/1,
      fun bad_signature_rejected_when_enforcing_test/1,
      fun no_profile_or_contacts_files_test/1,
      fun fetch_missing_msg_test/1,
      fun archive_manual_test/1,
      fun fetch_archived_msg_test/1,
      fun archive_writes_hint_test/1,
      fun live_index_survives_stale_offset_test/1,
      fun cold_fetch_indexes_whole_live_log_test/1,
      fun missing_hint_rebuilt_test/1,
      fun corrupt_hint_tolerated_test/1,
      fun post_after_archive_test/1,
      fun second_archive_naming_test/1,
      fun restart_then_archive_naming_test/1,
      fun crash_window_recovery_test/1,
      fun frame_chain_is_walkable_test/1,
      fun idle_releases_log_handle_test/1,
      fun log_handles_are_capped_test/1]}.

%% Fully isolated home per test: these tests archive and restart the
%% own feed, and a home shared across eunit runs accumulates
%% overlapping archives (the old setup deleted only the live log,
%% which is exactly the do_archive crash window).
setup() ->
    teardown(ignore),
    Home = filename:join("/tmp", "feed_" ++
                          integer_to_list(erlang:system_time(microsecond))),
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

teardown(ignore) ->
    [catch gen_server:stop(Name)
     || Name <- [blobs, mess_auth, ssb_store, keys, config]],
    ok;
teardown({Pid, _, Home}) ->
    catch gen_server:stop(Pid),
    teardown(ignore),
    os:cmd("rm -rf " ++ Home),
    application:unset_env(ssb, ssb_home),
    ok.

feed_file(FeedId) ->
    Location = config:feed_loc(),
    DecId = utils:decode_id(FeedId),
    <<Dir:2/binary, Rest/binary>> = DecId,
    <<Location/binary, Dir/binary, "/", Rest/binary, "/log.offset">>.

%% Writing to more feeds than the cap allows must not exhaust fds.
%%
%% Regression for an emfile that killed a feed mid-write during a bulk
%% import: handles were held per feed with only an idle timer to release
%% them, and an import claims thousands of them long before the first tick
%% fires.  Feeds past the cap must still store — uncached, reopening per
%% write — rather than crash.
log_handles_are_capped_test({_Pid, _FeedId, _}) ->
    fun() ->
        Cap = 3,
        application:set_env(ssb, max_open_feed_logs, Cap),
        reset_log_slots(),
        Feeds = [begin
                     Id = <<"@", (base64:encode(crypto:hash(sha256,
                              <<"cap feed ", (integer_to_binary(N))/binary>>)))/binary,
                            ".ed25519">>,
                     {ok, P} = ssb_feed:start_link(Id),
                     {P, Id}
                 end || N <- lists:seq(1, Cap * 3)],
        %% every feed stores, whether or not it got a handle
        [begin
             M = message:new_msg(null, 1,
                                 {[{~"type", ~"post"}, {~"text", ~"capped"}]},
                                 {Id, keys:priv_key()}),
             ?assertEqual(stored, ssb_feed:store_msg(P, M))
         end || {P, Id} <- Feeds],
        %% at most Cap of them kept one, and the count agrees with reality
        Held = [P || {P, _} <- Feeds, feed_fd(P) =/= undefined],
        ?assert(length(Held) =< Cap),
        ?assertEqual(length(Held), counters:get(log_slots(), 1)),
        %% and every message really landed
        [?assertMatch(#message{sequence = 1}, ssb_feed:fetch_last_msg(P))
         || {P, _} <- Feeds],
        [gen_server:stop(P) || {P, _} <- Feeds],
        application:unset_env(ssb, max_open_feed_logs),
        reset_log_slots()
    end.

%% A quiet feed gives its log handle back, and writing again reopens it.
%%
%% The handle is held so a burst of stores costs one open instead of one
%% per message, but holding it forever would mean an fd per feed the node
%% has ever written — emfile on a node following a few thousand.  Drives
%% the tick directly rather than waiting ?LOG_IDLE_MS.
idle_releases_log_handle_test({Pid, FeedId, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"before idle"),
        ?assertNotEqual(undefined, feed_fd(Pid)),
        %% first tick: written since the last one, so the handle stays
        Pid ! close_idle_log,
        ?assertNotEqual(undefined, feed_fd(Pid)),
        %% second tick with no write in between: released
        Pid ! close_idle_log,
        ?assertEqual(undefined, feed_fd(Pid)),
        %% and the feed still writes, reopening as it goes
        ok = ssb_feed:post_content(Pid, ~"after idle"),
        ?assertNotEqual(undefined, feed_fd(Pid)),
        #message{id = Key, sequence = 2} = ssb_feed:fetch_last_msg(Pid),
        #message{content = ~"after idle"} = ssb_feed:fetch_msg(Pid, Key),
        %% both records are on disk and the chain still walks
        {ok, Bin} = file:read_file(feed_file(FeedId)),
        ?assertEqual({2, byte_size(Bin)}, walk_frames(Bin, 0, 0))
    end.

%% The feed's current log handle, via a sys call so the assertions read
%% real state rather than a value the test kept for itself.  element/2
%% rather than record syntax, matching the other state peeks below.
feed_fd(Pid) ->
    element(#state.fd, sys:get_state(Pid)).

%% Walk the live log using only the NextOffset field of each frame and
%% land exactly on every record, then exactly on EOF.
%%
%% write_msg/2 computes NextOffset arithmetically (Offset + Len + 12);
%% it used to re-stat the file after writing the record.  The two agree
%% only if the frame really is Len + two 4-byte lengths + this 4-byte
%% field, so this asserts that layout rather than trusting the sum —
%% scan/3 steps through the log on these offsets, so an error here would
%% surface far away, as a catch-up fold silently reading garbage.
frame_chain_is_walkable_test({Pid, FeedId, _}) ->
    fun() ->
        [ok = ssb_feed:post_content(Pid, <<"frame ", (integer_to_binary(N))/binary>>)
         || N <- lists:seq(1, 5)],
        {ok, Bin} = file:read_file(feed_file(FeedId)),
        ?assertEqual({5, byte_size(Bin)}, walk_frames(Bin, 0, 0))
    end.

%% Returns {RecordsSeen, FinalOffset}; crashes if a NextOffset does not
%% point at a frame boundary.
walk_frames(Bin, Pos, N) when Pos =:= byte_size(Bin) ->
    {N, Pos};
walk_frames(Bin, Pos, N) ->
    <<_:Pos/binary, Len:32, _Msg:Len/binary, Len:32, Next:32, _/binary>> = Bin,
    ?assertEqual(Pos + Len + 12, Next),
    walk_frames(Bin, Next, N + 1).

post_and_fetch_test({Pid, _, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"hello world"),
        #message{id = Key, sequence = 1} = ssb_feed:fetch_last_msg(Pid),
        #message{content = ~"hello world"} = ssb_feed:fetch_msg(Pid, Key)
    end.

sequence_increments_test({Pid, _, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"first"),
        ok = ssb_feed:post_content(Pid, ~"second"),
        ok = ssb_feed:post_content(Pid, ~"third"),
        #message{sequence = 3} = ssb_feed:fetch_last_msg(Pid)
    end.

fetch_last_msg_test({Pid, _, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"a"),
        ok = ssb_feed:post_content(Pid, ~"b"),
        ok = ssb_feed:post_content(Pid, ~"c"),
        #message{content = ~"c"} = ssb_feed:fetch_last_msg(Pid)
    end.

%% store_msg reports `stored` for a new sequence and `skipped` for a
%% duplicate, so EBT can avoid re-acking (and re-inviting) duplicates.
store_msg_dedup_test({Pid, FeedId, _}) ->
    fun() ->
        Msg = message:new_msg(null, 1, {[{~"type", ~"post"}, {~"text", ~"once"}]},
                              {FeedId, keys:priv_key()}),
        ?assertEqual(stored,  ssb_feed:store_msg(Pid, Msg)),
        ?assertEqual(skipped, ssb_feed:store_msg(Pid, Msg))
    end.

%% store_msg_checked/2 accepts a genesis and an in-chain successor, but
%% rejects a (validly signed) message whose `previous` does not link the tail
%% — the shape of the chain-broken junk lenient peers re-gossip.
store_msg_checked_chain_test({Pid, FeedId, _}) ->
    fun() ->
        Priv = keys:priv_key(),
        Post = fun(Prev, Seq, T) ->
                   message:new_msg(Prev, Seq,
                                   {[{~"type", ~"post"}, {~"text", T}]},
                                   {FeedId, Priv})
               end,
        Gen = Post(null, 1, ~"g"),
        ?assertEqual(stored,  ssb_feed:store_msg_checked(Pid, Gen)),
        Two = Post(Gen#message.id, 2, ~"two"),
        ?assertEqual(stored,  ssb_feed:store_msg_checked(Pid, Two)),
        %% seq 3 whose previous points at a bogus (non-canonical) id is rejected
        Bogus = <<"%", (binary:copy(~"A", 43))/binary, "=.sha256">>,
        Bad   = Post(Bogus, 3, ~"bad"),
        ?assertEqual(skipped, ssb_feed:store_msg_checked(Pid, Bad)),
        %% the correct seq 3 (previous = seq 2's id) is still accepted after
        Good  = Post(Two#message.id, 3, ~"three"),
        ?assertEqual(stored,  ssb_feed:store_msg_checked(Pid, Good))
    end.

%% Storing about and contact messages no longer writes the per-feed
%% `profile` and `contacts` side-logs.  They duplicated message bodies for
%% a lazy loader that no longer exists (doc/persistence.md §3); the feed
%% directory should now hold only log.offset and references.
no_profile_or_contacts_files_test({Pid, FeedId, _}) ->
    fun() ->
        Priv = keys:priv_key(),
        About = message:new_msg(null, 1,
                                {[{~"type",  ~"about"},
                                  {~"about", FeedId},
                                  {~"name",  ~"tester"}]},
                                {FeedId, Priv}),
        ?assertEqual(stored, ssb_feed:store_msg(Pid, About)),
        Contact = message:new_msg(About#message.id, 2,
                                  {[{~"type",      ~"contact"},
                                    {~"contact",   FeedId},
                                    {~"following", true}]},
                                  {FeedId, Priv}),
        ?assertEqual(stored, ssb_feed:store_msg(Pid, Contact)),
        Dir = filename:dirname(?b2l(feed_file(FeedId))),
        ?assertNot(filelib:is_file(filename:join(Dir, "profile"))),
        ?assertNot(filelib:is_file(filename:join(Dir, "contacts"))),
        %% the messages themselves are still stored
        ?assertMatch(#message{sequence = 2}, ssb_feed:fetch_last_msg(Pid))
    end.

%% An id this feed does not hold answers not_found — it must not badmatch
%% and take down the feed process, which is shared by every caller.
fetch_missing_msg_test({Pid, _, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"only message"),
        Absent = <<"%", (binary:copy(~"A", 43))/binary, "=.sha256">>,
        ?assertEqual(not_found, ssb_feed:fetch_msg(Pid, Absent)),
        ?assert(is_process_alive(Pid)),
        %% the live log is still readable afterwards
        #message{id = Key} = ssb_feed:fetch_last_msg(Pid),
        #message{content = ~"only message"} = ssb_feed:fetch_msg(Pid, Key)
    end.

%% Once history is archived it is no longer in log.offset, but fetch_msg
%% must still resolve it — previously this returned not_found from the
%% live-log scan and crashed the feed on the {Pos, Msg} badmatch.
fetch_archived_msg_test({Pid, _, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"archived one"),
        #message{id = Key} = ssb_feed:fetch_last_msg(Pid),
        ok = ssb_feed:post_content(Pid, ~"archived two"),
        {ok, _} = ssb_feed:archive(Pid),
        %% the live log now holds only the archive-genesis message
        #message{content = {Props}} = ssb_feed:fetch_last_msg(Pid),
        ?assertEqual(~"archive", proplists:get_value(~"type", Props)),
        #message{content = ~"archived one"} = ssb_feed:fetch_msg(Pid, Key),
        ?assert(is_process_alive(Pid))
    end.

archive_manual_test({Pid, _, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"x"),
        ok = ssb_feed:post_content(Pid, ~"y"),
        {ok, BlobId} = ssb_feed:archive(Pid),
        ?assert(blobs:has(BlobId) =:= true),
        #message{sequence = 3,
                 previous = null,
                 content  = {Props}} = ssb_feed:fetch_last_msg(Pid),
        ?assert(proplists:get_value(~"type",        Props) =:= ~"archive"),
        ?assert(proplists:get_value(~"to_sequence", Props) =:= 2)
    end.

post_after_archive_test({Pid, _, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"before"),
        {ok, _} = ssb_feed:archive(Pid),
        #message{id = GenesisId, sequence = GenesisSeq} = ssb_feed:fetch_last_msg(Pid),
        ok = ssb_feed:post_content(Pid, ~"after"),
        #message{previous = GenesisId, sequence = AfterSeq} = ssb_feed:fetch_last_msg(Pid),
        ?assert(AfterSeq =:= GenesisSeq + 1)
    end.

%% A run of chain-broken messages is logged once and counted, and the
%% count is cleared when the feed resumes.  The rejections must still all
%% be REJECTED — suppressing the log must not suppress the check.
chain_break_is_counted_then_cleared_test({Pid, FeedId, _}) ->
    fun() ->
        Priv = keys:priv_key(),
        Post = fun(Prev, Seq, T) ->
                   message:new_msg(Prev, Seq,
                                   {[{~"type", ~"post"}, {~"text", T}]},
                                   {FeedId, Priv})
               end,
        Gen = Post(null, 1, ~"g"),
        ?assertEqual(stored, ssb_feed:store_msg_checked(Pid, Gen)),
        ?assertEqual(undefined, feed_chain_break(Pid)),
        %% a peer whose clock has run ahead: seqs 5..9, none of which link
        Bogus = <<"%", (binary:copy(~"A", 43))/binary, "=.sha256">>,
        [?assertEqual(skipped,
                      ssb_feed:store_msg_checked(Pid, Post(Bogus, S, ~"ahead")))
         || S <- lists:seq(5, 9)],
        %% one stall, five rejections counted against it
        ?assertEqual({5, 5}, feed_chain_break(Pid)),
        %% the feed is untouched by any of them
        ?assertMatch(#message{sequence = 1}, ssb_feed:fetch_last_msg(Pid)),
        %% the real successor still lands, and clears the stall
        ?assertEqual(stored,
                     ssb_feed:store_msg_checked(Pid, Post(Gen#message.id, 2, ~"two"))),
        ?assertEqual(undefined, feed_chain_break(Pid)),
        ?assertMatch(#message{sequence = 2}, ssb_feed:fetch_last_msg(Pid))
    end.

feed_chain_break(Pid) ->
    element(#state.chain_break, sys:get_state(Pid)).

feed_bad_sig(Pid) ->
    element(#state.bad_sig, sys:get_state(Pid)).

%% Default mode is measure, not enforce: a message whose signature did
%% not verify is COUNTED and logged but still stored.  That is the whole
%% point of the first phase — find out whether a real corpus produces any
%% failures before anything starts being refused.
bad_signature_warns_but_stores_test({Pid, FeedId, _}) ->
    fun() ->
        ?assertEqual(false, config:require_valid_sigs()),
        Priv = keys:priv_key(),
        Good = message:new_msg(null, 1,
                               {[{~"type", ~"post"}, {~"text", ~"g"}]},
                               {FeedId, Priv}),
        %% new_msg leaves validated unset — an ingest path that never
        %% verified.  Distinct from a signature that failed, and reported
        %% as such, but still not something store_msg_checked may trust.
        ?assertEqual(undefined, Good#message.validated),
        ?assertEqual(stored, ssb_feed:store_msg_checked(Pid, Good)),
        ?assertMatch({1, 1}, feed_bad_sig(Pid)),
        %% stored despite the warning
        ?assertMatch(#message{sequence = 1}, ssb_feed:fetch_last_msg(Pid)),
        %% and further ones are counted, not re-logged
        Two = message:new_msg(Good#message.id, 2,
                              {[{~"type", ~"post"}, {~"text", ~"h"}]},
                              {FeedId, Priv}),
        ?assertEqual(stored, ssb_feed:store_msg_checked(Pid, Two)),
        ?assertMatch({1, 2}, feed_bad_sig(Pid)),
        %% a verified message passes straight through and is not counted
        Three = (message:new_msg(Two#message.id, 3,
                                 {[{~"type", ~"post"}, {~"text", ~"i"}]},
                                 {FeedId, Priv}))#message{validated = true},
        ?assertEqual(stored, ssb_feed:store_msg_checked(Pid, Three)),
        ?assertMatch({1, 2}, feed_bad_sig(Pid))
    end.

%% With {require_valid_sigs, true} the same message is refused, and the
%% feed is left untouched.
bad_signature_rejected_when_enforcing_test({Pid, FeedId, _}) ->
    fun() ->
        ok = config:set_require_valid_sigs(true),
        try
            ?assert(config:require_valid_sigs()),
            Msg = message:new_msg(null, 1,
                                  {[{~"type", ~"post"}, {~"text", ~"nope"}]},
                                  {FeedId, keys:priv_key()}),
            ?assertEqual(skipped, ssb_feed:store_msg_checked(Pid, Msg)),
            ?assertEqual(no_file, ssb_feed:fetch_last_msg(Pid)),
            %% a verified one still lands
            Ok = Msg#message{validated = true},
            ?assertEqual(stored, ssb_feed:store_msg_checked(Pid, Ok)),
            ?assertMatch(#message{sequence = 1}, ssb_feed:fetch_last_msg(Pid))
        after
            ok = config:set_require_valid_sigs(false)
        end
    end.

%% A restarted feed has an empty index; the first fetch indexes the whole
%% live log in one pass, so every message becomes readable by id — not
%% just the one that was asked for.
cold_fetch_indexes_whole_live_log_test({Pid, FeedId, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"cold one"),
        #message{id = K1} = ssb_feed:fetch_last_msg(Pid),
        ok = ssb_feed:post_content(Pid, ~"cold two"),
        #message{id = K2} = ssb_feed:fetch_last_msg(Pid),
        ok = ssb_feed:post_content(Pid, ~"cold three"),
        #message{id = K3} = ssb_feed:fetch_last_msg(Pid),
        %% restart: the index is volatile, so it starts empty
        ok = gen_server:stop(Pid),
        {ok, Pid2} = ssb_feed:start_link(FeedId),
        %% asking for the FIRST message indexes the whole log
        ?assertMatch(#message{content = ~"cold one"},
                     ssb_feed:fetch_msg(Pid2, K1)),
        ?assertMatch(#message{content = ~"cold two"},
                     ssb_feed:fetch_msg(Pid2, K2)),
        ?assertMatch(#message{content = ~"cold three"},
                     ssb_feed:fetch_msg(Pid2, K3)),
        ?assertEqual(not_found,
                     ssb_feed:fetch_msg(Pid2, ~"%nope.sha256")),
        ok = gen_server:stop(Pid2)
    end.

%% An indexed offset that no longer addresses its message — the live log
%% rewritten under us, as truncate_feed.escript does — must not produce a
%% wrong answer or a crash.  read_at/3 verifies the key of whatever it
%% finds, so the mismatch is detected and the index rebuilt.
live_index_survives_stale_offset_test({Pid, FeedId, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"first"),
        #message{id = K1} = ssb_feed:fetch_last_msg(Pid),
        ok = ssb_feed:post_content(Pid, ~"second"),
        #message{id = K2} = ssb_feed:fetch_last_msg(Pid),
        %% index both
        ?assertMatch(#message{content = ~"first"},  ssb_feed:fetch_msg(Pid, K1)),
        ?assertMatch(#message{content = ~"second"}, ssb_feed:fetch_msg(Pid, K2)),
        %% rewrite the log with the two records swapped: every indexed
        %% offset now points at the wrong message
        ok = swap_first_two_frames(?b2l(feed_file(FeedId))),
        ?assertMatch(#message{content = ~"first"},  ssb_feed:fetch_msg(Pid, K1)),
        ?assertMatch(#message{content = ~"second"}, ssb_feed:fetch_msg(Pid, K2)),
        ?assert(is_process_alive(Pid))
    end.

swap_first_two_frames(Path) ->
    {ok, Bin} = file:read_file(Path),
    <<L1:32, M1:L1/binary, L1:32, N1:32, Rest/binary>> = Bin,
    <<L2:32, M2:L2/binary, L2:32, N2:32, Tail/binary>> = Rest,
    F1 = <<L1:32, M1/binary, L1:32, N1:32>>,
    F2 = <<L2:32, M2/binary, L2:32, N2:32>>,
    file:write_file(Path, <<F2/binary, F1/binary, Tail/binary>>).

%% Archiving writes a .hint beside the .gz, listing every message in the
%% segment with the offset it starts at in the uncompressed frames.
archive_writes_hint_test({Pid, FeedId, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"hinted one"),
        #message{id = K1} = ssb_feed:fetch_last_msg(Pid),
        ok = ssb_feed:post_content(Pid, ~"hinted two"),
        #message{id = K2} = ssb_feed:fetch_last_msg(Pid),
        {ok, _} = ssb_feed:archive(Pid),
        [Gz]  = archive_paths(FeedId),
        Hint  = feed_store:hint_file(Gz),
        ?assert(filelib:is_file(Hint)),
        {ok, Index} = feed_store:read_hint(Gz),
        ?assertEqual([K1, K2], [Id || {Id, _Seq, _Off, _Len} <- Index]),
        ?assertEqual([1, 2],   [S  || {_Id, S, _Off, _Len} <- Index]),
        %% the hinted offsets really do address those records
        ?assertMatch(#message{content = ~"hinted one"},
                     ssb_feed:fetch_msg(Pid, K1)),
        ?assertMatch(#message{content = ~"hinted two"},
                     ssb_feed:fetch_msg(Pid, K2))
    end.

%% A segment archived before hints existed has none; the first lookup
%% rebuilds it from the segment and writes it for next time.
missing_hint_rebuilt_test({Pid, FeedId, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"pre-hint era"),
        #message{id = Key} = ssb_feed:fetch_last_msg(Pid),
        {ok, _} = ssb_feed:archive(Pid),
        [Gz] = archive_paths(FeedId),
        Hint = feed_store:hint_file(Gz),
        ok = file:delete(Hint),
        ?assertNot(filelib:is_file(Hint)),
        ?assertMatch(#message{content = ~"pre-hint era"},
                     ssb_feed:fetch_msg(Pid, Key)),
        ?assert(filelib:is_file(Hint))          %% healed itself
    end.

%% A corrupt hint must not make a message we hold look missing: it is
%% treated as absent and rebuilt.
corrupt_hint_tolerated_test({Pid, FeedId, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"survives a bad hint"),
        #message{id = Key} = ssb_feed:fetch_last_msg(Pid),
        {ok, _} = ssb_feed:archive(Pid),
        [Gz] = archive_paths(FeedId),
        Hint = feed_store:hint_file(Gz),
        ok = file:write_file(Hint, <<"not a term at all">>),
        ?assertMatch(#message{content = ~"survives a bad hint"},
                     ssb_feed:fetch_msg(Pid, Key)),
        %% rebuilt over the garbage
        ?assertMatch({ok, [{Key, 1, _, _}]}, feed_store:read_hint(Gz))
    end.

%% Full paths of a feed's archived segments.
archive_paths(FeedId) ->
    Dir = filename:dirname(?b2l(feed_file(FeedId))),
    lists:sort(filelib:wildcard(filename:join(Dir, "log.offset.*.gz"))).

archive_files(FeedId) ->
    Dir = filename:dirname(?b2l(feed_file(FeedId))),
    lists:sort([filename:basename(F)
                || F <- filelib:wildcard(filename:join(Dir, "log.offset.*.gz"))]).

%% The second archive's range comes from its content (its first record
%% is the previous archive-genesis message), not from a tracked counter.
second_archive_naming_test({Pid, FeedId, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"x"),          %% 1
        ok = ssb_feed:post_content(Pid, ~"y"),          %% 2
        {ok, _} = ssb_feed:archive(Pid),                %% genesis = 3
        ok = ssb_feed:post_content(Pid, ~"z"),          %% 4
        {ok, _} = ssb_feed:archive(Pid),                %% genesis = 5
        ?assertEqual(["log.offset.1-2.gz", "log.offset.3-4.gz"],
                     archive_files(FeedId))
    end.

%% A restart between archives must not reset the range bookkeeping
%% (the old segment_start guess produced a second archive named 1-N).
restart_then_archive_naming_test({Pid, FeedId, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"a"),          %% 1
        {ok, _} = ssb_feed:archive(Pid),                %% genesis = 2
        ok = ssb_feed:post_content(Pid, ~"b"),          %% 3
        ok = gen_server:stop(Pid),
        {ok, Pid2} = ssb_feed:start_link(FeedId),
        ok = ssb_feed:post_content(Pid2, ~"c"),         %% 4
        {ok, _} = ssb_feed:archive(Pid2),               %% genesis = 5
        ?assertEqual(["log.offset.1-1.gz", "log.offset.2-4.gz"],
                     archive_files(FeedId)),
        ok = gen_server:stop(Pid2)
    end.

%% Crash window in do_archive: old live log deleted, genesis not yet
%% stored.  Recovery must take last_seq from the archives' content, not
%% restart the feed at sequence 0 and re-store duplicates.
crash_window_recovery_test({Pid, FeedId, _}) ->
    fun() ->
        ok = ssb_feed:post_content(Pid, ~"one"),        %% 1
        ok = ssb_feed:post_content(Pid, ~"two"),        %% 2
        #message{id = LastId} = ssb_feed:fetch_last_msg(Pid),
        {ok, _} = ssb_feed:archive(Pid),                %% genesis = 3
        %% simulate the crash: live log (holding only the genesis) gone
        ok = gen_server:stop(Pid),
        ok = file:delete(?b2l(feed_file(FeedId))),
        {ok, Pid2} = ssb_feed:start_link(FeedId),
        %% recovered from archive content: next post continues the chain
        ok = ssb_feed:post_content(Pid2, ~"three"),
        #message{sequence = 3, previous = Prev} = ssb_feed:fetch_last_msg(Pid2),
        ?assertEqual(LastId, Prev),
        ok = gen_server:stop(Pid2)
    end.

-endif.
