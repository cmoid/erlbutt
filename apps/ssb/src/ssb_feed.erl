%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
%%
%% Per-feed gen_server.  Each SSB author gets one instance, managed by
%% ssb_feed_sup.  Owns four append-only files: log.offset (all messages),
%% profile (about messages only), contacts (contact/follow messages only),
%% and references (tangle arc records).
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
         store_ref/2,
         references/3,
         foldl/3,
         fold_contacts/3,
         profile_name/1,
         archive/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile({no_auto_import,[size/1]}).
-import(utils, [load_term/1,
                 size/1]).


-record(state, {id,
                last_msg = null,
                last_seq = 0,
                feed,
                profile,
                contacts,
                refs,
                msg_cache}).
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

fetch_msg(FeedPid, Key) ->
    gen_server:call(FeedPid, {fetch, Key}).

fetch_last_msg(FeedPid) ->
    gen_server:call(FeedPid, {fetch_last_msg}).

%% Cast, not call: tangle calls this from inside a feed's handle_call, so a
%% synchronous call here would deadlock the same process.
store_ref(FeedPid, Arrow) ->
    gen_server:cast(FeedPid, {store_ref, Arrow}).

references(FeedPid, MsgId, RootId) ->
    gen_server:call(FeedPid, {refs, MsgId, RootId}, infinity).

foldl(FeedPid, Fun, Acc) ->
    gen_server:call(FeedPid, {foldl, Fun, Acc}, infinity).

fold_contacts(FeedPid, Fun, Acc) ->
    gen_server:call(FeedPid, {fold_contacts, Fun, Acc}, infinity).

%% Return the most recent self-chosen display name from the feed's profile,
%% or undefined if none has been set.
profile_name(FeedPid) ->
    gen_server:call(FeedPid, profile_name, infinity).

archive(FeedPid) ->
    gen_server:call(FeedPid, archive, infinity).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([FeedId]) ->
    process_flag(trap_exit, true),
    {Feed, Profile, Contacts, Refs} = init_directories(FeedId),
    State = #state{id = FeedId,
                   feed = Feed,
                   profile = Profile,
                   contacts = Contacts,
                   refs = Refs,
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

handle_call({store_checked, Msg}, _From,
            #state{last_seq = Before, last_msg = LastMsg, id = FeedId} = State) ->
    %% Chain-validated store for the untrusted replication path.  erlbutt only
    %% verified the signature on ingest, so a message whose `previous` linked a
    %% non-canonical id (the UTF-8/latin1 id bug) would splice a broken chain
    %% into the log — and lenient peers keep re-gossiping such junk.  Accept only a sequence
    %% we already hold (store/2 dedups it) or the next sequence whose previous
    %% matches our current tail.
    case chain_continues(Msg, State) of
        false ->
            #message{sequence = Seq, previous = Prev} = Msg,
            ?SSB_INFO("feed ~s: rejecting seq ~p — chain break "
                      "(tail seq ~p id ~p; msg previous ~p)~n",
                      [FeedId, Seq, Before, LastMsg, Prev]),
            {reply, skipped, State};
        true ->
            NewState = store(Msg, State),
            Status = case NewState#state.last_seq > Before of
                true  -> stored;
                false -> skipped
            end,
            {reply, Status, NewState}
    end;


handle_call({fetch, Key}, _From, #state{feed = Feed,
                                       msg_cache = Messages} = State) ->
    Val = ets:lookup(Messages, Key),
    {Pos, Msg} = feed_get(Feed, Val, Key),
    case Val of
        [] ->
            ets:insert(Messages, {Key, Pos});
        _Else ->
            nop
    end,
    {reply, message:decode(Msg, false), State};

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

handle_call({refs, MsgId, TangleId}, _From, #state{refs = Refs} = State) ->
    Fun =
        fun(Data, Acc) ->
                IsArc = has_target(Data, MsgId, TangleId),
                case IsArc of
                    false ->
                        Acc;
                    Targets ->
                        [Targets | Acc]
                end end,

    Result = utils:fold_log_file(Fun, [], Refs),
    {reply, Result, State};

handle_call({foldl, Fun, Acc}, _From, #state{feed = Feed} = State) ->
    {reply, utils:fold_log_file(Fun, Acc, Feed), State};

handle_call({fold_contacts, Fun, Acc}, _From, #state{contacts = Contacts} = State) ->
    {reply, utils:fold_log_file(Fun, Acc, Contacts), State};

handle_call(profile_name, _From, #state{id = Id, profile = Profile} = State) ->
    Name = utils:fold_log_file(
        fun(MsgData, Acc) ->
            try
                #message{content = {Props}} = message:decode(MsgData, false),
                case {?pgv(~"about", Props), ?pgv(~"name", Props)} of
                    {Id, N} when is_binary(N) -> N;
                    _                         -> Acc
                end
            catch _:_ -> Acc
            end
        end, undefined, Profile),
    {reply, Name, State}.

handle_cast({store_ref, Arrow}, #state{refs = Refs} = State) ->
    write_msg(Arrow, Refs),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%% info

handle_info(Info, State) ->
    ?LOG_INFO("WTF: ~p ~n",[Info]),
    {noreply, State}.

%%

terminate(Reason, #state{id = FeedId}) ->
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
                  feed = FeedFile} = State) ->
    {ok, LogData} = file:read_file(FeedFile),
    %% The segment's range comes from its own content: the first record
    %% of the live log.  (A tracked segment_start used to be guessed at
    %% restart and produced archives whose filenames lied about their
    %% ranges — content cannot.)
    From = first_seq(LogData),
    GzData = zlib:gzip(LogData),
    ArchiveFile = archive_filename(FeedFile, From, LastSeq),
    ok = file:write_file(ArchiveFile, GzData),
    BlobId = blobs:store(GzData),
    ok = file:delete(FeedFile),
    Content = {[{~"type",          ~"archive"},
                {~"archive",       BlobId},
                {~"from_sequence", From},
                {~"to_sequence",   LastSeq}]},
    NewSeq = LastSeq + 1,
    #message{id = NewId} = Msg =
        message:new_msg(null, NewSeq, Content, {FeedId, keys:priv_key()}),
    State1 = store(Msg, State),
    {State1#state{last_msg = NewId,
                  last_seq = NewSeq}, BlobId}.

first_seq(<<Len:32, Msg:Len/binary, _/binary>>) ->
    #message{sequence = Seq} = message:decode(Msg, false),
    Seq.

archive_filename(FeedFile, From, To) ->
    <<FeedFile/binary, ".",
      (integer_to_binary(From))/binary, "-",
      (integer_to_binary(To))/binary, ".gz">>.

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
      #state{feed = Feed,
             profile = Profile,
             contacts = Contacts} = State) ->
    mess_auth:put(Id, Auth),
    write_msg(Msg, Feed),
    %% arrival-order ref; the message body lives only in the feed's own log
    ingest_journal:append(Auth, Seq),
    utils:update_refs(Msg),
    case social_msg:is_about(Msg) of
        true -> write_msg(Msg, Profile);
        _    -> ok
    end,
    %% The contacts file holds all contact messages — follows and blocks —
    %% so friends can lazily load both graphs from it on first query.
    case social_msg:is_follow(Msg) =/= nope
         orelse social_msg:is_block(Msg) =/= nope of
        true  -> write_msg(Msg, Contacts);
        false -> ok
    end,
    social_msg:dispatch(Msg),
    view_manager:ingest(Msg),
    State#state{last_msg = Id, last_seq = Seq}.

write_msg(#message{} = DecMsg, Store) ->
    Msg = message:encode(DecMsg),
    write_msg(Msg, Store);

%% On-disk frame: <<Len:32, Msg:Len/binary, Len:32, NextOffset:32>>
%% Trailing Len enables backward seek to find the last record.
%% NextOffset is the absolute file position of the following record's Len field,
%% used by scan/3 to step forward without re-reading the leading length.
write_msg(Msg, Store) ->
    DataSiz = size(Msg),
    O = open_file(Store),
    ok = file:write(O,
               <<DataSiz:32, Msg/binary, DataSiz:32>>),
    FileSize = filelib:file_size(Store) + 4,
    ok = file:write(O, <<FileSize:32>>),
    close_file(O).

init_directories(FeedId) ->
    FeedDir = utils:feed_dir(FeedId),
    Feed = <<FeedDir/binary,~"/"/binary,~"log.offset"/binary>>,
    Profile = <<FeedDir/binary,~"/"/binary,~"profile"/binary>>,
    Contacts = <<FeedDir/binary,~"/"/binary,~"contacts"/binary>>,
    Refs = <<FeedDir/binary,~"/"/binary,~"references"/binary>>,
    filelib:ensure_dir(Feed),
    filelib:ensure_dir(Profile),
    filelib:ensure_dir(Contacts),
    filelib:ensure_dir(Refs),
    {Feed, Profile, Contacts, Refs}.

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

feed_get(Feed, [], Key) ->
    feed_get(Feed, [{Key, 0}], Key);

feed_get(Feed, [{Key, Pos}], Key) ->
    try
        {ok, IoDev} = file:open(Feed, [read, binary]),
        file:position(IoDev, Pos),
        Data = scan(IoDev, Pos, Key),
        file:close(IoDev),
        Data
    catch
        {error, enoent} ->
            ?LOG_INFO("Probably bad input ~n",[]),
            done
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

scan(IoDev, Pos, Key) ->
    case load_term(IoDev) of
        {ok, Data} ->
            KeyVal = extract_key(Data),
            if KeyVal == Key ->
                    {Pos, Data};
               true ->
                    {ok, <<NextPos:32/integer>>} = file:read(IoDev, 4),
                    scan(IoDev, NextPos, Key)
            end;
        {error, eof} ->
            ?LOG_INFO("Key not found: ~p ~n",[Key]),
            not_found;
        {error, Error} ->
            ?LOG_INFO("Error ~p scanning for key: ~p ~n",[Error, Key])
    end.


has_target(Msg, Id, RootId) ->
    {DecProps} = utils:nat_decode(Msg),
    Root = ?pgv(~"root", DecProps),
    IsRootId = RootId == Root,
    [Src, _AuthId] = ?pgv(~"src", DecProps),
    case IsRootId of
        true ->
            if Src == Id ->
                    ?pgv(~"tar", DecProps);
               true ->
                    false
            end;
        false ->
            false
    end.

open_file(File) ->
    %% NOTE: do NOT use the `sync` flag here. It forces an fsync on every
    %% file:write, and a stored message can hit several files (per-feed
    %% log plus profile/contacts) — i.e.
    %% several fsyncs per stored message. On Linux that is ~60ms each, which
    %% throttled EBT replication to ~4 msgs/sec and left peers stuck in
    %% "Downloading new messages"/"Scuttling…" during a full-DB sync. Plain
    %% [append] still writes through to the OS (so filelib:file_size and the
    %% on-disk frame layout stay correct); the OS flushes lazily, and any
    %% messages lost in a crash are recovered by re-replication.
    Open = file:open(File, [append]),
    case Open of
        {ok, F} ->
            F;
        Else ->
            ?LOG_INFO("Tried to open failed: ~p ~n",[Else]),
            nil
    end.

close_file(File) ->
    ok = file:close(File).

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
      fun archive_manual_test/1,
      fun post_after_archive_test/1,
      fun second_archive_naming_test/1,
      fun restart_then_archive_naming_test/1,
      fun crash_window_recovery_test/1]}.

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
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    FeedId = keys:pub_key_disp(),
    {ok, Pid} = ssb_feed:start_link(FeedId),
    {Pid, FeedId, Home}.

teardown(ignore) ->
    [catch gen_server:stop(Name)
     || Name <- [blobs, mess_auth, keys, config]],
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
