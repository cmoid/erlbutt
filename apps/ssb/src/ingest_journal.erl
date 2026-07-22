%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% The ingest journal: an append-only file of {FeedId, Seq} refs, one
%% per stored message, recording arrival order across all feeds.  It
%% replaces the global log.offset (which duplicated every message body
%% and, on converted nodes, silently held only a fraction of them).
%% Consumers that need "everything, in the order it arrived" — the
%% createLogStream RPC, future timeline views — read the journal and
%% resolve bodies from the per-feed store; consumers that only need
%% "everything" fold the per-feed store directly (feed_store:fold_all).
%%
%% Records use the standard <<Len:32, Data, Len:32, NextOffset:32>>
%% framing (utils:fold_log_file reads it); Data is
%% term_to_binary({FeedId, Seq}).
%%
%% On first start with existing feeds (no journal file), the journal is
%% backfilled feed by feed: pre-journal history gets per-feed sequence
%% order, not true arrival order — the best available approximation,
%% same as any conversion.
-module(ingest_journal).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").
-compile({no_auto_import, [size/1]}).
-import(utils, [size/1]).

%% API
-export([start_link/0,
         append/2,
         fold_refs/2,
         stream_messages/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(ij_state, {iodev, offset}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Record the arrival of {FeedId, Seq}.  Called from ssb_feed:store/2
%% right after the per-feed write; synchronous so the journal order is
%% the store order.  A no-op when the journal is not running (bare
%% eunit setups).
append(FeedId, Seq) when is_binary(FeedId), is_integer(Seq) ->
    try gen_server:call(?SERVER, {append, FeedId, Seq}, infinity)
    catch exit:{noproc, _} -> ok
    end.

%% Fold Fun({FeedId, Seq}, Acc) over every ref in arrival order.
fold_refs(Fun, Acc0) ->
    utils:fold_log_file(
      fun(Data, Acc) ->
              try Fun(binary_to_term(Data), Acc)
              catch _:_ -> Acc
              end
      end, Acc0, journal_file()).

%% Fold Fun(MsgBinary, Acc) over every stored message in arrival order.
%% Refs for one feed are monotone in Seq, so each feed is read with a
%% single sequential cursor (archives, then the live log) — the whole
%% stream is O(total bytes) with one open cursor per feed.  A cursor is
%% held as {Cursor, Pending} where Pending is a read-ahead record that
%% did not match its ref (a journal hole); it is offered to the next ref.
stream_messages(Fun, Acc0) ->
    {Cursors, _Warned, Acc} =
        fold_refs(
          fun({FeedId, Seq}, {Curs, Warned, AccIn}) ->
                  C0 = case Curs of
                           #{FeedId := C} -> C;
                           _ ->
                               try {feed_store:cursor_open(feed_dir_of(FeedId)),
                                    none}
                               catch _:_ -> {eof, none}
                               end
                       end,
                  case next_matching(C0, FeedId, Seq) of
                      {Msg, C1} when is_binary(Msg) ->
                          {Curs#{FeedId => C1}, Warned, Fun(Msg, AccIn)};
                      {skip, C1} ->
                          {Curs#{FeedId => C1}, warn_gap(FeedId, Seq, Warned),
                           AccIn}
                  end
          end, {#{}, #{}, Acc0}),
    [feed_store:cursor_close(C) || _ := {C, _} <- Cursors],
    Acc.

%% Log a journal/store gap at most ONCE per feed.  A drifted journal (e.g.
%% after a wipe/truncate that shrank the store without rebuilding the journal)
%% can have thousands of refs with no backing record; logging one error per ref
%% floods the logger hard enough to trip its overload protection and take the
%% node down.  One line per feed keeps the diagnostic without the flood.
warn_gap(FeedId, Seq, Warned) ->
    case Warned of
        #{FeedId := _} -> Warned;
        _ ->
            ?SSB_ERROR("ingest_journal: ~s has journal refs with no store "
                       "record (first gap at seq ~p; journal is ahead of the "
                       "feed store — rebuild ingest.journal). Further gaps for "
                       "this feed are suppressed.", [FeedId, Seq]),
            Warned#{FeedId => true}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    File = ?b2l(journal_file()),
    ok = filelib:ensure_dir(File),
    case filelib:is_file(File) of
        true  -> ok;
        false -> backfill(File)
    end,
    {ok, IoDev} = file:open(File, [append, binary]),
    {ok, #ij_state{iodev = IoDev, offset = filelib:file_size(File)}}.

handle_call({append, FeedId, Seq}, _From,
            #ij_state{iodev = IoDev, offset = Offset} = State) ->
    NewOffset = write_ref(IoDev, Offset, FeedId, Seq),
    {reply, ok, State#ij_state{offset = NewOffset}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #ij_state{iodev = IoDev}) ->
    catch file:close(IoDev),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

journal_file() ->
    <<(config:ssb_repo_loc())/binary, "ingest.journal">>.

%% Returns the offset after the written record (the next record's
%% position, also stored in the frame's NextOffset field).
write_ref(IoDev, Offset, FeedId, Seq) ->
    Data = term_to_binary({FeedId, Seq}),
    Len  = size(Data),
    NextOffset = Offset + 4 + Len + 4 + 4,
    ok = file:write(IoDev, <<Len:32, Data/binary, Len:32, NextOffset:32>>),
    NextOffset.

%% First start on a node with existing feeds: seed the journal from the
%% per-feed store, feed by feed.
backfill(File) ->
    Start = erlang:monotonic_time(millisecond),
    {ok, IoDev} = file:open(File, [append, binary]),
    {N, _} = feed_store:fold_all(
          fun(Data, {Count, Offset}) ->
                  try
                      #message{author = FeedId, sequence = Seq} =
                          message:decode(Data, false),
                      {Count + 1, write_ref(IoDev, Offset, FeedId, Seq)}
                  catch _:_ -> {Count, Offset}
                  end
          end, {0, 0}),
    ok = file:close(IoDev),
    ?SSB_INFO("ingest_journal: backfilled ~p refs in ~p ms",
              [N, erlang:monotonic_time(millisecond) - Start]),
    ok.

%% Advance a feed cursor to the record with sequence Seq.  Refs and
%% store records correspond 1:1 in order, so normally this is just the
%% next record (or the pending read-ahead).  A ref whose record is
%% missing is skipped with a warning; a record beyond the ref is kept
%% pending for the next ref rather than being consumed.
next_matching({Cursor, {PSeq, PMsg}}, _FeedId, Seq) when PSeq =:= Seq ->
    {PMsg, {Cursor, none}};
%% Read-ahead record is beyond this ref (a store hole): skip the ref, keep the
%% record pending for a later one.  The caller logs the gap once per feed.
next_matching({_Cursor, {PSeq, _PMsg}} = C, _FeedId, Seq) when PSeq > Seq ->
    {skip, C};
%% Store exhausted but the journal still has refs: skip.  Caller logs once.
next_matching({eof, _}, _FeedId, _Seq) ->
    {skip, {eof, none}};
next_matching({Cursor, _}, FeedId, Seq) ->
    case feed_store:cursor_next(Cursor) of
        eof ->
            next_matching({eof, none}, FeedId, Seq);
        {Msg, C1} ->
            try (message:decode(Msg, false))#message.sequence of
                Seq            -> {Msg, {C1, none}};
                S when S < Seq -> next_matching({C1, none}, FeedId, Seq);
                S              -> next_matching({C1, {S, Msg}}, FeedId, Seq)
            catch _:_ -> next_matching({C1, none}, FeedId, Seq)
            end
    end.

feed_dir_of(FeedId) ->
    ?b2l(utils:feed_dir(FeedId)).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

ij_test_() ->
    {setup, fun ij_setup/0, fun ij_teardown/1,
     fun(_) ->
             [?_test(arrival_order()),
              ?_test(backfill_recreates_refs()),
              ?_test(stream_spans_archives())]
     end}.

%% Fully isolated home: these tests archive and restart stores, and a
%% home shared across eunit runs accumulates overlapping archives and
%% stale journals (found the hard way).
ij_setup() ->
    ij_teardown(ignore),
    Home = filename:join("/tmp", "ij_" ++
                          integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = ingest_journal:start_link(),
    Home.

ij_teardown(Home) ->
    [catch gen_server:stop(Name)
     || Name <- [ingest_journal, ssb_feed_sup, blobs, mess_auth, keys, config]],
    case Home of
        ignore -> ok;
        _ ->
            os:cmd("rm -rf " ++ Home),
            application:unset_env(ssb, ssb_home)
    end,
    ok.

%% eunit may run each test in its own process; a journal started (and
%% linked) inside an earlier test dies with it.
ij_ensure() ->
    case whereis(?SERVER) of
        undefined -> {ok, _} = start_link(), ok;
        _         -> ok
    end.

ij_make_peer() ->
    #{public := Pub, secret := Priv} = enacl:sign_keypair(),
    Id = <<"@", (base64:encode(Pub))/binary, ".ed25519">>,
    {utils:find_or_create_feed_pid(Id), Id, base64:encode(Priv)}.

ij_store(FeedPid, AuthId, AuthPriv, Prev, Seq) ->
    Content = {[{~"type", ~"post"}, {~"text", ~"journal test"}]},
    Msg = message:new_msg(Prev, Seq, Content, {AuthId, AuthPriv}),
    _ = ssb_feed:store_msg(FeedPid, Msg),
    ssb_feed:fetch_last_msg(FeedPid).

%% Streamed refs for the given feeds only, in stream order.
ij_stream_refs(FeedIds) ->
    lists:reverse(
      stream_messages(
        fun(Data, Acc) ->
                try
                    #message{author = A, sequence = S} =
                        message:decode(Data, false),
                    case lists:member(A, FeedIds) of
                        true  -> [{A, S} | Acc];
                        false -> Acc
                    end
                catch _:_ -> Acc
                end
        end, [])).

%% Interleaved stores across two feeds come back in arrival order.
arrival_order() ->
    ok = ij_ensure(),
    {PidA, IdA, PrivA} = ij_make_peer(),
    {PidB, IdB, PrivB} = ij_make_peer(),
    #message{id = A1} = ij_store(PidA, IdA, PrivA, null, 1),
    #message{}        = ij_store(PidB, IdB, PrivB, null, 1),
    #message{}        = ij_store(PidA, IdA, PrivA, A1, 2),
    ?assertEqual([{IdA, 1}, {IdB, 1}, {IdA, 2}],
                 ij_stream_refs([IdA, IdB])).

%% Deleting the journal and restarting recreates refs from the per-feed
%% store (feed-major, so per-feed order is preserved even though
%% cross-feed arrival order is approximated).
backfill_recreates_refs() ->
    ok = ij_ensure(),
    {Pid, Id, Priv} = ij_make_peer(),
    #message{id = M1} = ij_store(Pid, Id, Priv, null, 1),
    #message{}        = ij_store(Pid, Id, Priv, M1, 2),
    catch gen_server:stop(?SERVER),
    ok = file:delete(?b2l(journal_file())),
    {ok, _} = start_link(),          %% init backfills
    ?assertEqual([{Id, 1}, {Id, 2}], ij_stream_refs([Id])).

%% Archived segments are resolved by the stream: after archiving, the
%% feed's full sequence range comes back with no gaps.
stream_spans_archives() ->
    ok = ij_ensure(),
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    ok = ssb_feed:post_content(OwnPid, ~"journal pre-archive"),
    ok = ssb_feed:post_content(OwnPid, ~"journal pre-archive two"),
    _ = ssb_feed:archive(OwnPid),
    ok = ssb_feed:post_content(OwnPid, ~"journal post-archive"),
    #message{sequence = Last} = ssb_feed:fetch_last_msg(OwnPid),
    Seqs = [S || {_A, S} <- ij_stream_refs([OwnId])],
    ?assertEqual(lists:seq(1, Last), Seqs).

-endif.
