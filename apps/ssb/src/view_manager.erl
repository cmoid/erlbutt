%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Lifecycle manager for ssb_view materialized views (the flume role):
%% per-view/per-feed sequence checkpoints, catch-up replay at
%% registration, full rebuild when a view's version bumps, synchronous
%% fan-out of every stored message, and change-event publication.
%%
%% Replay and rebuild fold the per-feed logs (archived .gz segments in
%% sequence order, then the live log.offset of each feed) — the real
%% store.  The global log.offset is NOT used: it predates nothing and
%% guarantees nothing (in practice it holds only a fraction of the
%% per-feed history), and it is slated for retirement.  Feeds are
%% folded independently; per-author order is all a view may rely on,
%% which is sufficient for SSB content semantics (only my messages
%% assert my follows/abouts).  Views needing arrival order across
%% feeds will use the future ingest journal (doc/plugin-architecture.md).
%%
%% Events: a view's view_entry/1 may return {events, [Event]}; each is
%% published as {view_event, ViewMod, Event} to processes that joined
%% via subscribe/1 (a pg group in the ssb_views scope, whose process is
%% started here and lives under this server).
-module(view_manager).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

%% API
-export([start_link/0,
         register_view/1,
         rebuild/1,
         ingest/1,
         subscribe/1,
         unsubscribe/1,
         checkpoint/2,
         save/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CKPT, ssb_view_checkpoints).
-define(PG_SCOPE, ssb_views).
-define(SAVE_EVERY_MS, 60_000).

-record(vm_state, {views = []}).   %% registered view modules, oldest first

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Register a view module.  If its stored version matches
%% Mod:view_version() and Mod:view_load() reports surviving state, the
%% view is caught up from its checkpoints; otherwise it is reset and
%% rebuilt from the whole log.  Synchronous: when this returns, the
%% view is current.
register_view(Mod) when is_atom(Mod) ->
    gen_server:call(?SERVER, {register_view, Mod}, infinity).

%% Wipe a registered view's derived state and refold it from the whole
%% log — the recovery hammer for a corrupted or suspect index.
rebuild(Mod) when is_atom(Mod) ->
    gen_server:call(?SERVER, {rebuild, Mod}, infinity).

%% Fold one just-stored message into every registered view.  Called
%% synchronously from ssb_feed:store/2 so views are current when the
%% store returns (the same contract friends:update/3 used to have).
%% A no-op when the manager is not running.
ingest(#message{} = Msg) ->
    try gen_server:call(?SERVER, {ingest, Msg}, infinity)
    catch exit:{noproc, _} -> ok
    end.

%% Receive {view_event, ViewMod, Event} messages for a view's changes.
subscribe(ViewMod) ->
    ok = pg:join(?PG_SCOPE, {view, ViewMod}, self()).

unsubscribe(ViewMod) ->
    ok = pg:leave(?PG_SCOPE, {view, ViewMod}, self()).

%% The highest sequence of FeedId delivered to ViewMod (0 if none).
%% Reads the protected checkpoint table directly.
checkpoint(ViewMod, FeedId) ->
    try ets:lookup(?CKPT, {ViewMod, feed, FeedId}) of
        [{_, Seq}] -> Seq;
        []         -> 0
    catch error:badarg -> 0
    end.

%% Flush every view's durable state and the checkpoint table to disk.
save() ->
    gen_server:call(?SERVER, save, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    %% The pg scope is started with the first manager; it survives a
    %% manager restart (normal exits do not propagate through the link).
    case pg:start_link(?PG_SCOPE) of
        {ok, _}                        -> ok;
        {error, {already_started, _}}  -> ok
    end,
    case ets:file2tab(?b2l(ckpt_file())) of
        {ok, ?CKPT} -> ok;
        _ -> ets:new(?CKPT, [named_table, protected, set,
                             {read_concurrency, true}])
    end,
    erlang:send_after(?SAVE_EVERY_MS, self(), save_tick),
    {ok, #vm_state{}}.

handle_call({register_view, Mod}, _From, #vm_state{views = Views} = State) ->
    case lists:member(Mod, Views) of
        true ->
            {reply, ok, State};
        false ->
            StoredV = stored_version(Mod),
            CodeV   = Mod:view_version(),
            case StoredV =:= CodeV andalso Mod:view_load() =:= ok of
                true ->
                    catch_up(Mod);
                false ->
                    ?SSB_INFO("view_manager: rebuilding ~p (stored ~p, code ~p)",
                              [Mod, StoredV, CodeV]),
                    rebuild_view(Mod)
            end,
            {reply, ok, State#vm_state{views = Views ++ [Mod]}}
    end;

handle_call({rebuild, Mod}, _From, #vm_state{views = Views} = State) ->
    case lists:member(Mod, Views) of
        true  -> {reply, rebuild_view(Mod), State};
        false -> {reply, {error, not_registered}, State}
    end;

handle_call({ingest, Msg}, _From, #vm_state{views = Views} = State) ->
    [deliver(Mod, Msg) || Mod <- Views],
    {reply, ok, State};

handle_call(save, _From, #vm_state{views = Views} = State) ->
    save_all(Views),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(save_tick, #vm_state{views = Views} = State) ->
    save_all(Views),
    erlang:send_after(?SAVE_EVERY_MS, self(), save_tick),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #vm_state{views = Views}) ->
    save_all(Views),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Reset a view and refold it from the start of the log, then snapshot.
rebuild_view(Mod) ->
    ok = Mod:view_reset(),
    ets:match_delete(?CKPT, {{Mod, feed, '_'}, '_'}),
    ets:insert(?CKPT, {{Mod, version}, Mod:view_version()}),
    catch_up(Mod),
    ok = Mod:view_save(),
    persist_ckpt(),
    ok.

%% Deliver every stored message past Mod's checkpoints by folding each
%% feed's own store: archived segments oldest-first, then the live log.
%% Within a feed the fold is in sequence order, which deliver/2 relies
%% on (a delivered Seq advances the checkpoint past everything below it).
catch_up(Mod) ->
    Start = erlang:monotonic_time(millisecond),
    N = lists:foldl(fun(Dir, Acc) -> Acc + catch_up_feed(Mod, Dir) end,
                    0, feed_dirs()),
    ?SSB_INFO("view_manager: folded ~p messages into ~p in ~p ms",
              [N, Mod, erlang:monotonic_time(millisecond) - Start]),
    ok.

feed_dirs() ->
    Loc = ?b2l(config:feed_loc()),
    [D || D <- filelib:wildcard(filename:join(Loc, "*/*")),
          filelib:is_dir(D)].

catch_up_feed(Mod, Dir) ->
    Live = filename:join(Dir, "log.offset"),
    N = lists:foldl(fun(Gz, Acc) -> Acc + fold_archive(Mod, Gz) end,
                    0, archive_segments(Dir)),
    N + utils:fold_log_file(fun(Data, Acc) -> Acc + deliver_raw(Mod, Data) end,
                            0, ?l2b(Live)).

%% Archived segment files, oldest first.  Names are
%% log.offset.<From>-<To>.gz; sort numerically on From (lexicographic
%% ordering breaks once sequence numbers gain a digit).
archive_segments(Dir) ->
    Segs = filelib:wildcard(filename:join(Dir, "log.offset.*.gz")),
    Numbered = [{archive_from(S), S} || S <- Segs],
    [S || {_From, S} <- lists:sort(Numbered)].

archive_from(Path) ->
    ["gz", Range | _] = lists:reverse(string:split(filename:basename(Path), ".", all)),
    [From | _] = string:split(Range, "-"),
    list_to_integer(From).

%% An archive is the gzipped raw log file: the same
%% <<Len:32, Msg:Len, Len:32, NextOffset:32>> framing, iterated in memory.
fold_archive(Mod, GzFile) ->
    try
        {ok, GzData} = file:read_file(GzFile),
        fold_log_bin(Mod, zlib:gunzip(GzData), 0)
    catch C:R ->
            ?SSB_ERROR("view_manager: unreadable archive ~s: ~p:~p",
                       [GzFile, C, R]),
            0
    end.

fold_log_bin(Mod, <<Len:32, Msg:Len/binary, Len:32, _Next:32, Rest/binary>>, N) ->
    fold_log_bin(Mod, Rest, N + deliver_raw(Mod, Msg));
fold_log_bin(_Mod, _Rest, N) ->
    N.

deliver_raw(Mod, Data) ->
    try message:decode(Data, false) of
        #message{} = Msg -> deliver(Mod, Msg)
    catch _:_ -> 0
    end.

%% Deliver Msg to Mod if it is beyond Mod's checkpoint for the feed;
%% advance the checkpoint and publish any events the view emits.
%% Returns 1 when the message was folded, 0 when it was already covered.
deliver(Mod, #message{author = FeedId, sequence = Seq} = Msg) ->
    case checkpoint(Mod, FeedId) of
        Ckpt when Seq > Ckpt ->
            Res = try Mod:view_entry(Msg)
                  catch C:R:Stack ->
                          ?SSB_ERROR("view ~p crashed on ~p seq ~p: ~p:~p ~p",
                                     [Mod, FeedId, Seq, C, R, Stack]),
                          ok
                  end,
            ets:insert(?CKPT, {{Mod, feed, FeedId}, Seq}),
            case Res of
                {events, Events} -> publish(Mod, Events);
                _                -> ok
            end,
            1;
        _ ->
            0
    end.

publish(Mod, Events) ->
    Members = pg:get_members(?PG_SCOPE, {view, Mod}),
    [Pid ! {view_event, Mod, Event} || Pid <- Members, Event <- Events],
    ok.

save_all(Views) ->
    [try Mod:view_save()
     catch C:R ->
             %% Routine at shutdown: views stop before this manager
             %% (reverse start order) and snapshot themselves in their
             %% own terminate; their tables are already gone here.
             ?SSB_DEBUG("view ~p save skipped: ~p:~p", [Mod, C, R])
     end || Mod <- Views],
    persist_ckpt().

persist_ckpt() ->
    %% config may already be down during shutdown teardown; losing one
    %% checkpoint flush is safe (worst case the view replays messages it
    %% has already folded — folds are idempotent per {feed, seq}).
    try
        File = ckpt_file(),
        filelib:ensure_dir(File),
        ok = ets:tab2file(?CKPT, ?b2l(File))
    catch C:R ->
            ?SSB_ERROR("view_manager: checkpoint flush failed: ~p:~p", [C, R])
    end.

ckpt_file() ->
    <<(config:ssb_repo_loc())/binary, "views/checkpoints.tab">>.

stored_version(Mod) ->
    case ets:lookup(?CKPT, {Mod, version}) of
        [{_, V}] -> V;
        []       -> undefined
    end.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

vm_test_() ->
    {setup, fun vm_setup/0, fun vm_teardown/1,
     fun(_) ->
             [?_test(ingest_and_checkpoint()),
              ?_test(events_to_subscriber()),
              ?_test(catch_up_after_restart()),
              ?_test(rebuild_on_version_bump()),
              ?_test(rebuild_without_global_log()),
              ?_test(rebuild_folds_archives())]
     end}.

vm_setup() ->
    lists:foreach(
      fun({Name, StartFun}) ->
              case whereis(Name) of
                  undefined -> {ok, _} = StartFun(), ok;
                  _         -> ok
              end
      end,
      [{config,       fun() -> config:start_link("test/ssb.cfg") end},
       {keys,         fun() -> keys:start_link() end},
       {mess_auth,    fun() -> mess_auth:start_link() end},
       {blobs,        fun() -> blobs:start_link() end},
       {ssb_feed_sup, fun() -> ssb_feed_sup:start_link() end},
       {view_manager, fun() -> view_manager:start_link() end}]).

vm_teardown(_) ->
    application:unset_env(ssb, test_view_version),
    [catch gen_server:stop(Name)
     || Name <- [view_manager, ssb_feed_sup, blobs, mess_auth, keys, config]],
    ok.

%% eunit may run each test in its own process; a manager started (and
%% linked) inside an earlier test dies with it, so every test begins by
%% making sure one is running.
vm_ensure_manager() ->
    case whereis(view_manager) of
        undefined -> {ok, _} = view_manager:start_link(), ok;
        _         -> ok
    end.

vm_restart_manager() ->
    catch gen_server:stop(view_manager),
    {ok, _} = view_manager:start_link(),
    ok.

vm_make_peer() ->
    #{public := Pub, secret := Priv} = enacl:sign_keypair(),
    Id = <<"@", (base64:encode(Pub))/binary, ".ed25519">>,
    {utils:find_or_create_feed_pid(Id), Id, base64:encode(Priv)}.

vm_store_post(FeedPid, AuthId, AuthPriv, Prev, Seq) ->
    Content = {[{~"type", ~"post"}, {~"text", ~"view manager test"}]},
    Msg = message:new_msg(Prev, Seq, Content, {AuthId, AuthPriv}),
    _ = ssb_feed:store_msg(FeedPid, Msg),
    ssb_feed:fetch_last_msg(FeedPid).

ingest_and_checkpoint() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    {Pid, Id, Priv} = vm_make_peer(),
    ?assertEqual(0, checkpoint(test_counter_view, Id)),
    #message{id = M1} = vm_store_post(Pid, Id, Priv, null, 1),
    #message{}        = vm_store_post(Pid, Id, Priv, M1, 2),
    ?assertEqual([1, 2], test_counter_view:entries(Id)),
    ?assertEqual(2, checkpoint(test_counter_view, Id)).

events_to_subscriber() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    ok = subscribe(test_counter_view),
    {Pid, Id, Priv} = vm_make_peer(),
    #message{} = vm_store_post(Pid, Id, Priv, null, 1),
    receive
        {view_event, test_counter_view, {seen, Id, 1}} -> ok
    after 1000 ->
        error(no_view_event)
    end,
    ok = unsubscribe(test_counter_view).

%% Messages stored while the manager is down are delivered on
%% re-registration (catch-up from checkpoints, not a full rebuild).
catch_up_after_restart() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    {Pid, Id, Priv} = vm_make_peer(),
    #message{id = M1} = vm_store_post(Pid, Id, Priv, null, 1),
    ?assertEqual([1], test_counter_view:entries(Id)),
    ok = gen_server:stop(view_manager),         %% saves views + checkpoints
    #message{} = vm_store_post(Pid, Id, Priv, M1, 2),  %% ingest is a no-op
    ?assertEqual([1], test_counter_view:entries(Id)),
    {ok, _} = view_manager:start_link(),
    ok = register_view(test_counter_view),
    ?assertEqual([1, 2], test_counter_view:entries(Id)),
    ?assertEqual(2, checkpoint(test_counter_view, Id)).

%% Rebuild reads the per-feed logs, not the global log.offset: wiping
%% the global log must lose nothing.  (Regression: the global log holds
%% only a fraction of the per-feed history on converted nodes, which
%% left the friends view nearly empty after its first rebuild.)
rebuild_without_global_log() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    {Pid, Id, Priv} = vm_make_peer(),
    #message{id = M1} = vm_store_post(Pid, Id, Priv, null, 1),
    #message{}        = vm_store_post(Pid, Id, Priv, M1, 2),
    ok = file:delete(?b2l(<<(config:ssb_repo_loc())/binary, "log.offset">>)),
    ok = rebuild(test_counter_view),
    ?assertEqual([1, 2], test_counter_view:entries(Id)).

%% Archived segments are folded too: after archiving, a rebuild sees the
%% feed's full history with no sequence gaps.
rebuild_folds_archives() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    ok = ssb_feed:post_content(OwnPid, ~"before archive one"),
    ok = ssb_feed:post_content(OwnPid, ~"before archive two"),
    _ = ssb_feed:archive(OwnPid),   %% gzips the live log, posts an archive msg
    ok = ssb_feed:post_content(OwnPid, ~"after archive"),
    #message{sequence = Last} = ssb_feed:fetch_last_msg(OwnPid),
    ok = rebuild(test_counter_view),
    ?assertEqual(lists:seq(1, Last), test_counter_view:entries(OwnId)).

%% A version bump forces reset + refold of the whole log.
rebuild_on_version_bump() ->
    ok = test_counter_view:ensure_table(),
    ok = vm_ensure_manager(),
    ok = register_view(test_counter_view),
    {Pid, Id, Priv} = vm_make_peer(),
    #message{id = M1} = vm_store_post(Pid, Id, Priv, null, 1),
    #message{}        = vm_store_post(Pid, Id, Priv, M1, 2),
    ?assertEqual([1, 2], test_counter_view:entries(Id)),
    application:set_env(ssb, test_view_version, 2),
    ok = vm_restart_manager(),
    ok = register_view(test_counter_view),
    %% state wiped and refolded from the log, exactly once per message
    ?assertEqual([1, 2], test_counter_view:entries(Id)),
    ?assertEqual(2, checkpoint(test_counter_view, Id)).

-endif.
