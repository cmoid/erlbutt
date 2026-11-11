%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Is it safe to publish on our own feed?  The fork guard.
%%
%% A feed forks when its author signs two different messages at one
%% sequence.  The node signs at last_seq + 1 of whatever its local log
%% holds, so publishing forks the feed whenever the log is BEHIND the
%% feed's real head: a secret restored onto an empty or old store, a
%% store rebuilt and only partly replicated, the same secret used on a
%% second device, a client that switches identities over one store,
%% or our own unsynced tail lost to a power cut after peers
%% already had it.  Nothing in the log itself can tell us; only two
%% things can:
%%
%%   The own-head record (.ssberl/own_heads).  Per identity, the highest
%%   sequence this store has held and whether a peer has ever confirmed
%%   it.  Written on every own message, outside the feed log.  A log
%%   BELOW its record has lost messages — known with no peer at all.
%%
%%   Peers' EBT notes.  Every note for our own feed carries the peer's
%%   sequence for it.  A peer AHEAD of us means the feed went on without
%%   this store.  This works even against ssb-ebt peers whose cached
%%   per-peer clocks stop them re-sending (their `max()` never lowers
%%   what they believe we hold): the note is about THEIR copy, not ours.
%%
%% From those, one of three states:
%%
%%   confident    the record is confirmed and the log reaches it.
%%                Publish.  Offline is fine — the normal boot.
%%   unconfirmed  no record for this identity, or one no peer has ever
%%                confirmed.  Publishing is allowed but logged: a
%%                restored secret and a brand-new identity look exactly
%%                alike here (empty feed, no record), and a key minted
%%                by a client rather than by this node has nothing to
%%                tell us which it is.  The first peer that replicates
%%                us and reports our feed at or below our sequence
%%                confirms it.  (Refusing here, with an owner-only
%%                override a client calls right after minting a key, is
%%                the stricter option; it needs that client's help.)
%%   behind       a peer's note is ahead of us, or the log is below its
%%                record.  Publishing is REFUSED, and our own feed is
%%                pulled from every connected peer with
%%                createHistoryStream — an explicit pull from a sequence,
%%                which no peer's cached EBT state can stall — until the
%%                log reaches the target.
%%
%% Nothing here makes a synchronous call into a feed process: feeds call
%% in (casts and ETS reads only), so the feed that asks "may I publish?"
%% can never deadlock against a guard waiting on it.
-module(own_feed).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("ssb/include/ssb.hrl").

-export([start_link/0,
         status/0,
         may_publish/0,
         stored/2,
         peer_note/1,
         seed_record/1,
         pull_now/0,
         last_pull/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         handle_info/2, terminate/2, code_change/3]).

-export([boot_state/2, note_state/3, seq_state/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).
-define(RECORD, "own_heads").
%% While behind, the pull is retried this often, in case no peer was
%% connected (or none was ahead) the last time.
-define(PULL_RETRY_MS, 30_000).
%% Messages asked for per createHistoryStream call.
-define(PULL_LIMIT, 500).

-record(st, {self, record, pulling = false}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% {State, OurSeq, Target} — State one of confident | unconfirmed |
%% behind | not_running.
status() ->
    case lookup(status) of
        undefined -> {not_running, 0, 0};
        S         -> {S, lookup(ours), lookup(target)}
    end.

%% The guard, read by the own feed before it signs.  ETS only.
may_publish() ->
    case status() of
        {behind, Ours, Target} ->
            {error, {own_feed_behind, Ours, Target}};
        {unconfirmed, Ours, _} ->
            warn_unconfirmed(Ours),
            ok;
        _ ->
            ok
    end.

%% The own feed stored a message of ours — posted here, or recovered
%% from a peer.  Called from the feed process; never blocks.
stored(FeedId, Seq) ->
    case whereis(?SERVER) of
        undefined -> ok;
        Pid       -> gen_server:cast(Pid, {stored, FeedId, Seq})
    end.

%% A peer's note for our own feed, as {Replicating, PeerSeq}.  From EBT.
peer_note({false, _Seq}) ->
    ok;
peer_note({true, PeerSeq}) when is_integer(PeerSeq) ->
    case whereis(?SERVER) of
        undefined -> ok;
        Pid       -> gen_server:cast(Pid, {peer_note, PeerSeq})
    end.

%% A key this node has just minted is a new feed by construction: there
%% is no history anywhere to be behind.  Record it as confirmed at 0.
%% Called by keys before this server runs, so it writes the file itself.
seed_record(FeedId) ->
    Rec = read_record(),
    write_record(Rec#{FeedId => #{seq => 0, confirmed => true}}).

%% Tests and operators: pull our own feed from connected peers now.
pull_now() ->
    gen_server:cast(?SERVER, pull).

%% What the most recent pull did: #{peers, stored}, or undefined.
last_pull() ->
    lookup(last_pull).

%%%===================================================================
%%% The decisions — pure, so they read (and test) as policy
%%%===================================================================

%% At boot: what the record and the log say before any peer has spoken.
%% -> {State, Target}
boot_state(undefined, _LogSeq) ->
    {unconfirmed, 0};
boot_state(#{seq := RecSeq}, LogSeq) when LogSeq < RecSeq ->
    {behind, RecSeq};
boot_state(#{confirmed := true}, LogSeq) ->
    {confident, LogSeq};
boot_state(#{}, LogSeq) ->
    {unconfirmed, LogSeq}.

%% A peer reported PeerSeq for our feed while we held Ours.
%% -> {NewState, NewTarget}
note_state(_State, {Ours, Target}, PeerSeq) when PeerSeq > Ours ->
    {behind, max(Target, PeerSeq)};
note_state(behind, {_Ours, Target}, _PeerSeq) ->
    {behind, Target};
note_state(_State, {Ours, _Target}, _PeerSeq) ->
    %% at or below us: nothing went on without us, as far as this peer
    %% knows — which is exactly what "confirmed" means
    {confident, Ours}.

%% Our own log reached Seq.  -> {NewState, NewTarget}
seq_state(behind, Seq, Target) when Seq >= Target ->
    {confident, Seq};
seq_state(State, Seq, Target) ->
    {State, max(Seq, Target)}.

%%%===================================================================
%%% gen_server
%%%===================================================================

init([]) ->
    ets:new(?TAB, [set, named_table, public, {read_concurrency, true}]),
    Self = keys:pub_key_disp(),
    {ok, #st{self = Self, record = read_record()}, {continue, boot}}.

%% The log's sequence comes from the feed process.  That call is safe:
%% a feed only ever casts to this server, so nothing it is doing can be
%% waiting on us.
handle_continue(boot, #st{self = Self, record = Rec} = St) ->
    LogSeq = ssb_feed:current_seq(utils:find_or_create_feed_pid(Self)),
    Mine = maps:get(Self, Rec, undefined),
    {State, Target} = boot_state(Mine, LogSeq),
    ets:insert(?TAB, [{status, State}, {ours, LogSeq}, {target, Target}]),
    log_state(boot, State, LogSeq, Target),
    {noreply, after_change(State, St)}.

handle_call(_Req, _From, St) ->
    {reply, {error, unknown_call}, St}.

handle_cast({stored, FeedId, Seq}, #st{self = FeedId, record = Rec} = St) ->
    Old = lookup(status),
    {State, Target} = seq_state(Old, Seq, lookup(target)),
    ets:insert(?TAB, [{status, State}, {ours, Seq}, {target, Target}]),
    Prev = maps:get(FeedId, Rec, #{seq => 0, confirmed => false}),
    %% Catching up to a target a peer set is itself a confirmation.
    Confirmed = maps:get(confirmed, Prev) orelse (Old =:= behind andalso State =:= confident),
    Rec1 = Rec#{FeedId => #{seq => max(Seq, maps:get(seq, Prev)),
                            confirmed => Confirmed}},
    ok = write_record(Rec1),
    State =/= Old andalso log_state(caught_up, State, Seq, Target),
    {noreply, after_change(State, St#st{record = Rec1})};
handle_cast({stored, _Other, _Seq}, St) ->
    {noreply, St};

handle_cast({peer_note, PeerSeq}, #st{self = Self, record = Rec} = St) ->
    Old = lookup(status),
    Ours = lookup(ours),
    {State, Target} = note_state(Old, {Ours, lookup(target)}, PeerSeq),
    ets:insert(?TAB, [{status, State}, {target, Target}]),
    St1 = case {Old, State} of
              {unconfirmed, confident} ->
                  Prev = maps:get(Self, Rec, #{seq => Ours}),
                  Rec1 = Rec#{Self => Prev#{seq => max(Ours, maps:get(seq, Prev)),
                                            confirmed => true}},
                  ok = write_record(Rec1),
                  St#st{record = Rec1};
              _ ->
                  St
          end,
    case State =/= Old orelse (State =:= behind andalso PeerSeq > Ours) of
        true  -> log_state({peer, PeerSeq}, State, Ours, Target);
        false -> ok
    end,
    {noreply, after_change(State, St1)};

handle_cast(pull, St) ->
    {noreply, start_pull(St)};

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info(retry_pull, St) ->
    {noreply, after_change(lookup(status), St)};
handle_info({pull_done, Result}, St) ->
    ?SSB_INFO("own_feed: pull finished: ~p; status ~p", [Result, status()]),
    ets:insert(?TAB, {last_pull, Result}),
    St1 = St#st{pulling = false},
    case lookup(status) of
        behind -> erlang:send_after(?PULL_RETRY_MS, self(), retry_pull);
        _      -> ok
    end,
    {noreply, St1};
handle_info(_Info, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%%%===================================================================
%%% Recovery: pull our own feed
%%%===================================================================

after_change(behind, St) -> start_pull(St);
after_change(_State, St) -> St.

start_pull(#st{pulling = true} = St) ->
    St;
start_pull(#st{self = Self} = St) ->
    Server = self(),
    _ = spawn(fun() -> Server ! {pull_done, pull(Self)} end),
    St#st{pulling = true}.

%% Ask every connected peer for our feed after what we hold, storing
%% through the same signature + chain checks as any untrusted ingest.
%% Runs outside the gen_server: a slow peer blocks nothing.
pull(Self) ->
    Pid = utils:find_or_create_feed_pid(Self),
    Peers = [P || {_Key, P} <- peer_registry_all()],
    Stored = lists:sum([pull_from(Peer, Self, Pid, 0) || Peer <- Peers]),
    #{peers => length(Peers), stored => Stored}.

pull_from(Peer, Self, Pid, Acc) ->
    Ours = ssb_feed:current_seq(Pid),
    %% seq is exclusive on erlbutt and inclusive on ssb-db: asking from
    %% Ours costs ssb-db one duplicate (skipped) and misses nothing.
    Args = [{[{~"id", Self}, {~"seq", Ours}, {~"keys", true},
              {~"limit", ?PULL_LIMIT}]}],
    try ssb_peer:rpc_stream_call(Peer, [?createhistorystream], Args) of
        {ok, Frames} ->
            N = length([ok || F <- Frames, store_own(F, Self, Pid) =:= stored]),
            case N > 0 andalso length(Frames) >= ?PULL_LIMIT of
                true  -> pull_from(Peer, Self, Pid, Acc + N);
                false -> Acc + N
            end;
        _ ->
            Acc
    catch _:_ ->
        Acc
    end.

%% A frame is a {key, value, timestamp} envelope (keys: true).  The id is
%% derived from the value, and the signature must verify: this is our
%% own feed, and nothing unsigned goes into it whatever require_valid_sigs
%% says.
store_own(Frame, Self, Pid) ->
    try utils:nat_decode(Frame) of
        {Props} ->
            Value = case proplists:get_value(~"value", Props) of
                        {VProps} -> VProps;
                        _        -> Props
                    end,
            case message:from_value(Value, true) of
                #message{author = Self, validated = true} = Msg ->
                    ssb_feed:store_msg_checked(Pid, Msg);
                _ ->
                    skipped
            end;
        _ ->
            skipped
    catch _:_ ->
        skipped
    end.

peer_registry_all() ->
    try peer_registry:all() catch _:_ -> [] end.

%%%===================================================================
%%% The record
%%%===================================================================

record_file() ->
    ?b2l(config:ssb_repo_loc()) ++ ?RECORD.

%% #{FeedId => #{seq => N, confirmed => Bool}}.  Missing or unreadable is
%% an empty record — every identity unconfirmed, which only warns.
read_record() ->
    case file:consult(record_file()) of
        {ok, [Rec]} when is_map(Rec) -> Rec;
        _                            -> #{}
    end.

%% Temp file, fsync, rename: the record must never be half written, and
%% must reach the disk at least as surely as the log it vouches for — a
%% record AHEAD of a log that lost its tail is what detects the loss.
write_record(Rec) ->
    File = record_file(),
    Tmp = File ++ ".tmp",
    ok = file:write_file(Tmp, io_lib:format("~p.~n", [Rec]), [sync]),
    ok = file:rename(Tmp, File).

%%%===================================================================
%%% Helpers
%%%===================================================================

lookup(Key) ->
    try ets:lookup(?TAB, Key) of
        [{Key, V}] -> V;
        []         -> undefined
    catch error:badarg ->
        undefined
    end.

%% Once per run: the warning is about the state, not about every post.
warn_unconfirmed(Ours) ->
    case ets:insert_new(?TAB, {warned_unconfirmed, true}) of
        true ->
            ?LOG_WARNING("own_feed: publishing at seq ~p before any peer has "
                         "confirmed this feed's head; if this secret has "
                         "history elsewhere, this post forks it",
                         [Ours + 1]);
        false ->
            ok
    end.

log_state(Why, behind, Ours, Target) ->
    ?LOG_WARNING("own_feed: ~p: BEHIND — we hold ~p, the feed reaches ~p; "
                 "publishing refused until caught up", [Why, Ours, Target]);
log_state(Why, State, Ours, _Target) ->
    ?SSB_INFO("own_feed: ~p: ~p at seq ~p", [Why, State, Ours]).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

boot_state_test() ->
    %% never recorded: a restored secret and a new key look the same
    ?assertEqual({unconfirmed, 0}, boot_state(undefined, 0)),
    ?assertEqual({unconfirmed, 0}, boot_state(undefined, 7)),
    %% the normal boot
    ?assertEqual({confident, 7}, boot_state(#{seq => 7, confirmed => true}, 7)),
    ?assertEqual({confident, 9}, boot_state(#{seq => 7, confirmed => true}, 9)),
    %% recorded but never confirmed by a peer
    ?assertEqual({unconfirmed, 7}, boot_state(#{seq => 7, confirmed => false}, 7)),
    %% the log lost messages the record saw — confirmed or not
    ?assertEqual({behind, 7}, boot_state(#{seq => 7, confirmed => true}, 5)),
    ?assertEqual({behind, 7}, boot_state(#{seq => 7, confirmed => false}, 0)).

note_state_test() ->
    %% a peer ahead puts any state behind, and raises the target
    [?assertEqual({behind, 9}, note_state(S, {5, 5}, 9))
     || S <- [confident, unconfirmed, behind]],
    ?assertEqual({behind, 12}, note_state(behind, {5, 12}, 9)),
    %% at or below us confirms
    ?assertEqual({confident, 5}, note_state(unconfirmed, {5, 5}, 5)),
    ?assertEqual({confident, 5}, note_state(unconfirmed, {5, 5}, 0)),
    ?assertEqual({confident, 5}, note_state(confident, {5, 5}, 3)),
    %% but cannot clear behind: someone else still says we are short
    ?assertEqual({behind, 9}, note_state(behind, {5, 9}, 5)).

seq_state_test() ->
    ?assertEqual({behind, 9},    seq_state(behind, 6, 9)),
    ?assertEqual({confident, 9}, seq_state(behind, 9, 9)),
    ?assertEqual({confident, 6}, seq_state(confident, 6, 5)),
    ?assertEqual({unconfirmed, 6}, seq_state(unconfirmed, 6, 0)).

%% The record survives a restart and is written atomically.
record_round_trip_test() ->
    Home = filename:join("/tmp", "own_feed_" ++
                             integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    try
        ok = filelib:ensure_dir(record_file()),
        ?assertEqual(#{}, read_record()),
        ok = seed_record(~"@a.ed25519"),
        ok = write_record((read_record())#{~"@b.ed25519" => #{seq => 4, confirmed => false}}),
        ?assertEqual(#{~"@a.ed25519" => #{seq => 0, confirmed => true},
                       ~"@b.ed25519" => #{seq => 4, confirmed => false}},
                     read_record()),
        ?assertNot(filelib:is_file(record_file() ++ ".tmp")),
        %% garbage is an empty record, not a crash
        ok = file:write_file(record_file(), ~"not a term"),
        ?assertEqual(#{}, read_record())
    after
        catch gen_server:stop(config),
        application:unset_env(ssb, ssb_home),
        os:cmd("rm -rf " ++ Home)
    end.

-endif.
