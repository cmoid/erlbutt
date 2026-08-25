%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Asks connected peers where feeds can be started, and adopts validation
%% floors from what they answer.
%%
%% A FULL ROUND, NOT A CONNECT HOOK.  Every so often this asks every
%% currently-connected peer for its whole boundary list, waits for the
%% answers, and only then decides.  Hooking connection setup instead would
%% have meant two hooks (inbound and outbound handshakes complete in
%% different places) and, worse, a decision taken on the first answer to
%% arrive.  A round gives "we have heard from everyone" a definition, and
%% picks up a replication set that grew since last time for free.
%%
%% WHY IT MUST NOT DECIDE ON THE FIRST ANSWER.  A floor can only be set
%% once: ssb_feed:seed_floor/2 refuses a feed that already holds anything,
%% and by the time a second peer offers a lower boundary we would already
%% be storing messages above the first one.  Lowering it afterwards means
%% backfilling the gap, which is a different feature.  So offers are
%% staged for the length of a round and the LOWEST is taken — retaining
%% more history and keeping us a witness to more of the feed.
%%
%% DIRECT FOLLOWS ARE NEVER FLOORED.  People you followed deliberately are
%% people you are the witness of last resort for; the saving is wanted at
%% hops 2 and 3, where the boundary set explodes.  This is the hop count
%% standing in for a trust metric — when one exists, it replaces this test
%% and nothing else here changes.
%%
%% NOTHING HERE TRUSTS A PEER.  Every offer is a message signed by the
%% feed's own author, verified before it is looked at.  A hostile peer can
%% withhold boundaries (we replicate from the beginning, as we would have
%% anyway) or offer an older one (we skip less) — neither is an attack.
-module(boundary_discovery).

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/0,
         run_now/0,
         adopt_offers/1,
         decide/5,
         lowest/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% Long enough that a node settles after start before spending anything on
%% a question whose answer is usually "no boundaries at all".
-define(FIRST_ROUND_MS, 60_000).
-define(ROUND_MS, 300_000).

%% A whole round's budget for peers to answer.  Peers are asked in
%% parallel, so this bounds the round rather than each peer: one
%% unresponsive peer costs the round nothing but its own answer.
-define(ROUND_BUDGET_MS, 20_000).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Run a round now instead of waiting for the timer (tests, and an
%% operator who has just changed the replication set).
run_now() ->
    gen_server:call(?SERVER, run_now, ?ROUND_BUDGET_MS + 10_000).

%%%===================================================================
%%% Rounds
%%%===================================================================

%% One round: ask everyone, then decide once.  Runs outside the
%% gen_server so a slow peer cannot block anything else.
round() ->
    case config:archive_floors() of
        false -> ok;
        true  -> adopt_offers(collect(peer_registry:all()))
    end.

%% Ask every connected peer in parallel and return the offers that
%% verified.  A peer that errors, disconnects or simply never answers
%% contributes nothing and costs the round nothing.
collect(Peers) ->
    Self = self(),
    Refs = [begin
                Ref = make_ref(),
                _ = spawn(fun() -> Self ! {Ref, ask(Pid)} end),
                Ref
            end || {_PubKey, Pid} <- Peers],
    Deadline = erlang:monotonic_time(millisecond) + ?ROUND_BUDGET_MS,
    lists:append(gather(Refs, Deadline)).

gather([], _Deadline) ->
    [];
gather([Ref | Rest], Deadline) ->
    Wait = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Ref, Offers} -> [Offers | gather(Rest, Deadline)]
    after Wait ->
        %% Out of budget: abandon this answer and every one still
        %% outstanding.  They are re-asked next round.
        []
    end.

ask(Pid) ->
    try ssb_peer:rpc_stream_call(Pid, [?archives, ?boundaries], []) of
        {ok, Bodies} -> verified(Bodies);
        _            -> []
    catch _:_ ->
        []
    end.

%% Decode with signature checking.  An offer that does not verify is
%% dropped silently: it means a peer sent us rubbish, not that anything
%% is wrong with the feed it names.
verified(Bodies) ->
    lists:filtermap(
      fun(Body) ->
              try message:decode_value(Body, true) of
                  #message{validated = true} = Msg -> {true, Msg};
                  _                                -> false
              catch _:_ ->
                  false
              end
      end, Bodies).

%%%===================================================================
%%% Adoption
%%%===================================================================

%% Take the lowest offered boundary for every feed we are willing to
%% floor.  Exported so a test can drive it without a network.
adopt_offers([]) ->
    ok;
adopt_offers(Offers) ->
    Self   = keys:pub_key_disp(),
    Direct = sets:from_list(ssb_social_graph:direct_follows(Self)),
    maps:foreach(fun(FeedId, Msgs) ->
                         maybe_adopt(FeedId, Msgs, Self, Direct)
                 end, by_feed(Offers)),
    ok.

by_feed(Offers) ->
    lists:foldl(fun(#message{author = A} = M, Acc) ->
                        maps:update_with(A, fun(L) -> [M | L] end, [M], Acc)
                end, #{}, Offers).

maybe_adopt(FeedId, Msgs, Self, Direct) ->
    case decide(FeedId, Msgs, Self, Direct, ebt:replicate_feed(FeedId)) of
        {skip, _Reason} -> ok;
        {adopt, Msg}    -> adopt(FeedId, Msg)
    end.

%% The whole policy, in one pure function so it can be read and tested as
%% policy rather than inferred from the plumbing around it.
decide(FeedId, _Msgs, Self, _Direct, _InSet) when FeedId =:= Self ->
    %% Our own feed is authored here; there is no history to skip.
    {skip, own_feed};
decide(FeedId, Msgs, _Self, Direct, InSet) ->
    case sets:is_element(FeedId, Direct) of
        true ->
            %% A direct follow keeps its whole history: see the note on
            %% witnesses at the top.
            {skip, direct_follow};
        false when not InSet ->
            {skip, not_replicated};
        false ->
            {adopt, lowest(Msgs)}
    end.

%% The most conservative offer: the boundary that skips the least.
lowest(Msgs) ->
    hd(lists:sort(fun(#message{sequence = A}, #message{sequence = B}) ->
                          A =< B
                  end, Msgs)).

adopt(FeedId, Lowest) ->
    case utils:find_or_create_feed_pid(FeedId) of
        bad ->
            ok;
        Pid ->
            %% seed_floor/2 refuses a feed holding anything, so this is
            %% belt and braces — but checking first keeps a routine
            %% "already replicating" out of the error log.
            case ssb_feed:current_seq(Pid) of
                0 -> seed(FeedId, Pid, Lowest);
                _ -> ok
            end
    end.

seed(FeedId, Pid, #message{sequence = Seq} = Msg) ->
    case ssb_feed:seed_floor(Pid, Msg) of
        ok ->
            ?SSB_INFO("boundary_discovery: floored ~s at seq ~p", [FeedId, Seq]);
        {error, Reason} ->
            ?SSB_DEBUG("boundary_discovery: ~s not floored: ~p~n",
                       [FeedId, Reason])
    end.

%%%===================================================================
%%% gen_server
%%%===================================================================

init([]) ->
    _ = erlang:send_after(?FIRST_ROUND_MS, self(), round),
    {ok, #{}}.

handle_call(run_now, _From, State) ->
    {reply, round(), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(round, State) ->
    %% Off the server process: a round waits on peers, and this server
    %% should stay answerable while it does.
    _ = spawn(fun round/0),
    _ = erlang:send_after(?ROUND_MS, self(), round),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State)       -> ok.
code_change(_Old, State, _Extra) -> {ok, State}.
