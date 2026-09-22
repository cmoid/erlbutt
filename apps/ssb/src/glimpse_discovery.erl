%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Asks connected peers what the feeds just outside our replication set
%% say about themselves, and stages the answers for a person to act on.
%%
%% The shape is boundary_discovery's, for the reasons that module already
%% gives: a full round rather than a connect hook, every peer asked in
%% parallel, offers staged for the length of the round and decided once at
%% the end.  Two things differ.
%%
%% THE HIGHEST SEQUENCE WINS, not the lowest.  A boundary is a decision
%% about how much history to keep, so the most conservative offer is the
%% right one.  A glimpse is a self-description: the only thing wrong with
%% an old one is that it is old.  Nothing pins which glimpse a peer shows
%% us — taking the highest across a round narrows that, and does not close
%% it.  See "Staleness" in doc/research/feed-glimpses.md.
%%
%% THE GRAPH DECIDES CANDIDACY, THE GLIMPSE DECIDES PROMOTION.  A glimpse
%% is considered only for a feed already reachable at hops+1 — someone
%% followed by someone we replicate.  This is the answer to the one new
%% attack the feature opens: a hostile peer can make a boundary feed look
%% more interesting than it is, but it cannot introduce a feed, because an
%% offer for a stranger nobody we carry has ever followed is dropped
%% unread.  We don't want nobody that nobody sent.
%%
%% NOTHING HERE TRUSTS A PEER.  Every offer is a message signed by the
%% feed's own author and verified before it is looked at — and a feed id
%% IS an ed25519 public key, so that check needs no prior knowledge of the
%% feed whatsoever.  A hostile peer can withhold glimpses (we see nothing,
%% as we would have anyway) or serve a stale one (we are out of date about
%% a feed we do not replicate).  Neither is an attack.
%%
%% NOTHING HERE REPLICATES ANYTHING.  A staged glimpse creates no feed
%% process and no author record; it sits in its own table until a person
%% promotes the feed (feed_pins) or it ages out.
-module(glimpse_discovery).

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/0,
         run_now/0,
         peek/0,
         boundary_set/0,
         adopt_offers/1,
         context/0,
         decide/4,
         highest/1,
         usable/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% Long enough that a node settles before spending anything on a question
%% whose answer, on a network where nobody publishes glimpses, is nothing.
-define(FIRST_ROUND_MS, 90_000).
-define(ROUND_MS, 600_000).

%% A whole round's budget for peers to answer.  Peers are asked in
%% parallel, so this bounds the round rather than each peer.
-define(ROUND_BUDGET_MS, 20_000).

%% How close together a client may ask for a round.  peek/0 exists so a
%% person who has just opened an unreplicated profile does not wait out
%% the timer; it is not a licence to run a round per click.
-define(PEEK_FLOOR_MS, 30_000).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Run a round now and wait for it (tests, and an operator).
run_now() ->
    gen_server:call(?SERVER, run_now, ?ROUND_BUDGET_MS + 10_000).

%% Run a round soon, without waiting: what a client calls when a person
%% has asked about a feed we know nothing about.  Rate-limited, and a
%% no-op when the server is not running.
peek() ->
    gen_server:cast(?SERVER, peek).

%%%===================================================================
%%% Rounds
%%%===================================================================

round() ->
    case config:glimpses() of
        false -> ok;
        true  -> adopt_offers(collect(peer_registry:all()))
    end.

%% Ask every connected peer in parallel and return the offers that
%% verified.  A peer that errors, disconnects or never answers
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
    try ssb_peer:rpc_stream_call(Pid, [?glimpses, ?offers], []) of
        {ok, Bodies} -> verified(Bodies);
        _            -> []
    catch _:_ ->
        []
    end.

%% Decode with signature checking.  An offer that does not verify is
%% dropped silently: it means a peer sent us rubbish, not that anything is
%% wrong with the feed it names.
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

adopt_offers([]) ->
    ok;
adopt_offers(Offers) ->
    Ctx = context(),
    maps:foreach(fun(FeedId, Msgs) -> maybe_stage(FeedId, Msgs, Ctx) end,
                 by_feed(Offers)),
    ok.

%% Everything a decision needs, gathered once per round rather than per
%% offer: the boundary walk is two reachability queries over the whole
%% follow graph.
context() ->
    Self = keys:pub_key_disp(),
    #{self     => Self,
      boundary => boundary_set(),
      blocked  => sets:from_list(ssb_social_graph:blocks(Self))}.

%% Feeds at hops+1: reachable through the follow graph one step beyond
%% what we replicate, and therefore deliberately not replicated.
%%
%% This is a large set — on a node replicating a few thousand feeds it
%% runs to tens of thousands — which is the whole reason a glimpse is
%% wanted rather than a fetch.  It is used only as a membership test, and
%% nothing about it goes on the wire.
boundary_set() ->
    Self  = keys:pub_key_disp(),
    Hops  = config:replication_hops(),
    Inner = sets:from_list(ssb_social_graph:follows(Self, Hops)),
    Outer = sets:from_list(ssb_social_graph:follows(Self, Hops + 1)),
    sets:subtract(Outer, Inner).

by_feed(Offers) ->
    lists:foldl(fun(#message{author = A} = M, Acc) ->
                        maps:update_with(A, fun(L) -> [M | L] end, [M], Acc)
                end, #{}, Offers).

maybe_stage(FeedId, Msgs, Ctx) ->
    case decide(FeedId, Msgs, Ctx, ebt:replicate_feed(FeedId)) of
        {skip, _Reason} -> ok;
        {stage, Msg}    -> stage(FeedId, Msg)
    end.

%% The whole policy, in one pure function so it can be read and tested as
%% policy rather than inferred from the plumbing around it.
decide(FeedId, _Msgs, #{self := Self}, _InSet) when FeedId =:= Self ->
    %% We wrote it; the view already has it.
    {skip, own_feed};
decide(FeedId, Msgs, #{boundary := Boundary, blocked := Blocked}, InSet) ->
    case sets:is_element(FeedId, Blocked) of
        true ->
            {skip, blocked};
        false when InSet ->
            %% Already replicated: the glimpse arrives through the
            %% ordinary path and is indexed by the view, which is a
            %% better copy than anything a peer hands us.
            {skip, replicated};
        false ->
            case sets:is_element(FeedId, Boundary) of
                false -> {skip, not_at_boundary};
                true  -> usable_highest(Msgs)
            end
    end.

usable_highest(Msgs) ->
    case [M || M <- Msgs, usable(M)] of
        []       -> {skip, no_usable_offer};
        Sensible -> {stage, highest(Sensible)}
    end.

%% An offer worth staging: it names a blob, and the payload it claims is
%% one we are willing to fetch.  A glimpse with no blob shows nothing; one
%% the size of a feed defeats the point of not replicating the feed.
%%
%% The size is the author's own claim and could be a lie.  It is checked
%% anyway, because the honest case is the common one and this is the only
%% point where the cost is knowable before it is paid.
usable(#message{content = {Props}}) ->
    case {?pgv(~"blob", Props), ?pgv(~"size", Props)} of
        {<<"&", _/binary>>, Size} when is_integer(Size) ->
            Size =< ?GLIMPSE_MAX_SIZE;
        {<<"&", _/binary>>, undefined} ->
            %% No claim made.  Allowed: the blob fetch is bounded
            %% elsewhere, and refusing would punish a client that simply
            %% did not fill the field in.
            true;
        _ ->
            false
    end;
usable(_) ->
    false.

%% The newest self-description on offer.
highest(Msgs) ->
    hd(lists:sort(fun(#message{sequence = A}, #message{sequence = B}) ->
                          A >= B
                  end, Msgs)).

stage(FeedId, #message{sequence = Seq, content = {Props}} = Msg) ->
    ok = ssb_glimpses:stage(Msg),
    want_payload(?pgv(~"blob", Props)),
    ?SSB_DEBUG("glimpse_discovery: staged ~s at seq ~p~n", [FeedId, Seq]),
    ok.

%% The message is the pointer; the payload is a blob like any other, and
%% wanting it is how every other blob arrives.  Nothing waits on it: the
%% staged row is useful on its own (it says this feed describes itself),
%% and a client renders the payload when it lands.
want_payload(<<"&", _/binary>> = Blob) ->
    case blobs:has(Blob) of
        true  -> ok;
        false -> blob_fetcher:want(Blob)
    end;
want_payload(_) ->
    ok.

%%%===================================================================
%%% gen_server
%%%===================================================================

init([]) ->
    _ = erlang:send_after(?FIRST_ROUND_MS, self(), round),
    {ok, #{last_round => 0}}.

handle_call(run_now, _From, State) ->
    {reply, round(), State#{last_round => now_ms()}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(peek, #{last_round := Last} = State) ->
    Now = now_ms(),
    case Now - Last < ?PEEK_FLOOR_MS of
        true ->
            %% A round is already this fresh; whatever the caller is
            %% waiting for either arrived in it or is not on offer.
            {noreply, State};
        false ->
            _ = spawn(fun round/0),
            {noreply, State#{last_round => Now}}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(round, State) ->
    %% Off the server process: a round waits on peers, and this server
    %% should stay answerable while it does.
    _ = spawn(fun round/0),
    _ = erlang:send_after(?ROUND_MS, self(), round),
    {noreply, State#{last_round => now_ms()}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State)       -> ok.
code_change(_Old, State, _Extra) -> {ok, State}.

now_ms() -> erlang:monotonic_time(millisecond).

-ifdef(TEST).

-define(SELF,     ~"@self.ed25519").
-define(FRIEND,   ~"@friend.ed25519").
-define(EDGE,     ~"@edge.ed25519").
-define(BLOCKED,  ~"@blocked.ed25519").
-define(STRANGER, ~"@stranger.ed25519").

glimpse(Feed, Seq) ->
    glimpse(Feed, Seq, ~"&payload.sha256", 4096).

glimpse(Feed, Seq, Blob, Size) ->
    #message{author = Feed, sequence = Seq, validated = true,
             content = {[{~"type", ~"glimpse"},
                         {~"blob", Blob},
                         {~"size", Size}]}}.

ctx() ->
    #{self     => ?SELF,
      boundary => sets:from_list([?EDGE, ?BLOCKED]),
      blocked  => sets:from_list([?BLOCKED])}.

%% A self-description is only interesting if it is the current one, so
%% where boundary_discovery takes the lowest offer this takes the highest.
highest_offer_wins_test() ->
    Offers = [glimpse(?EDGE, 12), glimpse(?EDGE, 300), glimpse(?EDGE, 88)],
    ?assertMatch({stage, #message{sequence = 300}},
                 decide(?EDGE, Offers, ctx(), false)),
    %% order of arrival must not matter
    ?assertMatch({stage, #message{sequence = 300}},
                 decide(?EDGE, lists:reverse(Offers), ctx(), false)).

%% The spam defence: an offer for a feed nobody we replicate has followed
%% is dropped unread, so a peer can flatter a boundary feed but cannot
%% introduce one.
a_feed_nobody_sent_is_not_considered_test() ->
    ?assertEqual({skip, not_at_boundary},
                 decide(?STRANGER, [glimpse(?STRANGER, 5)], ctx(), false)).

%% A block outranks reachability: being followed by someone we replicate
%% does not get a blocked feed back in front of us.
blocked_feeds_are_not_staged_test() ->
    ?assertEqual({skip, blocked},
                 decide(?BLOCKED, [glimpse(?BLOCKED, 5)], ctx(), false)).

%% For a feed we already replicate the view holds the real message; a
%% peer's copy is not an improvement on it.
replicated_feeds_are_left_to_the_view_test() ->
    ?assertEqual({skip, replicated},
                 decide(?FRIEND, [glimpse(?FRIEND, 5)], ctx(), true)).

own_glimpse_is_never_staged_test() ->
    ?assertEqual({skip, own_feed},
                 decide(?SELF, [glimpse(?SELF, 5)], ctx(), true)).

%% A glimpse naming no blob shows nothing, and one that claims to be
%% enormous defeats the purpose of not replicating the feed.
unusable_offers_are_refused_test() ->
    NoBlob = #message{author = ?EDGE, sequence = 9, validated = true,
                      content = {[{~"type", ~"glimpse"}]}},
    ?assertNot(usable(NoBlob)),
    ?assertNot(usable(glimpse(?EDGE, 9, ~"&big.sha256",
                              ?GLIMPSE_MAX_SIZE + 1))),
    ?assert(usable(glimpse(?EDGE, 9, ~"&ok.sha256", ?GLIMPSE_MAX_SIZE))),
    %% a client that filled in no size is not punished for it
    ?assert(usable(glimpse(?EDGE, 9, ~"&nosize.sha256", undefined))),
    ?assertEqual({skip, no_usable_offer},
                 decide(?EDGE, [NoBlob], ctx(), false)).

%% An oversized offer must not shadow a usable one from another peer.
usable_offer_survives_an_unusable_one_test() ->
    Offers = [glimpse(?EDGE, 40, ~"&big.sha256", ?GLIMPSE_MAX_SIZE * 4),
              glimpse(?EDGE, 20, ~"&ok.sha256", 1024)],
    ?assertMatch({stage, #message{sequence = 20}},
                 decide(?EDGE, Offers, ctx(), false)).

%% Nothing offered, nothing to do — the normal case on a network where
%% nobody publishes glimpses.
empty_round_is_a_noop_test() ->
    ?assertEqual(ok, adopt_offers([])).

%% Offers are grouped by author, so two feeds in one round are both
%% considered and neither shadows the other.
grouping_keeps_feeds_apart_test() ->
    Offers = [glimpse(?EDGE, 30), glimpse(?FRIEND, 20), glimpse(?EDGE, 10)],
    Grouped = by_feed(Offers),
    ?assertEqual(2, maps:size(Grouped)),
    ?assertMatch(#message{sequence = 30}, highest(maps:get(?EDGE, Grouped))),
    ?assertMatch(#message{sequence = 20}, highest(maps:get(?FRIEND, Grouped))).

-endif.
