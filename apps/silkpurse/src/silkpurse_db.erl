%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Stateless db-surface plugins for UI clients: methods that answer
%% straight from the per-feed store with no derived index.  Registered
%% by silkpurse_app at startup.
-module(silkpurse_db).

-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([manifest/0, handle_rpc/3]).

manifest() ->
    [{[~"getLatest"], async, owner},
     {[~"latestSequence"], async, owner},
     {[~"friends", ~"get"], async, owner},
     {[~"patchwork", ~"suggest", ~"profile"], async, owner}].

%% getLatest(feedId) -> {id, sequence, ts} of the feed's newest message
%% (the shape ssb-db clients expect).
handle_rpc([~"getLatest"], [FeedId], _Caller) when is_binary(FeedId) ->
    try utils:find_or_create_feed_pid(FeedId) of
        Pid ->
            case ssb_feed:fetch_last_msg(Pid) of
                #message{id = Id, sequence = Seq, timestamp = Ts} ->
                    {reply, {[{~"id", Id},
                              {~"sequence", Seq},
                              {~"ts", Ts}]}};
                _ ->
                    {error, ~"no messages"}
            end
    catch _:_ ->
            {error, ~"bad feed id"}
    end;
handle_rpc([~"getLatest"], _Args, _Caller) ->
    {error, ~"getLatest takes a feed id"};

%% latestSequence(feedId) -> the sequence number of the feed's newest
%% message (ssb-db), or 0 for an empty/unknown feed.
handle_rpc([~"latestSequence"], [FeedId], _Caller) when is_binary(FeedId) ->
    try
        Pid = utils:find_or_create_feed_pid(FeedId),
        case ssb_feed:fetch_last_msg(Pid) of
            #message{sequence = Seq} -> {reply, Seq};
            _                        -> {reply, 0}
        end
    catch _:_ ->
            {reply, 0}
    end;
handle_rpc([~"latestSequence"], _Args, _Caller) ->
    {error, ~"latestSequence takes a feed id"};

%% friends.get(opts) -> the follow/block relationship in ssb-friends
%% legacy terms (true following, false blocking, null neither):
%%   {source, dest} -> a single value
%%   {source}       -> {dest: value} for all of source's edges
handle_rpc([~"friends", ~"get"], [{Opts}], _Caller) ->
    Source = ?pgv(~"source", Opts),
    Dest   = ?pgv(~"dest", Opts),
    case {Source, Dest} of
        {S, D} when is_binary(S), is_binary(D) ->
            {reply, ssb_social_graph:edge(S, D)};
        {S, undefined} when is_binary(S) ->
            {reply, {maps:to_list(ssb_social_graph:edges(S))}};
        _ ->
            {error, ~"friends.get needs a source"}
    end;
handle_rpc([~"friends", ~"get"], _Args, _Caller) ->
    {error, ~"friends.get takes an options object"};

%% suggest.profile({text, defaultIds, limit}) -> profiles for mention
%% autocomplete: names matching text, ranked, each as
%% {id, name, image, following}.
%%
%% `defaultIds` is the composer's list of people already in the thread.  It
%% used to be consulted ONLY when the text was empty, so typing a single
%% letter threw the thread away — the one piece of context a suggestion
%% has, gone at the first keystroke.  It is a ranking signal now.
handle_rpc([~"patchwork", ~"suggest", ~"profile"], [{Opts}], _Caller) ->
    Text  = ?pgv(~"text", Opts),
    Limit = case ?pgv(~"limit", Opts) of L when is_integer(L) -> L; _ -> 20 end,
    Ids   = case ?pgv(~"defaultIds", Opts) of
                DL when is_list(DL) -> [I || I <- DL, is_binary(I)];
                _                   -> []
            end,
    Owner = keys:pub_key_disp(),
    Pairs = case Text of
                T when is_binary(T), T =/= ~"" ->
                    rank(silkpurse_about:search_names(T), T, Ids, Owner, Limit);
                _ ->
                    %% empty text: the thread, which is the best guess there is
                    [{Id, ssb_feed_meta:name(Id)} || Id <- Ids]
            end,
    {reply, [suggestion(Id, Name, Owner) || {Id, Name} <- Pairs]};
handle_rpc([~"patchwork", ~"suggest", ~"profile"], _Args, _Caller) ->
    {reply, []}.

%% Order the matches by how likely you meant them, then keep Limit.
%%
%% Ranking BEFORE truncating is the point.  Keeping the first twenty
%% matches and ordering only those is what surfaced long-dead feeds: the
%% candidate order out of SQLite is effectively feed id, so on a store with
%% 42,051 named feeds the twenty you were shown had nothing to do with you.
%%
%% Tiers, strongest first:
%%   0  already in this thread
%%   1  someone you follow
%%   2  everyone else
%% and within a tier a name STARTING with what you typed beats one merely
%% containing it, then the shorter name, then alphabetical — so the list
%% does not reshuffle between keystrokes.
rank(Matches, Text, Ids, Owner, Limit) ->
    %% One lookup for the whole follow graph, not one per candidate.
    Follows = try ssb_social_graph:edges(Owner) catch _:_ -> #{} end,
    rank(Matches, Text, Ids, Follows, Owner, Limit).

%% The ordering itself, with every signal passed in — pure, so it can be
%% read and tested as policy rather than inferred from the plumbing.
rank(Matches, Text, Ids, Follows, _Owner, Limit) ->
    Participants = sets:from_list(Ids),
    Needle  = string:lowercase(Text),
    Keyed = [{key(Id, Name, Needle, Participants, Follows), {Id, Name}}
             || {Id, Name} <- Matches],
    lists:sublist([P || {_K, P} <- lists:keysort(1, Keyed)], Limit).

key(Id, Name, Needle, Participants, Follows) ->
    Tier = case {sets:is_element(Id, Participants),
                 maps:get(Id, Follows, false)} of
               {true, _} -> 0;
               {_, true} -> 1;
               _         -> 2
           end,
    Lower = string:lowercase(Name),
    %% string:find returns the SUFFIX starting at the match, so a match at
    %% position 0 comes back as the whole string.
    Prefix = case string:find(Lower, Needle) of
                 Lower -> 0;
                 _     -> 1
             end,
    %% Id last so feeds sharing a name get a stable order rather than
    %% whatever SQLite happened to yield.
    {Tier, Prefix, byte_size(Name), Lower, Id}.

suggestion(Id, Name, Owner) ->
    {[{~"id", Id},
      {~"name", Name},
      {~"image", silkpurse_about:social_value(Id, ~"image")},
      {~"following", ssb_social_graph:edge(Owner, Id) =:= true}]}.

-ifdef(TEST).

friends_get_args_test() ->
    %% only shape handling here; graph semantics live in ssb_social_graph.erl
    ?assertMatch({error, _},
                 handle_rpc([~"friends", ~"get"], [~"notanobj"], #{})),
    ?assertMatch({error, _},
                 handle_rpc([~"friends", ~"get"], [{[]}], #{})).


%% Ranking is pure, so it can be tested without a store: the signals are
%% the thread, the follow graph, and what was typed.
rank_key(Id, Name, Text, Ids, Follows) ->
    key(Id, Name, string:lowercase(Text), sets:from_list(Ids), Follows).

%% Someone in the thread outranks someone you follow, who outranks a
%% stranger — regardless of how the names sort.  This is the whole point:
%% the old code kept the first twenty matches in feed-id order.
suggest_ranks_thread_then_follows_test() ->
    In     = ~"@inthread=.ed25519",
    Follow = ~"@followed=.ed25519",
    Other  = ~"@stranger=.ed25519",
    Follows = #{Follow => true},
    Ids = [In],
    K = fun(Id, Name) -> rank_key(Id, Name, ~"al", Ids, Follows) end,
    ?assert(K(In, ~"zzz alice") < K(Follow, ~"alice")),
    ?assert(K(Follow, ~"zzz alice") < K(Other, ~"alice")),
    %% a block is not a follow: edges/1 reports false, so tier 2
    ?assert(K(Other, ~"alice") =:= rank_key(Other, ~"alice", ~"al", [],
                                            #{Other => false})).

%% Within a tier, a name that STARTS with what you typed wins, then the
%% shorter name, then alphabetical so the list is stable between
%% keystrokes.
suggest_prefers_prefix_then_shorter_test() ->
    A = ~"@a=.ed25519", B = ~"@b=.ed25519",
    K = fun(Id, Name) -> rank_key(Id, Name, ~"al", [], #{}) end,
    ?assert(K(A, ~"alice")   < K(B, ~"realtor")),   %% prefix beats substring
    ?assert(K(A, ~"al")      < K(B, ~"alice")),     %% then shorter
    ?assert(K(A, ~"alan")    < K(B, ~"alice")).     %% then alphabetical

%% Truncation happens AFTER ordering, so a followed feed cannot be lost to
%% strangers that merely sorted earlier out of SQLite.
suggest_truncates_after_ranking_test() ->
    Follow = ~"@followed=.ed25519",
    Strangers = [{list_to_binary("@s" ++ integer_to_list(N) ++ "=.ed25519"),
                  ~"alpha"} || N <- lists:seq(1, 50)],
    Matches = Strangers ++ [{Follow, ~"alpha"}],
    Ranked = rank(Matches, ~"al", [], #{Follow => true},
                  ~"@owner=.ed25519", 5),
    ?assertEqual(5, length(Ranked)),
    ?assertNotEqual(false, lists:keyfind(Follow, 1, Ranked)).
-endif.
