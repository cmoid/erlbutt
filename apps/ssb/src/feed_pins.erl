%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Feeds this node replicates because its operator said so, rather than
%% because the follow graph reached them.
%%
%% The replication set has always been derived: {self} ∪ follows(self,
%% hops) ∪ room members − blocks.  Every feed in it is there as a
%% consequence of something published.  A pin is the one exception — a
%% local, private, reversible decision to carry a feed the graph did not
%% deliver.
%%
%% WHY NOT JUST FOLLOW THEM.  Following is a public statement about a
%% stranger, and un-following is a second one.  Promotion from a glimpse
%% (doc/research/feed-glimpses.md) has to be cheap in both directions or
%% the argument that a glimpse is *advisory* stops holding: the whole
%% claim is that acting on an unanchored self-description is safe because
%% the action is trivially undone.  A pin is undone by deleting a row, and
%% nobody else ever knew.
%%
%% Following is still the right thing to do once a feed has earned it.
%% This is what you do BEFORE that, while you are finding out.
%%
%% A BLOCK STILL WINS.  ebt applies blocks after the union, so pinning
%% someone you block replicates nothing; the pin is simply inert.  That
%% ordering is deliberate — a pin is a convenience, a block is a
%% statement.
-module(feed_pins).

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/0,
         pin/1, pin/2,
         unpin/1,
         is_pinned/1,
         all/0,
         list/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SCHEMA_VERSION, 1).

-define(DDL,
        ["CREATE TABLE IF NOT EXISTS feed_pin("
         "  feed      TEXT PRIMARY KEY,"
         "  pinned_at INTEGER NOT NULL,"
         %% Free text saying why, so a list of pins a year from now is
         %% readable.  "glimpse" is what promotion writes.
         "  source    TEXT) WITHOUT ROWID;"]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

pin(FeedId) ->
    pin(FeedId, ~"manual").

%% Idempotent, and deliberately does not refresh pinned_at on a re-pin:
%% "since when" is more useful than "most recently asked for".
pin(<<"@", _/binary>> = FeedId, Source) ->
    _ = write("INSERT INTO feed_pin(feed, pinned_at, source) VALUES(?,?,?)"
              " ON CONFLICT(feed) DO NOTHING",
              [FeedId, erlang:system_time(millisecond), Source]),
    %% Nothing replicates until the set is recomputed, and the timer is
    %% 20 seconds away.  A person who just pressed a button should not
    %% wait for it.
    _ = catch ebt:refresh_repl_set(),
    ok;
pin(_NotAFeed, _Source) ->
    {error, not_a_feed}.

%% Unpinning stops us ASKING for the feed; it does not delete what has
%% already arrived.  Dropping stored messages is a different operation
%% with different consequences, and conflating the two would make "try
%% this feed for a week" quietly destructive.
unpin(FeedId) ->
    _ = write("DELETE FROM feed_pin WHERE feed = ?", [FeedId]),
    _ = catch ebt:refresh_repl_set(),
    ok.

is_pinned(FeedId) ->
    case q("SELECT 1 FROM feed_pin WHERE feed = ?", [FeedId]) of
        [_ | _] -> true;
        _       -> false
    end.

%% Just the ids, for the replication set union.
all() ->
    [F || [F] <- q("SELECT feed FROM feed_pin", [])].

%% Ids with their context, for a client.
list() ->
    [#{feed => F, pinned_at => At, source => Src}
     || [F, At, Src] <- q("SELECT feed, pinned_at, source FROM feed_pin"
                          " ORDER BY pinned_at DESC", [])].

%%%===================================================================
%%% Internal
%%%===================================================================

%% all/0 is called from ebt's replication-set recompute, which must not
%% fail: an empty pin list degrades to the graph-derived set, which is
%% what every node had before pins existed.
q(Sql, Params) ->
    try ssb_store:q(Sql, Params) of
        L when is_list(L) -> L;
        _                 -> []      %% no table yet, or the store said no
    catch _:_ -> []
    end.

write(Sql, Params) ->
    catch ssb_store:write(Sql, Params).

%%%===================================================================
%%% gen_server
%%%===================================================================

init([]) ->
    ok = ssb_store:declare(?MODULE, ?SCHEMA_VERSION, ?DDL),
    {ok, #{}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(_Info, State)           -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_Old, State, _Extra)    -> {ok, State}.

-ifdef(TEST).

-define(FEED, ~"@pinned.ed25519").

pins_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
             [?_test(pinning_is_idempotent_and_dated()),
              ?_test(unpinning_removes_it()),
              ?_test(only_a_feed_id_can_be_pinned()),
              ?_test(an_empty_pin_list_is_not_an_error())]
     end}.

setup() ->
    cleanup(ignore),
    Home = filename:join("/tmp", "pins_"
                         ++ integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = ssb_store:start_link(),
    ok = ssb_store:declare(?MODULE, ?SCHEMA_VERSION, ?DDL),
    Home.

cleanup(Home) ->
    [catch gen_server:stop(N) || N <- [?MODULE, ssb_store, config]],
    case Home of
        ignore -> ok;
        _ -> os:cmd("rm -rf " ++ Home),
             application:unset_env(ssb, ssb_home)
    end,
    ok.

%% Pinning twice is one pin, and "since when" survives the second press.
pinning_is_idempotent_and_dated() ->
    ok = pin(?FEED, ~"glimpse"),
    [#{pinned_at := First}] = list(),
    ok = pin(?FEED, ~"manual"),
    ?assertEqual([?FEED], all()),
    ?assert(is_pinned(?FEED)),
    ?assertMatch([#{feed := ?FEED, pinned_at := First, source := ~"glimpse"}],
                 list()).

unpinning_removes_it() ->
    ok = unpin(?FEED),
    ?assertEqual([], all()),
    ?assertNot(is_pinned(?FEED)),
    %% unpinning something that was never pinned is not an error
    ?assertEqual(ok, unpin(~"@never.ed25519")).

only_a_feed_id_can_be_pinned() ->
    ?assertEqual({error, not_a_feed}, pin(~"%amessage.sha256", ~"manual")),
    ?assertEqual({error, not_a_feed}, pin(~"&ablob.sha256", ~"manual")),
    ?assertEqual([], all()).

%% ebt's replication-set recompute calls all/0 every 20 seconds and must
%% never see an exception.
an_empty_pin_list_is_not_an_error() ->
    ?assertEqual([], all()),
    ?assertEqual([], list()).

-endif.
