%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Channel index: which channels exist, how active each is, and which
%% ones the node owner subscribes to.  A channel is named by a message's
%% content.channel; a subscription is a {type: channel, channel, subscribed}
%% message (a boolean toggle).  The view keeps, per normalized channel
%% name, a post count and last-activity timestamp, plus the owner's latest
%% subscription state.
%%
%% An ssb_view over ssb_store plus an ssb_plugin serving the discovery
%% surface:
%%   channels.suggest({text, limit})  async  -> [{id, count, subscribed}]
%%   channels.recentStream({limit})    source -> live [channelName] by recency
%%
%% Two tables, where there was one ETS set holding both kinds of row under
%% tagged keys ({stat, Ch} and {sub, Ch, Feed}).  That tagging is what a
%% single-table store forces, and it had a cost: subscribers/1 could not
%% look anything up, so listing one channel's subscribers meant folding
%% every stat and every subscription in the index.  Separated, it is a
%% primary-key prefix scan.
%%
%% Channel names are normalized (lowercased, '#' stripped) on the way in,
%% so the suggest match can be an instr() in SQL rather than a fold: both
%% sides have already been through Erlang's Unicode-aware lowercasing by
%% the time they meet, which is what makes pushing it down equivalent
%% rather than merely similar.
%%
%% channel.obs.subscribed (a feed's own subscriptions) is computed client
%% side from its feed via createUserStream, and subscribing is a publish,
%% so neither needs a method here.
-module(silkpurse_channels).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).
-behaviour(ssb_view).
-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

-export([start_link/0]).
-export([view_version/0, view_load/0, view_reset/0, view_save/0, view_entry/1]).
-export([manifest/0, handle_rpc/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SCHEMA_VERSION, 1).
-define(DDL,
        ["CREATE TABLE IF NOT EXISTS channel_stat("
         "  channel TEXT PRIMARY KEY,"
         "  posts   INTEGER NOT NULL DEFAULT 0,"
         "  last    INTEGER NOT NULL DEFAULT 0) WITHOUT ROWID;",
         "CREATE INDEX IF NOT EXISTS ix_channel_last"
         "  ON channel_stat(last DESC);",
         "CREATE TABLE IF NOT EXISTS channel_sub("
         "  channel    TEXT NOT NULL,"
         "  feed       TEXT NOT NULL,"
         "  subscribed INTEGER NOT NULL,"
         "  ts         INTEGER NOT NULL,"
         "  PRIMARY KEY (channel, feed)) WITHOUT ROWID;"]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% ssb_view callbacks (run in the view_manager process)
%%%===================================================================

%% 2: subscriptions are tracked per subscriber feed (key {sub,Ch,Feed}),
%% not just the owner, so channels.subscriptions can list subscribers.
view_version() -> 2.

view_load() ->
    case ssb_store:complete(?MODULE) of
        true  -> ok;
        false -> empty
    end.

view_reset() ->
    _ = ssb_store:clear_complete(?MODULE),
    [ssb_store:exec(["DELETE FROM ", T, ";"])
     || T <- ["channel_stat", "channel_sub"]],
    ok.

%% Rows are already durable; the only thing to record is that this view's
%% state is complete up to the manager's checkpoints.
view_save() ->
    _ = ssb_store:mark_complete(?MODULE),
    ok.

view_entry(#message{author = Author, timestamp = Ts, content = {Props}}) ->
    case normalize_channel(?pgv(~"channel", Props)) of
        Ch when is_binary(Ch), byte_size(Ch) > 0 ->
            case ?pgv(~"type", Props) of
                ~"channel" ->
                    %% a subscription toggle by any feed
                    case ?pgv(~"subscribed", Props) of
                        Sub when is_boolean(Sub) ->
                            set_sub(Ch, Author, Sub, Ts),
                            {events, [{csub, Ch, Author, Sub}]};
                        _ -> ok
                    end;
                _ ->
                    %% any other message tagged with a channel is activity
                    bump(Ch, Ts),
                    {events, [{channel, Ch}]}
            end;
        _ ->
            ok
    end;
view_entry(_) ->
    ok.

%%%===================================================================
%%% ssb_plugin callbacks
%%%===================================================================

manifest() ->
    [{[~"patchwork", ~"channels", ~"suggest"],      async,  owner},
     {[~"patchwork", ~"channels", ~"recentStream"], source, owner},
     {[~"patchwork", ~"subscriptions"],             source, owner}].

handle_rpc([~"patchwork", ~"channels", ~"suggest"], [{Opts}], _Caller) ->
    Text  = case ?pgv(~"text", Opts) of T when is_binary(T) -> T; _ -> ~"" end,
    Limit = case ?pgv(~"limit", Opts) of L when is_integer(L) -> L; _ -> 20 end,
    {reply, suggest(Text, Limit)};
handle_rpc([~"patchwork", ~"channels", ~"suggest"], _Args, _Caller) ->
    {reply, []};

handle_rpc([~"patchwork", ~"channels", ~"recentStream"], Args, _Caller) ->
    Limit = case Args of
                [{Opts}] ->
                    case ?pgv(~"limit", Opts) of
                        L when is_integer(L) -> L;
                        _                    -> 10
                    end;
                _ -> 10
            end,
    %% a value stream: the current recent list now, re-sent (whole) on
    %% any channel change so the sidebar stays current
    Initial  = encode_json(recent(Limit)),
    EventFun = fun({channel, _Ch}) -> {send, encode_json(recent(Limit))};
                  (_)              -> skip
               end,
    {live_source, [{make_ref(), Initial}], ?MODULE, EventFun};

%% subscriptions({channel}): who subscribes to the channel, as
%% {from, value} toggles — current subscribers first, then live changes.
handle_rpc([~"patchwork", ~"subscriptions"], [{Opts}], _Caller) ->
    case normalize_channel(?pgv(~"channel", Opts)) of
        Ch when is_binary(Ch), byte_size(Ch) > 0 ->
            Snapshot = [{make_ref(), toggle(F, true)} || F <- subscribers(Ch)],
            EventFun = fun({csub, C, F, B}) when C =:= Ch -> {send, toggle(F, B)};
                          (_)                             -> skip
                       end,
            {live_source, Snapshot, ?MODULE, EventFun};
        _ ->
            {source, []}
    end;
handle_rpc([~"patchwork", ~"subscriptions"], _Args, _Caller) ->
    {source, []}.

toggle(Feed, Value) ->
    encode_json({[{~"from", Feed}, {~"value", Value}]}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = ssb_store:declare(?MODULE, ?SCHEMA_VERSION, ?DDL),
    {ok, #{}, {continue, register}}.

%% Failures are loud and transient ones retried on a timer
%% (ssb_view:ensure_registered) — the old silent noproc swallow here cost
%% EarlButt its messagesByType method (July 2026).
handle_continue(register, State) ->
    ensure_registered(State).

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(ensure_registered, State) ->
    ensure_registered(State);
handle_info(_Info, State) ->
    {noreply, State}.

%% First attempt (from handle_continue) and every timer retry land
%% here; keep trying until every service accepts the registration.
ensure_registered(State) ->
    case ssb_view:ensure_registered(?MODULE) of
        ok    -> ok;
        retry -> erlang:send_after(2000, self(), ensure_registered)
    end,
    {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

owner() ->
    try keys:pub_key_disp() catch _:_ -> undefined end.

%% Lowercase, strip a leading '#', trim; undefined for a non-channel.
normalize_channel(<<"#", Rest/binary>>) ->
    normalize_channel(Rest);
normalize_channel(B) when is_binary(B) ->
    unicode:characters_to_binary(string:lowercase(string:trim(B)));
normalize_channel(_) ->
    undefined.

bump(Ch, Ts) ->
    _ = write("INSERT INTO channel_stat(channel, posts, last)"
              " VALUES(?1, 1, ?2)"
              " ON CONFLICT(channel) DO UPDATE SET"
              "   posts = channel_stat.posts + 1,"
              "   last  = max(channel_stat.last, excluded.last)",
              [Ch, sort_key(Ts)]),
    ok.

%% Keep the newer toggle; on an equal timestamp the later-folded message
%% wins (feeds fold in sequence order), so >= rather than >.
set_sub(Ch, Feed, Subscribed, Ts) ->
    _ = write("INSERT INTO channel_sub(channel, feed, subscribed, ts)"
              " VALUES(?1, ?2, ?3, ?4)"
              " ON CONFLICT(channel, feed) DO UPDATE SET"
              "   subscribed = excluded.subscribed, ts = excluded.ts"
              " WHERE excluded.ts >= channel_sub.ts",
              [Ch, Feed, bool(Subscribed), sort_key(Ts)]),
    ok.

sort_key(Ts) when is_integer(Ts) -> Ts;
sort_key(_)                      -> 0.

bool(true) -> 1;
bool(_)    -> 0.

%% Feeds currently subscribed to Ch.  An indexed prefix scan now — this
%% used to fold the whole index, stats included, for one channel.
subscribers(Ch) when is_binary(Ch) ->
    [F || [F] <- rows("SELECT feed FROM channel_sub"
                      " WHERE channel=?1 AND subscribed=1", [Ch])];
subscribers(_) ->
    [].

%% Channels whose name contains Text (empty text matches all), most posts
%% first, capped at Limit, as [{id, count, subscribed}].
%%
%% The owner's subscription comes back in the same query rather than a
%% lookup per result: suggest is called per keystroke of the composer's
%% channel autocomplete.
suggest(Text, Limit) when is_integer(Limit), Limit >= 0 ->
    Needle = unicode:characters_to_binary(string:lowercase(Text)),
    Owner = case owner() of undefined -> ~""; O -> O end,
    Rows = rows(["SELECT s.channel, s.posts,"
                 " EXISTS(SELECT 1 FROM channel_sub b"
                 "        WHERE b.channel = s.channel AND b.feed = ?1"
                 "          AND b.subscribed = 1)"
                 " FROM channel_stat s"
                 " WHERE instr(s.channel, ?2) > 0"
                 " ORDER BY s.posts DESC, s.channel ASC"
                 " LIMIT ", integer_to_list(Limit)],
                [Owner, Needle]),
    [{[{~"id", Ch}, {~"count", Count}, {~"subscribed", Sub =:= 1}]}
     || [Ch, Count, Sub] <- Rows];
suggest(_Text, _Limit) ->
    [].

%% The most recently active channel names, newest first, capped at Limit.
recent(Limit) when is_integer(Limit), Limit >= 0 ->
    [Ch || [Ch] <- rows(["SELECT channel FROM channel_stat"
                         " ORDER BY last DESC, channel ASC LIMIT ",
                         integer_to_list(Limit)], [])];
recent(_Limit) ->
    [].

rows(Sql, Params) ->
    try ssb_store:q(Sql, Params) of
        L when is_list(L) -> L;
        _                 -> []
    catch _:_ -> []          %% store down: no index, never a crash
    end.

write(Sql, Params) ->
    catch ssb_store:write(Sql, Params).

encode_json(Term) ->
    iolist_to_binary(message:ssb_encoder(Term, fun message:ssb_encoder/3, [pretty])).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

%% Whether Feed (default: the node owner) currently subscribes to Ch.
%%
%% Test-only.  suggest/2 used to call this per result; it now resolves the
%% owner's subscription inside its own query, which left this with no
%% production caller — and an unused function is a hard error under the
%% prod profile's warnings_as_errors.  Kept here rather than deleted
%% because the toggle tests below read better as a predicate than as
%% lists:member/2 over subscribers/1.
is_subscribed(Ch) ->
    is_subscribed(Ch, owner()).

is_subscribed(Ch, Feed) when is_binary(Ch), is_binary(Feed) ->
    rows("SELECT 1 FROM channel_sub"
         " WHERE channel=?1 AND feed=?2 AND subscribed=1", [Ch, Feed]) =/= [];
is_subscribed(_Ch, _Feed) ->
    false.

channels_test_() ->
    {setup, fun ch_setup/0, fun ch_teardown/1,
     fun(_) -> [?_test(index_suggest_recent()),
                ?_test(subscription_tracked()),
                ?_test(subscribers_are_per_channel()),
                ?_test(older_toggle_does_not_win()),
                ?_test(survives_a_restart())] end}.

ch_setup() ->
    ch_teardown(ignore),
    Home = filename:join("/tmp", "ch_" ++
                          integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = ssb_store:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = view_manager:start_link(),
    {ok, _} = silkpurse_channels:start_link(),
    ok = wait_view_ready(silkpurse_channels),
    Home.

%% Registration lands after start_link/0 returns, and registering a view
%% whose state is not marked complete resets it — so a test asserting on
%% the index must wait, or the reset arrives mid-test.  caught_up/1 alone
%% answers true for a module that has not registered at all, which is the
%% window being waited out.
wait_view_ready(Mod) ->
    wait_view_ready(Mod, 250).

wait_view_ready(Mod, 0) ->
    error({view_never_ready, Mod});
wait_view_ready(Mod, N) ->
    case lists:member(Mod, view_manager:views())
        andalso view_manager:caught_up(Mod) of
        true  -> ok;
        false -> timer:sleep(20), wait_view_ready(Mod, N - 1)
    end.

ch_teardown(Home) ->
    [catch gen_server:stop(N)
     || N <- [silkpurse_channels, view_manager, ssb_feed_sup, blobs,
              mess_auth, ssb_store, keys, config]],
    case Home of
        ignore -> ok;
        _ -> os:cmd("rm -rf " ++ Home), application:unset_env(ssb, ssb_home)
    end,
    ok.

post_in(Pid, Channel) ->
    ok = ssb_feed:post_content(
           Pid, {[{~"type", ~"post"}, {~"text", ~"hi"}, {~"channel", Channel}]}),
    ok.

sub_in(Pid, Channel, Subscribed) ->
    ok = ssb_feed:post_content(
           Pid, {[{~"type", ~"channel"}, {~"channel", Channel},
                  {~"subscribed", Subscribed}]}),
    ok.

index_suggest_recent() ->
    Pid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    post_in(Pid, ~"#general"),
    post_in(Pid, ~"#Random"),
    post_in(Pid, ~"#general"),
    %% suggest: general has 2 posts, random 1; #-stripped, lowercased
    ?assertMatch([{[{~"id", ~"general"}, {~"count", 2} | _]}],
                 suggest(~"gen", 20)),
    %% recent lists the active channels (exact order is by timestamp, but
    %% these posts share a millisecond so compare as a set)
    ?assertEqual([~"general", ~"random"], lists:sort(recent(10))).

subscription_tracked() ->
    Pid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    post_in(Pid, ~"#elm"),
    ?assertNot(is_subscribed(~"elm")),
    sub_in(Pid, ~"#elm", true),
    ?assert(is_subscribed(~"elm")),
    sub_in(Pid, ~"#elm", false),
    ?assertNot(is_subscribed(~"elm")),
    %% suggest reflects the (now unsubscribed) state
    ?assertMatch([{[{~"id", ~"elm"}, {~"count", 1}, {~"subscribed", false}]}],
                 suggest(~"elm", 20)),
    %% subscribe again and check the subscribers list
    sub_in(Pid, ~"#elm", true),
    ?assertEqual([keys:pub_key_disp()], subscribers(~"elm")).

%% subscribers/1 used to fold the entire index — stats and every other
%% channel's subscriptions included — to answer for one channel.  It must
%% return that channel's subscribers and only those.
subscribers_are_per_channel() ->
    A = ~"@subaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa=.ed25519",
    B = ~"@subbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb=.ed25519",
    bump(~"scoped", 1),
    bump(~"other", 1),
    set_sub(~"scoped", A, true, 10),
    set_sub(~"scoped", B, true, 10),
    set_sub(~"other",  A, true, 10),
    ?assertEqual([A, B], lists:sort(subscribers(~"scoped"))),
    ?assertEqual([A], subscribers(~"other")),
    ?assertEqual([], subscribers(~"never-seen")),
    %% and an unsubscribe removes only that pair
    set_sub(~"scoped", A, false, 20),
    ?assertEqual([B], subscribers(~"scoped")),
    ?assertEqual([A], subscribers(~"other")).

%% Toggles are resolved on timestamp, so a stale one arriving late must
%% not overwrite the current state.  An equal timestamp does win: feeds
%% fold in sequence order, so the later-folded message is the later one.
older_toggle_does_not_win() ->
    Ch = ~"toggles",
    F  = ~"@togglerrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr=.ed25519",
    bump(Ch, 1),
    set_sub(Ch, F, true, 100),
    ?assert(is_subscribed(Ch, F)),
    set_sub(Ch, F, false, 50),           %% stale: ignored
    ?assert(is_subscribed(Ch, F)),
    set_sub(Ch, F, false, 100),          %% same instant: accepted
    ?assertNot(is_subscribed(Ch, F)),
    set_sub(Ch, F, true, 150),           %% newer: accepted
    ?assert(is_subscribed(Ch, F)).

%% The point of the port: durable as written, with no snapshot step.
survives_a_restart() ->
    Ch = ~"persisted",
    F  = ~"@persistsubbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb=.ed25519",
    bump(Ch, 7),
    bump(Ch, 9),
    set_sub(Ch, F, true, 9),
    ok = gen_server:stop(ssb_store),
    {ok, _} = ssb_store:start_link(),
    ?assertMatch([{[{~"id", Ch}, {~"count", 2} | _]}], suggest(~"persist", 20)),
    ?assertEqual([F], subscribers(Ch)),
    ?assert(lists:member(Ch, recent(50))).

-endif.
