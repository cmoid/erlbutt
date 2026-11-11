%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Likes index: who currently likes each message.  SSB likes are `vote`
%% messages — {type: vote, vote: {link: Target, value: N}} — where a
%% positive value is a like and a non-positive value retracts it.  The
%% view tracks, per target, the set of authors who currently like it.
%%
%% An ssb_view over ssb_store — a row per (target, author) — plus an
%% ssb_plugin serving the patchwork.likes surface:
%%   likes.get({dest})                     async  -> [likerId]
%%   likes.countStream({dest})             source -> live like count
%%   likes.feedLikesMsgStream({msgId,feedId}) source -> live "you like it"
%%
%% A row per liker rather than a set per target: the two hot questions are
%% "how many like this" and "does this one feed like it", and with the
%% whole set in one value both had to materialise it — countStream rebuilt
%% the entire liker list on every like event just to take its length.
%% They are now a COUNT(*) and a primary-key probe.
-module(silkpurse_likes).

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
        ["CREATE TABLE IF NOT EXISTS msg_like("
         "  target TEXT NOT NULL,"
         "  author TEXT NOT NULL,"
         "  PRIMARY KEY (target, author)) WITHOUT ROWID;"]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% ssb_view callbacks (run in the view_manager process)
%%%===================================================================

view_version() -> 1.

view_load() ->
    case ssb_store:complete(?MODULE) of
        true  -> ok;
        false -> empty
    end.

view_reset() ->
    _ = ssb_store:clear_complete(?MODULE),
    _ = ssb_store:exec("DELETE FROM msg_like;"),
    ok.

%% Rows are already durable; the only thing to record is that this view's
%% state is complete up to the manager's checkpoints.
view_save() ->
    _ = ssb_store:mark_complete(?MODULE),
    ok.

view_entry(#message{author = Author, content = {Props}}) ->
    case ?pgv(~"type", Props) of
        ~"vote" ->
            case ?pgv(~"vote", Props) of
                {VoteProps} ->
                    Link = ?pgv(~"link", VoteProps),
                    Value = ?pgv(~"value", VoteProps),
                    case is_binary(Link) of
                        true ->
                            apply_vote(Link, Author, Value),
                            {events, [{like, Link}]};
                        false -> ok
                    end;
                _ -> ok
            end;
        _ -> ok
    end;
view_entry(_) ->
    ok.

%%%===================================================================
%%% ssb_plugin callbacks
%%%===================================================================

manifest() ->
    [{[~"patchwork", ~"likes", ~"get"],                async,  owner},
     {[~"patchwork", ~"likes", ~"countStream"],        source, owner},
     {[~"patchwork", ~"likes", ~"feedLikesMsgStream"], source, owner}].

handle_rpc([~"patchwork", ~"likes", ~"get"], [{Opts}], _Caller) ->
    {reply, likers(?pgv(~"dest", Opts))};

handle_rpc([~"patchwork", ~"likes", ~"countStream"], [{Opts}], _Caller) ->
    Dest = ?pgv(~"dest", Opts),
    Initial = encode_json(like_count(Dest)),
    EventFun = fun({like, L}) when L =:= Dest ->
                       {send, encode_json(like_count(Dest))};
                  (_) -> skip
               end,
    {live_source, [{make_ref(), Initial}], ?MODULE, EventFun};

handle_rpc([~"patchwork", ~"likes", ~"feedLikesMsgStream"], [{Opts}], _Caller) ->
    MsgId  = ?pgv(~"msgId", Opts),
    FeedId = ?pgv(~"feedId", Opts),
    Initial = encode_json(likes_it(MsgId, FeedId)),
    EventFun = fun({like, L}) when L =:= MsgId ->
                       {send, encode_json(likes_it(MsgId, FeedId))};
                  (_) -> skip
               end,
    {live_source, [{make_ref(), Initial}], ?MODULE, EventFun}.

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

%% A vote restates the author's whole position on the target, so a
%% positive value asserts the row and anything else removes it.  Both are
%% idempotent, which is what makes a redelivered vote harmless.
apply_vote(Link, Author, Value) when is_binary(Author) ->
    case is_integer(Value) andalso Value > 0 of
        true ->
            catch ssb_store:write("INSERT INTO msg_like(target, author)"
                                  " VALUES(?1, ?2)"
                                  " ON CONFLICT(target, author) DO NOTHING",
                                  [Link, Author]);
        false ->
            catch ssb_store:write("DELETE FROM msg_like"
                                  " WHERE target=?1 AND author=?2",
                                  [Link, Author])
    end,
    ok;
apply_vote(_Link, _Author, _Value) ->
    ok.

likers(Dest) when is_binary(Dest) ->
    [A || [A] <- rows("SELECT author FROM msg_like WHERE target=?1", [Dest])];
likers(_) ->
    [].

%% Counted in SQL rather than by measuring likers/1: countStream asks this
%% again on every like of the target, and the list it used to build was
%% discarded immediately.
like_count(Dest) when is_binary(Dest) ->
    case rows("SELECT count(*) FROM msg_like WHERE target=?1", [Dest]) of
        [[N]] when is_integer(N) -> N;
        _                        -> 0
    end;
like_count(_) ->
    0.

likes_it(MsgId, FeedId) when is_binary(MsgId), is_binary(FeedId) ->
    rows("SELECT 1 FROM msg_like WHERE target=?1 AND author=?2",
         [MsgId, FeedId]) =/= [];
likes_it(_, _) ->
    false.

rows(Sql, Params) ->
    try ssb_store:q(Sql, Params) of
        L when is_list(L) -> L;
        _                 -> []
    catch _:_ -> []          %% store down: no index, never a crash
    end.

encode_json(Term) ->
    iolist_to_binary(message:ssb_encoder(Term, fun message:ssb_encoder/3, [pretty])).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

likes_test_() ->
    {setup, fun lk_setup/0, fun lk_teardown/1,
     fun(_) -> [?_test(like_and_unlike()),
                ?_test(count_matches_the_likers()),
                ?_test(survives_a_restart())] end}.

lk_setup() ->
    lk_teardown(ignore),
    Home = filename:join("/tmp", "lk_" ++
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
    {ok, _} = silkpurse_likes:start_link(),
    ok = wait_view_ready(silkpurse_likes),
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

lk_teardown(Home) ->
    [catch gen_server:stop(N)
     || N <- [silkpurse_likes, view_manager, ssb_feed_sup, blobs,
              mess_auth, ssb_store, keys, config]],
    case Home of
        ignore -> ok;
        _ -> os:cmd("rm -rf " ++ Home), application:unset_env(ssb, ssb_home)
    end,
    ok.

vote(Pid, Id, Priv, Prev, Seq, Link, Value) ->
    Content = {[{~"type", ~"vote"},
                {~"vote", {[{~"link", Link}, {~"value", Value}]}}]},
    Msg = message:new_msg(Prev, Seq, Content, {Id, Priv}),
    _ = ssb_feed:store_msg(Pid, Msg),
    ssb_feed:fetch_last_msg(Pid).

like_and_unlike() ->
    #{public := Pub, secret := Priv} = enacl:sign_keypair(),
    Id = <<"@", (base64:encode(Pub))/binary, ".ed25519">>,
    P = base64:encode(Priv),
    Pid = utils:find_or_create_feed_pid(Id),
    Target = ~"%sometargetmsgxxxxxxxxxxxxxxxxxxxxxxxxxxxxx=.sha256",
    ?assertEqual([], likers(Target)),
    ?assertNot(likes_it(Target, Id)),
    #message{id = V1} = vote(Pid, Id, P, null, 1, Target, 1),
    ?assertEqual([Id], likers(Target)),
    ?assert(likes_it(Target, Id)),
    _ = vote(Pid, Id, P, V1, 2, Target, 0),          %% retract
    ?assertEqual([], likers(Target)),
    ?assertNot(likes_it(Target, Id)).

%% countStream serves like_count/1 while likes.get serves likers/1, so the
%% two must not be able to disagree — and a restated vote must not make
%% the count drift, which is what the primary key is for.
count_matches_the_likers() ->
    Target = ~"%counttargetxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx=.sha256",
    A = ~"@likera=.ed25519",
    B = ~"@likerb=.ed25519",
    ?assertEqual(0, like_count(Target)),
    apply_vote(Target, A, 1),
    apply_vote(Target, B, 1),
    apply_vote(Target, A, 1),                    %% the same vote again
    ?assertEqual(2, like_count(Target)),
    ?assertEqual(2, length(likers(Target))),
    apply_vote(Target, B, 0),                    %% retract
    ?assertEqual(1, like_count(Target)),
    ?assertEqual([A], likers(Target)),
    %% retracting what was never asserted is also a no-op
    apply_vote(Target, B, 0),
    ?assertEqual(1, like_count(Target)).

%% The point of the port: durable as written, with no snapshot step.
survives_a_restart() ->
    Target = ~"%persisttargetxxxxxxxxxxxxxxxxxxxxxxxxxxxx=.sha256",
    A = ~"@persistliker=.ed25519",
    apply_vote(Target, A, 1),
    ok = gen_server:stop(ssb_store),
    {ok, _} = ssb_store:start_link(),
    ?assertEqual([A], likers(Target)),
    ?assert(likes_it(Target, A)).

-endif.
