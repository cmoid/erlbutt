%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Likes index: who currently likes each message.  SSB likes are `vote`
%% messages — {type: vote, vote: {link: Target, value: N}} — where a
%% positive value is a like and a non-positive value retracts it.  The
%% view tracks, per target, the set of authors who currently like it.
%%
%% An ssb_view over a named public ETS set  Target => #{Author => true}
%% plus an ssb_plugin serving the patchwork.likes surface:
%%   likes.get({dest})                     async  -> [likerId]
%%   likes.countStream({dest})             source -> live like count
%%   likes.feedLikesMsgStream({msgId,feedId}) source -> live "you like it"
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

-define(TAB, silkpurse_likes).
-define(MARKER, '$complete').

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% ssb_view callbacks (run in the view_manager process)
%%%===================================================================

view_version() -> 1.

view_load() ->
    Loaded = try ets:lookup(?TAB, ?MARKER) =/= []
             catch error:badarg -> false
             end,
    case Loaded of true -> ok; false -> empty end.

view_reset() ->
    ets:delete_all_objects(?TAB),
    ok.

view_save() ->
    ets:insert(?TAB, {?MARKER}),
    File = table_file(),
    filelib:ensure_dir(File),
    ok = ets:tab2file(?TAB, ?b2l(File)),
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
    Initial = encode_json(length(likers(Dest))),
    EventFun = fun({like, L}) when L =:= Dest ->
                       {send, encode_json(length(likers(Dest)))};
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
    %% Create the (empty) table now so it always exists, but defer the
    %% snapshot restore to handle_continue: file2tab of a large snapshot
    %% would otherwise block silkpurse_sup:start_link and thus the whole
    %% node boot (and the shell).  The restores then run concurrently
    %% across the views rather than serialized by the supervisor.
    ets:new(?TAB, [set, named_table, public]),
    {ok, #{}, {continue, register}}.

handle_continue(register, State) ->
    %% Swap in the snapshot (if any), then register the plugin (so the
    %% method appears in the manifest) before the view fold.  Guard each
    %% registration independently so a service that is down (bare eunit
    %% setups) does not skip the other.
    maybe_restore(),
    try plugin_registry:register_plugin(?MODULE)
    catch exit:{noproc, _} -> ok end,
    try view_manager:register_view(?MODULE)
    catch exit:{noproc, _} -> ok end,
    {noreply, State}.

maybe_restore() ->
    File = ?b2l(table_file()),
    case filelib:is_regular(File) of
        false ->
            ok;                        %% no snapshot; keep the empty table
        true ->
            ets:delete(?TAB),
            case (try ets:file2tab(File) catch _:_ -> error end) of
                {ok, ?TAB} -> ok;
                _          -> ets:new(?TAB, [set, named_table, public])
            end
    end.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> catch view_save(), ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

table_file() ->
    <<(config:ssb_repo_loc())/binary, "views/likes.tab">>.

apply_vote(Link, Author, Value) ->
    Cur = case ets:lookup(?TAB, Link) of
              [{Link, Set}] -> Set;
              []            -> #{}
          end,
    New = case is_integer(Value) andalso Value > 0 of
              true  -> Cur#{Author => true};
              false -> maps:remove(Author, Cur)
          end,
    ets:insert(?TAB, {Link, New}).

likers(Dest) when is_binary(Dest) ->
    case ets:lookup(?TAB, Dest) of
        [{Dest, Set}] -> maps:keys(Set);
        []            -> []
    end;
likers(_) ->
    [].

likes_it(MsgId, FeedId) when is_binary(MsgId), is_binary(FeedId) ->
    case ets:lookup(?TAB, MsgId) of
        [{MsgId, Set}] -> maps:is_key(FeedId, Set);
        []             -> false
    end;
likes_it(_, _) ->
    false.

encode_json(Term) ->
    iolist_to_binary(message:ssb_encoder(Term, fun message:ssb_encoder/3, [pretty])).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

likes_test_() ->
    {setup, fun lk_setup/0, fun lk_teardown/1,
     fun(_) -> [?_test(like_and_unlike())] end}.

lk_setup() ->
    lk_teardown(ignore),
    Home = filename:join("/tmp", "lk_" ++
                          integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = view_manager:start_link(),
    {ok, _} = silkpurse_likes:start_link(),
    Home.

lk_teardown(Home) ->
    [catch gen_server:stop(N)
     || N <- [silkpurse_likes, view_manager, ssb_feed_sup, blobs,
              mess_auth, keys, config]],
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

-endif.
