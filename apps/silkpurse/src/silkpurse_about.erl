%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% About index: the network's assignments of profile fields (name,
%% image, description) to a feed or message, resolved to a single value
%% the way ssb-social-index / patchcore does.  This is what renders
%% names, avatars and descriptions in the UI.
%%
%% An ssb_view over a named public ETS set {{Dest, Key} => #{Author =>
%% Value}} — each author's latest assignment, a {remove: true} pruning
%% the author's entry — plus an ssb_plugin serving:
%%   about.socialValue({dest, key})        async, owner
%%   about.socialValueStream({dest, key})   source (live), owner
%%
%% Resolution (getSocialValue, matching ssb-social-index exactly):
%%   1. the node owner's own assignment, else
%%   2. the described feed's own self-assignment, else
%%   3. the most common value across all assigners (plurality).
%%
%% Not yet served: socialValuesStream/groupedValues ("also known as"
%% alternate names) and the latest-family (latestValue/latestValues) —
%% the latter have no UI callers today.
-module(silkpurse_about).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).
-behaviour(ssb_view).
-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

%% API
-export([start_link/0]).

%% ssb_view callbacks
-export([view_version/0, view_load/0, view_reset/0, view_save/0,
         view_entry/1]).

%% ssb_plugin callbacks
-export([manifest/0, handle_rpc/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         handle_info/2, terminate/2, code_change/3]).

-define(TAB, silkpurse_about).
-define(MARKER, '$complete').

%% The about fields the UI reads; a bounded set keeps the index small.
-define(KEYS, [~"name", ~"image", ~"description"]).

%%%===================================================================
%%% API
%%%===================================================================

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
    case Loaded of
        true  -> ok;
        false -> empty
    end.

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
        ~"about" ->
            case ?pgv(~"about", Props) of
                Dest when is_binary(Dest) ->
                    Changed = [apply_field(Dest, Key, Author, ?pgv(Key, Props))
                               || Key <- ?KEYS, ?pgv(Key, Props) =/= undefined],
                    case [E || {changed, E} <- Changed] of
                        []     -> ok;
                        Events -> {events, Events}
                    end;
                _ -> ok
            end;
        _ -> ok
    end;
view_entry(_) ->
    ok.

%%%===================================================================
%%% ssb_plugin callbacks (run in each connection's rpc_processor)
%%%===================================================================

manifest() ->
    [{[~"about", ~"socialValue"],       async,  owner},
     {[~"about", ~"socialValueStream"], source, owner}].

handle_rpc([~"about", ~"socialValue"], Args, _Caller) ->
    case dest_key(Args) of
        {Dest, Key} -> {reply, social_value(Dest, Key)};
        undefined   -> {error, ~"about.socialValue needs dest and key"}
    end;

handle_rpc([~"about", ~"socialValueStream"], Args, _Caller) ->
    case dest_key(Args) of
        undefined ->
            {error, ~"about.socialValueStream needs dest and key"};
        {Dest, Key} ->
            Initial = encode_value(social_value(Dest, Key)),
            EventFun =
                fun({about, D, K}) when D =:= Dest, K =:= Key ->
                        {send, encode_value(social_value(Dest, Key))};
                   (_) -> skip
                end,
            %% snapshot = the current resolved value (a value stream, so
            %% no message-id dedup); then live updates on each change
            {live_source, [{make_ref(), Initial}], ?MODULE, EventFun}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Restored = try ets:file2tab(?b2l(table_file()))
               catch _:_ -> {error, no_config}
               end,
    case Restored of
        {ok, ?TAB} -> ok;
        _          -> ets:new(?TAB, [set, named_table, public])
    end,
    {ok, #{}, {continue, register}}.

handle_continue(register, State) ->
    %% Register the plugin (method) before the view fold so the method
    %% is in the served manifest immediately (the table was restored in
    %% init).  Guard each independently so a service that is down (bare
    %% eunit setups) does not skip the other.
    try plugin_registry:register_plugin(?MODULE)
    catch exit:{noproc, _} -> ok end,
    try view_manager:register_view(?MODULE)
    catch exit:{noproc, _} -> ok end,
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    catch view_save(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

table_file() ->
    <<(config:ssb_repo_loc())/binary, "views/about.tab">>.

%% Apply one author's assignment of Key for Dest; a {remove: true}
%% value drops the author's entry.  Returns {changed, Event} when the
%% stored map actually changed, unchanged otherwise.
apply_field(Dest, Key, Author, Value) ->
    TabKey = {Dest, Key},
    Cur = case ets:lookup(?TAB, TabKey) of
              [{TabKey, Map}] -> Map;
              []              -> #{}
          end,
    New = case is_remove(Value) of
              true  -> maps:remove(Author, Cur);
              false -> Cur#{Author => Value}
          end,
    case New =:= Cur of
        true  -> unchanged;
        false ->
            ets:insert(?TAB, {TabKey, New}),
            {changed, {about, Dest, Key}}
    end.

is_remove({Props}) when is_list(Props) ->
    ?pgv(~"remove", Props) =:= true;
is_remove(_) ->
    false.

%% getSocialValue: node owner's assignment, else the described feed's
%% own, else plurality.  Returns the raw value item, or null.
social_value(Dest, Key) ->
    Values = case ets:lookup(?TAB, {Dest, Key}) of
                 [{_, Map}] -> Map;
                 []         -> #{}
             end,
    Yours = keys:pub_key_disp(),
    Author = author_of(Dest),
    case Values of
        #{Yours := V}  -> V;
        #{Author := V} -> V;
        _              -> highest_rank(Values)
    end.

%% The most common extractable value across assigners, or null.
highest_rank(Values) ->
    Counts = maps:fold(
               fun(_Author, Item, Acc) ->
                       case extract(Item) of
                           undefined -> Acc;
                           V -> maps:update_with(V, fun(N) -> N + 1 end, 1, Acc)
                       end
               end, #{}, Values),
    case maps:fold(fun(V, N, {_BV, BN}) when N > BN -> {V, N};
                      (_V, _N, Best) -> Best
                   end, {null, 0}, Counts) of
        {Best, _} -> Best
    end.

%% Comparable value from a raw item: a plain string, or a blob link.
extract(Item) when is_binary(Item) -> Item;
extract({Props}) when is_list(Props) ->
    case ?pgv(~"link", Props) of
        L when is_binary(L) -> L;
        _                   -> undefined
    end;
extract(_) ->
    undefined.

%% The feed a dest "belongs to": a feed id is its own author; a message
%% id resolves through mess_auth; unknown falls back to the dest itself
%% (matching ssb-social-index's getAuthor fallback).
author_of(<<"@", _/binary>> = Dest) ->
    Dest;
author_of(Dest) ->
    case mess_auth:get(Dest) of
        not_found -> Dest;
        Author    -> Author
    end.

encode_value(null) ->
    encode_json(null);
encode_value(Value) ->
    encode_json(Value).

encode_json(Term) ->
    iolist_to_binary(message:ssb_encoder(Term, fun message:ssb_encoder/3, [pretty])).

%% {dest, key} from a [{[{"dest",D},{"key",K}]}] argument.
dest_key([{Props}]) ->
    case {?pgv(~"dest", Props), ?pgv(~"key", Props)} of
        {D, K} when is_binary(D), is_binary(K) -> {D, K};
        _                                      -> undefined
    end;
dest_key(_) ->
    undefined.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

extract_test() ->
    ?assertEqual(~"alice", extract(~"alice")),
    ?assertEqual(~"&blob.sha256", extract({[{~"link", ~"&blob.sha256"}]})),
    ?assertEqual(undefined, extract({[{~"foo", ~"bar"}]})).

is_remove_test() ->
    ?assert(is_remove({[{~"remove", true}]})),
    ?assertNot(is_remove({[{~"link", ~"x"}]})),
    ?assertNot(is_remove(~"alice")).

resolution_test_() ->
    {setup, fun ab_setup/0, fun ab_teardown/1,
     fun(_) ->
             [?_test(self_wins()),
              ?_test(author_wins_without_self()),
              ?_test(plurality_without_self_or_author()),
              ?_test(remove_falls_back()),
              ?_test(live_pushes_on_change())]
     end}.

ab_setup() ->
    ab_teardown(ignore),
    Home = filename:join("/tmp", "ab_" ++
                          integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = view_manager:start_link(),
    {ok, _} = silkpurse_about:start_link(),
    Home.

ab_teardown(Home) ->
    [catch gen_server:stop(Name)
     || Name <- [silkpurse_about, view_manager, ssb_feed_sup,
                 blobs, mess_auth, keys, config]],
    case Home of
        ignore -> ok;
        _ ->
            os:cmd("rm -rf " ++ Home),
            application:unset_env(ssb, ssb_home)
    end,
    ok.

%% Seed the index directly (bypassing feed storage) for resolution logic.
put_about(Dest, Key, Author, Value) ->
    apply_field(Dest, Key, Author, Value).

self_wins() ->
    Yours = keys:pub_key_disp(),
    Dest  = ~"@feeddddddddddddddddddddddddddddddddddddddd=.ed25519",
    Other = ~"@otherrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr=.ed25519",
    put_about(Dest, ~"name", Other, ~"they call me"),
    put_about(Dest, ~"name", Dest,  ~"self assigned"),
    put_about(Dest, ~"name", Yours, ~"my nickname for them"),
    ?assertEqual(~"my nickname for them", social_value(Dest, ~"name")).

author_wins_without_self() ->
    Dest  = ~"@feed2ddddddddddddddddddddddddddddddddddddd=.ed25519",
    Other = ~"@other2rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr=.ed25519",
    put_about(Dest, ~"name", Other, ~"nickname"),
    put_about(Dest, ~"name", Dest,  ~"my real name"),
    ?assertEqual(~"my real name", social_value(Dest, ~"name")).

plurality_without_self_or_author() ->
    Dest = ~"@feed3ddddddddddddddddddddddddddddddddddddd=.ed25519",
    A = ~"@aaaa3aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa=.ed25519",
    B = ~"@bbbb3bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb=.ed25519",
    C = ~"@cccc3ccccccccccccccccccccccccccccccccccccc=.ed25519",
    put_about(Dest, ~"name", A, ~"popular"),
    put_about(Dest, ~"name", B, ~"popular"),
    put_about(Dest, ~"name", C, ~"lonely"),
    ?assertEqual(~"popular", social_value(Dest, ~"name")).

remove_falls_back() ->
    Yours = keys:pub_key_disp(),
    Dest  = ~"@feed4ddddddddddddddddddddddddddddddddddddd=.ed25519",
    put_about(Dest, ~"name", Dest,  ~"self"),
    put_about(Dest, ~"name", Yours, ~"mine"),
    ?assertEqual(~"mine", social_value(Dest, ~"name")),
    %% remove my assignment -> falls back to the feed's own
    put_about(Dest, ~"name", Yours, {[{~"remove", true}]}),
    ?assertEqual(~"self", social_value(Dest, ~"name")).

live_pushes_on_change() ->
    %% view_entry over a real about message emits a change event
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    ok = view_manager:subscribe(silkpurse_about),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"about"},
                                         {~"about", OwnId},
                                         {~"name", ~"live name"}]}),
    receive
        {view_event, silkpurse_about, {about, OwnId, ~"name"}} -> ok
    after 1000 ->
        error(no_about_event)
    end,
    ?assertEqual(~"live name", social_value(OwnId, ~"name")),
    ok = view_manager:unsubscribe(silkpurse_about).

-endif.