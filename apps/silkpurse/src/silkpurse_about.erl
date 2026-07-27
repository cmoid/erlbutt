%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% About index: the network's assignments of profile fields (name,
%% image, description) to a feed or message, resolved to a single value
%% the way ssb-social-index / patchcore does.  This is what renders
%% names, avatars and descriptions in the UI.
%%
%% An ssb_view over ssb_store holding one row per assignment — (dest,
%% key, author) -> value, a {remove: true} deleting the row — plus an
%% ssb_plugin serving:
%%   about.socialValue({dest, key})          async, owner
%%   about.socialValueStream({dest, key})    source (live), owner
%%   about.socialValuesStream({dest, key})   source (live), owner
%%   about.latestValueStream({dest, key, authorId?}) source (live), owner
%%
%% Resolution (getSocialValue, matching ssb-social-index exactly):
%%   1. the node owner's own assignment, else
%%   2. the described feed's own self-assignment, else
%%   3. the most common value across all assigners (plurality).
%%
%% socialValuesStream is the "also known as" backing: one snapshot
%% frame with every author's assignment ({author: value}), then a
%% single-pair frame per change — a remove is sent raw so the client's
%% checkDelete drops the author (MutantPullDict semantics).
%%
%% APPROXIMATION: latestValueStream without authorId is defined in JS
%% as "the value set by whoever assigned last"; the view keeps no
%% assignment order, so we serve the resolved social value instead.
%% With authorId it is exact (that author's current assignment).
%%
%% Not yet served: the latest-family getters (latestValue/latestValues)
%% — no UI callers today.
%%
%% A row per assigner rather than a serialised map per {dest, key}: the
%% resolution above is a fold over assigners, so the map was the shape
%% ETS forced rather than the shape the data wants.  One row each makes
%% search_names/2 an indexed scan instead of a fold over every entry in
%% the index, and leaves plurality expressible as a GROUP BY if it ever
%% needs to be.
-module(silkpurse_about).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).
-behaviour(ssb_view).
-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

%% API
-export([start_link/0, social_value/2, search_names/2]).

%% ssb_view callbacks
-export([view_version/0, view_load/0, view_reset/0, view_save/0,
         view_entry/1]).

%% ssb_plugin callbacks
-export([manifest/0, handle_rpc/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SCHEMA_VERSION, 1).
-define(DDL,
        ["CREATE TABLE IF NOT EXISTS about_assign("
         "  dest   TEXT NOT NULL,"
         "  key    TEXT NOT NULL,"
         "  author TEXT NOT NULL,"
         "  value  TEXT,"
         "  json   INTEGER NOT NULL DEFAULT 0,"
         "  PRIMARY KEY (dest, key, author)) WITHOUT ROWID;",
         %% key first: search_names/2 asks for every dest carrying a
         %% `name`, which the primary key (dest-first) cannot serve.
         "CREATE INDEX IF NOT EXISTS ix_about_key"
         "  ON about_assign(key, dest);"]).

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
    case ssb_store:complete(?MODULE) of
        true  -> ok;
        false -> empty
    end.

view_reset() ->
    _ = ssb_store:clear_complete(?MODULE),
    _ = ssb_store:exec("DELETE FROM about_assign;"),
    ok.

%% Rows are already durable; the only thing to record is that this view's
%% state is complete up to the manager's checkpoints.
view_save() ->
    _ = ssb_store:mark_complete(?MODULE),
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
    [{[~"about", ~"socialValue"],        async,  owner},
     {[~"about", ~"socialValueStream"],  source, owner},
     {[~"about", ~"socialValuesStream"], source, owner},
     {[~"about", ~"latestValueStream"],  source, owner},
     {[~"patchwork", ~"profile", ~"avatar"], async, owner}].

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
                fun({about, D, K, _A, _V}) when D =:= Dest, K =:= Key ->
                        {send, encode_value(social_value(Dest, Key))};
                   (_) -> skip
                end,
            %% snapshot = the current resolved value (a value stream, so
            %% no message-id dedup); then live updates on each change
            {live_source, [{make_ref(), Initial}], ?MODULE, EventFun}
    end;

handle_rpc([~"about", ~"socialValuesStream"], Args, _Caller) ->
    case dest_key(Args) of
        undefined ->
            {error, ~"about.socialValuesStream needs dest and key"};
        {Dest, Key} ->
            Initial = encode_json(values_object(Dest, Key)),
            EventFun =
                fun({about, D, K, Author, Value}) when D =:= Dest,
                                                        K =:= Key ->
                        %% single-pair diff; removes go through raw so
                        %% the client deletes the author's entry
                        {send, encode_json({[{Author, Value}]})};
                   (_) -> skip
                end,
            {live_source, [{make_ref(), Initial}], ?MODULE, EventFun}
    end;

handle_rpc([~"about", ~"latestValueStream"], [{Props}] = Args, _Caller) ->
    case dest_key(Args) of
        undefined ->
            {error, ~"about.latestValueStream needs dest and key"};
        {Dest, Key} ->
            AuthorId = ?pgv(~"authorId", Props),
            Current = fun() -> latest_value(Dest, Key, AuthorId) end,
            EventFun =
                fun({about, D, K, _A, _V}) when D =:= Dest, K =:= Key ->
                        {send, encode_value(Current())};
                   (_) -> skip
                end,
            {live_source, [{make_ref(), encode_value(Current())}],
             ?MODULE, EventFun}
    end;

%% profile.avatar({id}) -> {id, name, image}: a feed's resolved display
%% name and avatar, for profile headers and lists.
handle_rpc([~"patchwork", ~"profile", ~"avatar"], [{Opts}], _Caller) ->
    case ?pgv(~"id", Opts) of
        Id when is_binary(Id) ->
            {reply, {[{~"id",    Id},
                      {~"name",  social_value(Id, ~"name")},
                      {~"image", social_value(Id, ~"image")}]}};
        _ ->
            {error, ~"profile.avatar needs an id"}
    end.

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

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

rows(Sql, Params) ->
    try ssb_store:q(Sql, Params) of
        L when is_list(L) -> L;
        _                 -> []
    catch _:_ -> []          %% store down: no index, never a crash
    end.

%% Apply one author's assignment of Key for Dest; a {remove: true}
%% value drops the author's row.  Returns {changed, Event} when the
%% stored state actually changed, unchanged otherwise.
%%
%% The current value is read first because the caller only emits an event
%% on a real change, and a stream that fires on every restatement of the
%% same name would push a frame per about message to every subscriber.
apply_field(Dest, Key, Author, Value) ->
    Cur = assignment(Dest, Key, Author),
    Changed =
        case is_remove(Value) of
            true ->
                Cur =/= undefined andalso
                    (catch ssb_store:write("DELETE FROM about_assign"
                                           " WHERE dest=?1 AND key=?2"
                                           " AND author=?3",
                                           [Dest, Key, Author]) =:= ok);
            false ->
                Cur =/= Value andalso put_assignment(Dest, Key, Author, Value)
        end,
    case Changed of
        true ->
            %% carry the author and RAW value (removes included) so
            %% socialValuesStream can emit exact single-pair diffs
            {changed, {about, Dest, Key, Author, Value}};
        _ ->
            unchanged
    end.

put_assignment(Dest, Key, Author, Value) ->
    {Enc, Json} = encode_stored(Value),
    catch ssb_store:write(
            "INSERT INTO about_assign(dest, key, author, value, json)"
            " VALUES(?1, ?2, ?3, ?4, ?5)"
            " ON CONFLICT(dest, key, author) DO UPDATE SET"
            "   value=excluded.value, json=excluded.json",
            [Dest, Key, Author, Enc, Json]) =:= ok.

%% One author's current assignment, or undefined.
assignment(Dest, Key, Author) ->
    case rows("SELECT value, json FROM about_assign"
              " WHERE dest=?1 AND key=?2 AND author=?3", [Dest, Key, Author]) of
        [[V, J]] -> decode_stored(V, J);
        _        -> undefined
    end.

%% Every author's current assignment for {Dest, Key}.
values_map(Dest, Key) ->
    maps:from_list([{A, decode_stored(V, J)}
                    || [A, V, J] <- rows("SELECT author, value, json"
                                         " FROM about_assign"
                                         " WHERE dest=?1 AND key=?2",
                                         [Dest, Key])]).

%% Strings stay strings so the column remains directly queryable;
%% anything else (patchwork's {link: …} images) becomes JSON with a flag.
encode_stored(V) when is_binary(V) ->
    {V, 0};
encode_stored(V) ->
    try {iolist_to_binary(message:ssb_encoder(V, fun message:ssb_encoder/3, [])), 1}
    catch _:_ -> {undefined, 0}
    end.

decode_stored(V, 0) ->
    V;
decode_stored(V, 1) when is_binary(V) ->
    try utils:nat_decode(V) catch _:_ -> undefined end;
decode_stored(_V, _J) ->
    undefined.

is_remove({Props}) when is_list(Props) ->
    ?pgv(~"remove", Props) =:= true;
is_remove(_) ->
    false.

%% Every author's current assignment for {Dest, Key} as a JSON object.
values_object(Dest, Key) ->
    {maps:to_list(values_map(Dest, Key))}.

%% latestValueStream's value: exact for a given author; the resolved
%% social value otherwise (see the module-doc approximation note).
latest_value(Dest, Key, AuthorId) when is_binary(AuthorId) ->
    case assignment(Dest, Key, AuthorId) of
        undefined -> null;
        V         -> V
    end;
latest_value(Dest, Key, _) ->
    social_value(Dest, Key).

%% getSocialValue: node owner's assignment, else the described feed's
%% own, else plurality.  Returns the raw value item, or null.
social_value(Dest, Key) ->
    Values = values_map(Dest, Key),
    Yours = keys:pub_key_disp(),
    Author = author_of(Dest),
    case Values of
        #{Yours := V}  -> V;
        #{Author := V} -> V;
        _              -> highest_rank(Values)
    end.

%% Feeds whose resolved display name contains Text (case-insensitive),
%% up to Limit, as [{FeedId, Name}] — the backing for mention
%% autocomplete.  Uses the resolved social value, so pet-names the owner
%% assigned are searchable too.
%% Matching stays in Erlang rather than becoming a SQL LIKE: string:find
%% on a string:lowercase'd binary is Unicode-aware, and SQLite's lower()
%% is ASCII-only, so pushing it down would quietly stop matching any name
%% that is not plain ASCII.  What the port does buy is the candidate set —
%% an indexed scan for dests carrying a `name`, where this used to fold
%% every entry in the whole index regardless of key.
search_names(Text, Limit) ->
    Needle = string:lowercase(Text),
    Dests = [D || [D] <- rows("SELECT DISTINCT dest FROM about_assign"
                              " WHERE key=?1", [~"name"])],
    Matches = lists:filtermap(
                fun(Dest) ->
                        case social_value(Dest, ~"name") of
                            Name when is_binary(Name) ->
                                case string:find(string:lowercase(Name),
                                                 Needle) of
                                    nomatch -> false;
                                    _       -> {true, {Dest, Name}}
                                end;
                            _ -> false
                        end
                end, Dests),
    lists:sublist(Matches, Limit).

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
              ?_test(live_pushes_on_change()),
              ?_test(social_values_stream()),
              ?_test(latest_value_stream()),
              ?_test(restating_a_value_is_not_a_change()),
              ?_test(remove_deletes_the_row()),
              ?_test(search_finds_the_resolved_name()),
              ?_test(survives_a_restart())]
     end}.

ab_setup() ->
    ab_teardown(ignore),
    Home = filename:join("/tmp", "ab_" ++
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
    {ok, _} = silkpurse_about:start_link(),
    Home.

ab_teardown(Home) ->
    [catch gen_server:stop(Name)
     || Name <- [silkpurse_about, view_manager, ssb_feed_sup,
                 blobs, mess_auth, ssb_store, keys, config]],
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
        {view_event, silkpurse_about, {about, OwnId, ~"name", OwnId,
                                       ~"live name"}} -> ok
    after 1000 ->
        error(no_about_event)
    end,
    ?assertEqual(~"live name", social_value(OwnId, ~"name")),
    ok = view_manager:unsubscribe(silkpurse_about).

social_values_stream() ->
    Dest = ~"@svsfeedddddddddddddddddddddddddddddddddddd=.ed25519",
    A = ~"@svsaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa=.ed25519",
    put_about(Dest, ~"name", A,    ~"ay"),
    put_about(Dest, ~"name", Dest, ~"me"),
    {live_source, [{_, Snap}], ?MODULE, EventFun} =
        handle_rpc([~"about", ~"socialValuesStream"],
                   [{[{~"dest", Dest}, {~"key", ~"name"}]}], caller()),
    {Props} = utils:nat_decode(Snap),
    ?assertEqual(~"ay", ?pgv(A, Props)),
    ?assertEqual(~"me", ?pgv(Dest, Props)),
    %% live diff: one {author: value} pair, removes passed through raw
    Remove = {[{~"remove", true}]},
    {send, Diff} = EventFun({about, Dest, ~"name", A, Remove}),
    {[{A, {DiffVal}}]} = utils:nat_decode(Diff),
    ?assertEqual(true, ?pgv(~"remove", DiffVal)),
    ?assertEqual(skip, EventFun({about, ~"@other", ~"name", A, ~"x"})).

latest_value_stream() ->
    Dest = ~"@lvsfeedddddddddddddddddddddddddddddddddddd=.ed25519",
    A = ~"@lvsaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa=.ed25519",
    put_about(Dest, ~"title", A,    ~"their title"),
    put_about(Dest, ~"title", Dest, ~"own title"),
    %% authorId: that author's assignment exactly
    {live_source, [{_, ForA}], ?MODULE, _} =
        handle_rpc([~"about", ~"latestValueStream"],
                   [{[{~"dest", Dest}, {~"key", ~"title"},
                      {~"authorId", A}]}], caller()),
    ?assertEqual(~"their title", utils:nat_decode(ForA)),
    %% without authorId: the resolved social value (documented approx)
    {live_source, [{_, Resolved}], ?MODULE, _} =
        handle_rpc([~"about", ~"latestValueStream"],
                   [{[{~"dest", Dest}, {~"key", ~"title"}]}], caller()),
    ?assertEqual(~"own title", utils:nat_decode(Resolved)).

%% Why apply_field/4 reads before it writes: an about message restating a
%% name it already asserted must not be reported as a change, or every
%% subscriber gets a frame per about message.
restating_a_value_is_not_a_change() ->
    Dest = ~"@rstfeedddddddddddddddddddddddddddddddddddd=.ed25519",
    A    = ~"@rstaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa=.ed25519",
    ?assertMatch({changed, _}, put_about(Dest, ~"name", A, ~"same")),
    ?assertEqual(unchanged,    put_about(Dest, ~"name", A, ~"same")),
    ?assertMatch({changed, _}, put_about(Dest, ~"name", A, ~"different")).

%% A remove drops the row rather than storing a {remove: true} value —
%% otherwise resolution would have to filter it out on every read.
remove_deletes_the_row() ->
    Dest = ~"@rmvfeedddddddddddddddddddddddddddddddddddd=.ed25519",
    A    = ~"@rmvaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa=.ed25519",
    put_about(Dest, ~"name", A, ~"briefly"),
    ?assertEqual([[1]], ssb_store:q("SELECT count(*) FROM about_assign"
                                    " WHERE dest=?1 AND key=?2", [Dest, ~"name"])),
    ?assertMatch({changed, _}, put_about(Dest, ~"name", A, {[{~"remove", true}]})),
    ?assertEqual([[0]], ssb_store:q("SELECT count(*) FROM about_assign"
                                    " WHERE dest=?1 AND key=?2", [Dest, ~"name"])),
    %% and removing what is not there is not a change
    ?assertEqual(unchanged, put_about(Dest, ~"name", A, {[{~"remove", true}]})).

%% Search resolves before matching, so a pet-name the owner assigned is
%% findable even though the feed calls itself something else.
search_finds_the_resolved_name() ->
    Yours = keys:pub_key_disp(),
    Dest  = ~"@srchfeeddddddddddddddddddddddddddddddddddd=.ed25519",
    put_about(Dest, ~"name", Dest,  ~"selfchosen"),
    put_about(Dest, ~"name", Yours, ~"petname"),
    ?assertEqual([{Dest, ~"petname"}], search_names(~"PETNA", 10)),
    %% the overridden self-assignment is not what search sees
    ?assertEqual([], search_names(~"selfchosen", 10)),
    %% other keys are not candidates
    put_about(Dest, ~"description", Yours, ~"petname too"),
    ?assertEqual(1, length(search_names(~"petname", 10))).

%% The point of the port: durable as written, with no snapshot step.
survives_a_restart() ->
    Dest = ~"@prsfeedddddddddddddddddddddddddddddddddddd=.ed25519",
    A    = ~"@prsaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa=.ed25519",
    Img  = {[{~"link", ~"&blob.sha256"}]},
    put_about(Dest, ~"name",  A, ~"persisted"),
    put_about(Dest, ~"image", A, Img),
    ok = gen_server:stop(ssb_store),
    {ok, _} = ssb_store:start_link(),
    ?assertEqual(~"persisted", social_value(Dest, ~"name")),
    %% A non-string value comes back the shape it went in.  Asked for
    %% through latest_value/3, not social_value/2: with no owner or self
    %% assignment the latter resolves by plurality, which compares on
    %% extract/1 and so answers with the bare link.
    ?assertEqual(Img, latest_value(Dest, ~"image", A)),
    ?assertEqual(~"&blob.sha256", social_value(Dest, ~"image")).

caller() ->
    #{class => owner, feed_id => keys:pub_key_disp()}.

-endif.