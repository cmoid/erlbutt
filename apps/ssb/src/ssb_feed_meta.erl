%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Core view: the latest self-asserted metadata for each feed.
%%
%% A feed describes itself with `about` messages naming itself
%% (content.about == author).  This view keeps the most recent value of
%% every field such a message carries, as (FeedId, Key) -> {Value, Seq}.
%%
%% The *mechanism* — last-write-wins metadata per feed, folded from the
%% log — is foundational: anything that renders a feed id needs it, and
%% that includes invites, rooms and connection UIs, not just social
%% clients.  The *schema* is not: `name`, `image` and `description` are
%% patchwork conventions.  So no field is privileged here; every key in
%% the message is stored, and callers ask for the ones they know about.
%% An application with entirely different self-description gets the same
%% machinery for free.  See doc/persistence.md §5.
%%
%% Last-write-wins is resolved on sequence number, not on the message's
%% self-asserted timestamp (which is untrustworthy — see §5 on causal
%% ordering).  view_manager delivers a feed's messages in sequence order,
%% so in practice later simply overwrites earlier; the stored Seq makes
%% that explicit and keeps an out-of-order delivery from winning.
%%
%% Split out of `friends` (now ssb_social_graph), which used to keep a
%% name-only cache in `friends_names.tab`.
%%
%% State lives in ssb_store, written through as it changes.  That is
%% affordable here for the same reason it is in ssb_social_graph: only an
%% `about` message writes anything, so the write rate is a small fraction
%% of the message rate — unlike view_manager's checkpoints, which move on
%% every message and are therefore batched instead.
%%
%% A value is whatever JSON the message carried, which is usually a string
%% but need not be: patchwork writes `image` as either a blob id or a
%% {link: …} object.  Strings are stored verbatim and anything else as
%% JSON with a flag, rather than encoding everything uniformly, so that
%% the column stays directly queryable — a name search should be able to
%% say `value LIKE ?` without matching against its own quote marks.
-module(ssb_feed_meta).

-behaviour(gen_server).
-behaviour(ssb_view).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/0,
         get/2,
         all/1,
         name/1]).

%% ssb_view callbacks
-export([view_version/0,
         view_class/0,
         view_load/0,
         view_reset/0,
         view_save/0,
         view_entry/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SCHEMA_VERSION, 1).
-define(DDL,
        ["CREATE TABLE IF NOT EXISTS feed_meta("
         "  feed  TEXT NOT NULL,"
         "  key   TEXT NOT NULL,"
         "  value TEXT,"
         "  json  INTEGER NOT NULL DEFAULT 0,"
         "  seq   INTEGER NOT NULL,"
         "  PRIMARY KEY (feed, key)) WITHOUT ROWID;"]).

%% Envelope fields of an about message; not metadata in their own right.
-define(SKIP, [~"type", ~"about"]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% The latest value FeedId asserted for Key, or undefined.
get(FeedId, Key) when is_binary(FeedId), is_binary(Key) ->
    case rows("SELECT value, json FROM feed_meta WHERE feed=?1 AND key=?2",
              [FeedId, Key]) of
        [[Value, Json]] -> decode_value(Value, Json);
        _               -> undefined
    end;
get(_FeedId, _Key) ->
    undefined.

%% Everything FeedId has asserted about itself, as #{Key => Value}.
all(FeedId) when is_binary(FeedId) ->
    maps:from_list([{K, decode_value(V, J)}
                    || [K, V, J] <- rows("SELECT key, value, json"
                                         " FROM feed_meta WHERE feed=?1",
                                         [FeedId])]);
all(_FeedId) ->
    #{}.

%% Convenience for the near-universal `name` key.
name(FeedId) ->
    get(FeedId, ~"name").

%%%===================================================================
%%% ssb_view callbacks (run in the view_manager process)
%%%===================================================================

view_version() -> 1.

view_class() -> core.

view_load() ->
    case ssb_store:complete(?MODULE) of
        true  -> ok;
        false -> empty
    end.

view_reset() ->
    _ = ssb_store:clear_complete(?MODULE),
    _ = ssb_store:exec("DELETE FROM feed_meta;"),
    ok.

%% Rows are already durable; the only thing to record is that this view's
%% state is complete up to the manager's checkpoints.
view_save() ->
    _ = ssb_store:mark_complete(?MODULE),
    ok.

%% Fold one stored message.  Only a self-about counts: a message whose
%% `about` names anyone other than its own author is an assertion about
%% someone else, which is a social-application concern (patchwork's
%% "socialValue"), not this feed's own metadata.
view_entry(#message{author = Author, sequence = Seq, content = {Props}} = Msg) ->
    case social_msg:is_about(Msg) andalso ?pgv(~"about", Props) =:= Author of
        true  -> put_fields(Author, Seq,
                            [{K, V} || {K, V} <- Props, is_binary(K),
                                       not lists:member(K, ?SKIP)]);
        false -> ok
    end;
view_entry(_) ->
    ok.

%% One about message asserts several fields, so they go in as one
%% transaction rather than one commit each.
put_fields(_FeedId, _Seq, []) ->
    ok;
put_fields(FeedId, Seq, Fields) ->
    Rows = [begin
                {Value, Json} = encode_value(V),
                [FeedId, K, Value, Json, Seq]
            end || {K, V} <- Fields],
    %% Last write wins on sequence, decided in SQL: the WHERE on the
    %% conflict clause makes an older assertion a no-op rather than a
    %% read-then-write that another writer could interleave with.
    _ = ssb_store:insert_many(
          "INSERT INTO feed_meta(feed, key, value, json, seq)"
          " VALUES(?1, ?2, ?3, ?4, ?5)"
          " ON CONFLICT(feed, key) DO UPDATE SET"
          "   value=excluded.value, json=excluded.json, seq=excluded.seq"
          " WHERE excluded.seq >= feed_meta.seq", Rows),
    ok.

%% Strings stay strings; anything else becomes JSON with the flag set.
encode_value(V) when is_binary(V) ->
    {V, 0};
encode_value(V) ->
    try {iolist_to_binary(message:ssb_encoder(V, fun message:ssb_encoder/3, [])), 1}
    catch _:_ -> {undefined, 0}    %% unrepresentable: record the key, not a lie
    end.

decode_value(Value, 0) ->
    Value;
decode_value(Value, 1) when is_binary(Value) ->
    try utils:nat_decode(Value) catch _:_ -> undefined end;
decode_value(_Value, _Json) ->
    undefined.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = ssb_store:declare(?MODULE, ?SCHEMA_VERSION, ?DDL),
    {ok, #{}, {continue, register_view}}.

handle_continue(register_view, State) ->
    ensure_registered(State).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(ensure_registered, State) ->
    ensure_registered(State);
handle_info(_Info, State) ->
    {noreply, State}.

%% Keep retrying until the registration is accepted; a silent skip means
%% feed metadata quietly stops updating (ssb_view:ensure_registered).
ensure_registered(State) ->
    case ssb_view:ensure_registered(?MODULE, [view]) of
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
    catch _:_ -> []          %% store down: no metadata, never a crash
    end.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

meta_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
             [?_test(stores_every_field()),
              ?_test(latest_sequence_wins()),
              ?_test(ignores_about_others()),
              ?_test(keeps_non_string_values()),
              ?_test(survives_a_restart()),
              ?_test(is_a_core_view())]
     end}.

setup() ->
    cleanup(ignore),
    Home = filename:join("/tmp", "feed_meta_"
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

about(Author, Seq, Props) ->
    #message{author = Author, sequence = Seq,
             content = {[{~"type", ~"about"}, {~"about", Author}] ++ Props}}.

%% Every field of a self-about is kept, not just `name`.
stores_every_field() ->
    Id = ~"@meta1.ed25519",
    ok = view_entry(about(Id, 1, [{~"name", ~"alice"},
                                  {~"description", ~"hello"},
                                  {~"image", ~"&blob.sha256"}])),
    ?assertEqual(~"alice", name(Id)),
    ?assertEqual(~"hello", ?MODULE:get(Id, ~"description")),
    ?assertEqual(~"&blob.sha256", ?MODULE:get(Id, ~"image")),
    ?assertEqual(#{~"name" => ~"alice",
                   ~"description" => ~"hello",
                   ~"image" => ~"&blob.sha256"}, all(Id)),
    %% envelope fields are not metadata
    ?assertEqual(undefined, ?MODULE:get(Id, ~"type")),
    ?assertEqual(undefined, ?MODULE:get(Id, ~"about")).

%% A later sequence replaces; an earlier one does not.
latest_sequence_wins() ->
    Id = ~"@meta2.ed25519",
    ok = view_entry(about(Id, 5, [{~"name", ~"second"}])),
    ?assertEqual(~"second", name(Id)),
    ok = view_entry(about(Id, 2, [{~"name", ~"first"}])),
    ?assertEqual(~"second", name(Id)),
    ok = view_entry(about(Id, 9, [{~"name", ~"third"}])),
    ?assertEqual(~"third", name(Id)).

%% An about naming someone else is a social assertion, not self-metadata.
ignores_about_others() ->
    Me    = ~"@meta3.ed25519",
    Other = ~"@meta4.ed25519",
    Msg = #message{author = Me, sequence = 1,
                   content = {[{~"type", ~"about"},
                               {~"about", Other},
                               {~"name", ~"nickname i gave you"}]}},
    ok = view_entry(Msg),
    ?assertEqual(undefined, name(Other)),
    ?assertEqual(undefined, name(Me)).

%% patchwork writes `image` as either a blob id or a {link: …} object, so
%% a value is not always a string and must come back the shape it went in.
keeps_non_string_values() ->
    Id  = ~"@meta5.ed25519",
    Obj = {[{~"link", ~"&blob.sha256"}, {~"size", 1234}]},
    ok = view_entry(about(Id, 1, [{~"image", Obj},
                                  {~"name", ~"has an object image"}])),
    ?assertEqual(Obj, ?MODULE:get(Id, ~"image")),
    %% the string alongside it is stored verbatim, not as quoted JSON —
    %% the column has to stay directly queryable
    ?assertEqual([[~"has an object image"]],
                 ssb_store:q("SELECT value FROM feed_meta"
                             " WHERE feed=?1 AND key=?2", [Id, ~"name"])).

%% The point of the port: durable as written, with no snapshot step.
survives_a_restart() ->
    Id = ~"@meta6.ed25519",
    ok = view_entry(about(Id, 3, [{~"name", ~"persisted"}])),
    ok = gen_server:stop(ssb_store),
    {ok, _} = ssb_store:start_link(),
    ?assertEqual(~"persisted", name(Id)).

is_a_core_view() ->
    ?assertEqual(core, view_class()),
    ?assertEqual(core, ssb_view:class(?MODULE)).

-endif.
