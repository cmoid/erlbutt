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

-define(TAB, ssb_feed_meta).
-define(TABFILE, ~"feed_meta.tab").

%% Written by view_save/0 before each snapshot; its presence after a
%% file2tab restore is how view_load/0 knows the state is complete up to
%% the manager's checkpoints.
-define(COMPLETE, '$complete').

%% Envelope fields of an about message; not metadata in their own right.
-define(SKIP, [~"type", ~"about"]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% The latest value FeedId asserted for Key, or undefined.
get(FeedId, Key) when is_binary(FeedId), is_binary(Key) ->
    try ets:lookup(?TAB, {FeedId, Key}) of
        [{_, Value, _Seq}] -> Value;
        []                 -> undefined
    catch error:badarg -> undefined     %% table absent: server not running
    end.

%% Everything FeedId has asserted about itself, as #{Key => Value}.
all(FeedId) when is_binary(FeedId) ->
    try ets:match(?TAB, {{FeedId, '$1'}, '$2', '_'}) of
        Rows -> maps:from_list([{K, V} || [K, V] <- Rows])
    catch error:badarg -> #{}
    end.

%% Convenience for the near-universal `name` key.
name(FeedId) ->
    get(FeedId, ~"name").

%%%===================================================================
%%% ssb_view callbacks (run in the view_manager process)
%%%===================================================================

view_version() -> 1.

view_class() -> core.

view_load() ->
    case has_marker() of
        true  -> ok;
        false -> empty
    end.

view_reset() ->
    catch ets:delete_all_objects(?TAB),
    ok.

view_save() ->
    ets:insert(?TAB, {?COMPLETE, true, 0}),
    File = table_file(),
    filelib:ensure_dir(File),
    ok = ets:tab2file(?TAB, ?b2l(File)),
    ok.

%% Fold one stored message.  Only a self-about counts: a message whose
%% `about` names anyone other than its own author is an assertion about
%% someone else, which is a social-application concern (patchwork's
%% "socialValue"), not this feed's own metadata.
view_entry(#message{author = Author, sequence = Seq, content = {Props}} = Msg) ->
    case social_msg:is_about(Msg) andalso ?pgv(~"about", Props) =:= Author of
        true  -> [put_field(Author, K, V, Seq) || {K, V} <- Props,
                                                  not lists:member(K, ?SKIP)],
                 ok;
        false -> ok
    end;
view_entry(_) ->
    ok.

%% Last write wins on sequence: an equal or newer Seq replaces.
put_field(FeedId, Key, Value, Seq) when is_binary(Key) ->
    case ets:lookup(?TAB, {FeedId, Key}) of
        [{_, _, Old}] when Old > Seq -> ok;
        _ -> ets:insert(?TAB, {{FeedId, Key}, Value, Seq})
    end;
put_field(_FeedId, _Key, _Value, _Seq) ->
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    restore_or_create(),
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
    %% Snapshot before the table dies with this process: at shutdown we
    %% stop before view_manager, so its own final save cannot succeed.
    catch view_save(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

restore_or_create() ->
    %% table_file needs config; without it (bare eunit setups) start
    %% fresh — view_load/0 then reports empty and the manager rebuilds.
    Restored = try ets:file2tab(?b2l(table_file()))
               catch _:_ -> {error, no_config}
               end,
    case Restored of
        {ok, ?TAB} -> ok;
        _          -> ets:new(?TAB, [set, named_table, public])
    end.

table_file() ->
    <<(config:ssb_repo_loc())/binary, "views/", (?TABFILE)/binary>>.

has_marker() ->
    try ets:lookup(?TAB, ?COMPLETE) =/= []
    catch error:badarg -> false
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
              ?_test(is_a_core_view())]
     end}.

setup() ->
    catch gen_server:stop(?MODULE),
    catch gen_server:stop(config),
    Home = filename:join("/tmp", "feed_meta_"
                         ++ integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    catch ets:new(?TAB, [set, named_table, public]),
    Home.

cleanup(Home) ->
    catch gen_server:stop(?MODULE),
    catch gen_server:stop(config),
    catch ets:delete(?TAB),
    os:cmd("rm -rf " ++ Home),
    application:unset_env(ssb, ssb_home),
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

is_a_core_view() ->
    ?assertEqual(core, view_class()),
    ?assertEqual(core, ssb_view:class(?MODULE)).

-endif.
