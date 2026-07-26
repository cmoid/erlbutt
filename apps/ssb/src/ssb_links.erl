%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Core view: every cross-feed reference, as an edge set.
%%
%% A message that carries the id of another message, feed or blob
%% anywhere in its content produces an edge.  This is the whole index —
%% no notion of a thread, a reply or a vote lives here.  Reading these
%% edges as a *conversation* is a convention (see tangle in ssb_conv);
%% reading them as a graph is what makes them foundational.
%%
%% WHY THIS BELONGS TO THE PROTOCOL LAYER (doc/persistence.md §5)
%%
%% A cross-feed reference is a cryptographically enforced happens-before:
%% if message M in feed A carries the id of message N in feed B, then A
%% held N when it wrote M, because you cannot reference a hash you have
%% not seen.  So
%%
%%   (per-feed sequence edges) u (cross-feed reference edges)
%%
%% is a DAG whose transitive closure is a causal partial order over the
%% whole database — Lamport happens-before derived from content rather
%% than from protocol bookkeeping.  SSB gives a total order within a feed
%% and none at all across feeds (timestamps are self-asserted and
%% routinely absurd), so this is the only trustworthy cross-feed ordering
%% signal there is.
%%
%% Two limits worth keeping in mind: it is a *partial* order and a sparse
%% one — it says "A knew B", not when — and it is not the arrival order
%% in ingest.journal, which is total but purely local.
%%
%% EXTRACTION is schema-agnostic: the content is walked and anything
%% shaped like a reference (%...sha256, @...ed25519, &...sha256) is
%% recorded along with the field path it was found at.  Nothing here
%% knows what a `root` or a `vote` is, which is exactly the property the
%% foundation wants — a new message type is indexed the day it appears.
%%
%% IDS ARE INTERNED to integers.  Measured on a 2.5M-message corpus, the
%% edge set stores three 53-byte ids per row and lands at 1.33 GB
%% indexed; interning takes that to roughly a fifth (§8).  The intern
%% table is also the msgid -> id mapping the eventual store needs, so it
%% is not a private optimisation.
%%
%% This is ETS today and moves to the embedded store in §8 item 11; the
%% shape here is chosen to survive that move unchanged.
-module(ssb_links).

-behaviour(gen_server).
-behaviour(ssb_view).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/0,
         refs/1,
         refs/2,
         links_of/1,
         edge_count/0]).

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

%% Id <-> integer, both directions, plus the counter and the completeness
%% marker (atom keys, which never collide with the binary ids).
-define(IDS,   ssb_links_ids).
-define(NAMES, ssb_links_names).
%% bag: {ToInt, FromInt, Field, Kind}
-define(LINKS, ssb_links_edges).

-define(NEXT, '$next').
-define(COMPLETE, '$complete').

-define(TABLES, [{?IDS,   ~"links_ids.tab"},
                 {?NAMES, ~"links_names.tab"},
                 {?LINKS, ~"links_edges.tab"}]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Ids of the messages that reference Target anywhere in their content.
%% One message can reach the same target through several fields (a reply
%% names its root as both `root` and `branch`), which is two edges but
%% one referrer — so the source ids are deduplicated here, keeping the
%% order they were first indexed in.
refs(Target) when is_binary(Target) ->
    dedup([name_of(From) || {_To, From, _Field, _Kind} <- edges_to(Target)]).

%% The same, restricted to references made in a particular field —
%% "who named this as their `root`" rather than "who mentioned it".
refs(Target, Field) when is_binary(Target), is_binary(Field) ->
    dedup([name_of(From) || {_To, From, F, _Kind} <- edges_to(Target),
                            F =:= Field]).

dedup(Ids) ->
    dedup(Ids, #{}, []).

dedup([], _Seen, Acc) ->
    lists:reverse(Acc);
dedup([Id | Rest], Seen, Acc) when is_map_key(Id, Seen) ->
    dedup(Rest, Seen, Acc);
dedup([Id | Rest], Seen, Acc) ->
    dedup(Rest, Seen#{Id => true}, [Id | Acc]).

edges_to(Target) ->
    case id_of(Target) of
        {ok, To} -> try ets:lookup(?LINKS, To) catch error:badarg -> [] end;
        miss     -> []
    end.

%% Total edges held.  For the admin surface and for tests that want to
%% assert the index is not silently empty.
edge_count() ->
    try ets:info(?LINKS, size) of
        N when is_integer(N) -> N;
        _                    -> 0
    catch error:badarg -> 0
    end.

%%%===================================================================
%%% Extraction (pure; exported so consumers can read a message's
%%% OUTGOING links, which the index does not store)
%%%===================================================================

%% [{Target, Field, Kind}] for one message's content, deduplicated.
%% Kind is msg | feed | blob; Field is the dotted path the reference was
%% found at ("branch", "vote.link", ...).
links_of({Props}) when is_list(Props) ->
    lists:usort(walk({Props}, []));
%% Private content is a "...box" binary.  We can only see inside messages
%% addressed to us, and those are ours to index — the alternative is that
%% a DM's replies are invisible in the thread that contains them.
links_of(Boxed) when is_binary(Boxed) ->
    case private_box:is_private(Boxed) andalso private_box:decrypt(Boxed) of
        {ok, Plain} ->
            try links_of(utils:nat_decode(Plain))
            catch _:_ -> []                 %% plaintext that is not JSON
            end;
        _ ->
            []
    end;
links_of(_) ->
    [].

walk({Props}, Path) when is_list(Props) ->
    lists:flatmap(fun({K, V}) when is_binary(K) -> walk(V, [K | Path]);
                     (_)                        -> []
                  end, Props);
walk(List, Path) when is_list(List) ->
    %% An array shares its parent's field name; the position is not part
    %% of the identity of the reference.
    lists:flatmap(fun(E) -> walk(E, Path) end, List);
walk(Bin, Path) when is_binary(Bin) ->
    case ref_kind(Bin) of
        undefined -> [];
        Kind      -> [{Bin, field_path(Path), Kind}]
    end;
walk(_Other, _Path) ->
    [].

field_path(Path) ->
    iolist_to_binary(lists:join(~".", lists:reverse(Path))).

%% Sigil plus suffix.  Deliberately lexical: the foundation should index
%% a reference it has never seen the message type of.
ref_kind(<<"%", _/binary>> = B) -> kind_if(B, ~".sha256",  msg);
ref_kind(<<"@", _/binary>> = B) -> kind_if(B, ~".ed25519", feed);
ref_kind(<<"&", _/binary>> = B) -> kind_if(B, ~".sha256",  blob);
ref_kind(_)                     -> undefined.

kind_if(B, Suffix, Kind) ->
    SS = byte_size(Suffix),
    BS = byte_size(B),
    case BS > SS andalso binary:part(B, BS - SS, SS) =:= Suffix of
        true  -> Kind;
        false -> undefined
    end.

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
    [catch ets:delete_all_objects(Tab) || {Tab, _} <- ?TABLES],
    ok.

view_save() ->
    ets:insert(?IDS, {?COMPLETE, true}),
    [begin
         File = table_file(Name),
         filelib:ensure_dir(File),
         ok = ets:tab2file(Tab, ?b2l(File))
     end || {Tab, Name} <- ?TABLES],
    ok.

%% Index one message's outgoing references.  Self-references are dropped:
%% a message naming its own id is not an edge, and it would make every
%% traversal cyclic.
view_entry(#message{id = MsgId, content = Content}) when is_binary(MsgId) ->
    case [L || {Target, _F, _K} = L <- links_of(Content), Target =/= MsgId] of
        [] ->
            ok;
        Links ->
            From = intern(MsgId),
            [ets:insert(?LINKS, {intern(Target), From, Field, Kind})
             || {Target, Field, Kind} <- Links],
            %% Same event shape the silkpurse backlinks view publishes, so
            %% its live streams can be repointed here without changing
            %% their subscribers.
            {events, [{link, Target, MsgId} || {Target, _F, _K} <- Links]}
    end;
view_entry(_) ->
    ok.

%%%===================================================================
%%% Interning
%%%===================================================================
%%
%% Writes happen only in view_entry, which view_manager runs in one
%% process, so the lookup-then-insert below cannot race.  Readers only
%% ever look up.

intern(Bin) ->
    case ets:lookup(?IDS, Bin) of
        [{Bin, Int}] ->
            Int;
        [] ->
            Int = ets:update_counter(?IDS, ?NEXT, 1, {?NEXT, 0}),
            ets:insert(?IDS, {Bin, Int}),
            ets:insert(?NAMES, {Int, Bin}),
            Int
    end.

%% Look up without creating: a query for an id we have never seen must
%% not grow the intern table.
id_of(Bin) ->
    try ets:lookup(?IDS, Bin) of
        [{Bin, Int}] -> {ok, Int};
        []           -> miss
    catch error:badarg -> miss              %% table absent: not running
    end.

name_of(Int) ->
    try ets:lookup(?NAMES, Int) of
        [{Int, Bin}] -> Bin;
        []           -> undefined
    catch error:badarg -> undefined
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    [restore_or_create(Tab, Name, kind_of(Tab)) || {Tab, Name} <- ?TABLES],
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

ensure_registered(State) ->
    case ssb_view:ensure_registered(?MODULE, [view]) of
        ok    -> ok;
        retry -> erlang:send_after(2000, self(), ensure_registered)
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    catch view_save(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

kind_of(?LINKS) -> bag;
kind_of(_)      -> set.

restore_or_create(Tab, Name, Kind) ->
    %% table_file needs config; without it (bare eunit setups) start
    %% fresh — view_load/0 then reports empty and the manager rebuilds.
    Restored = try ets:file2tab(?b2l(table_file(Name)))
               catch _:_ -> {error, no_config}
               end,
    case Restored of
        {ok, Tab} -> ok;
        _         -> ets:new(Tab, [Kind, named_table, public])
    end.

table_file(Name) ->
    <<(config:ssb_repo_loc())/binary, "views/", Name/binary>>.

has_marker() ->
    try ets:lookup(?IDS, ?COMPLETE) =/= []
    catch error:badarg -> false
    end.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

links_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
             [?_test(extracts_every_shape()),
              ?_test(ignores_non_references()),
              ?_test(indexes_and_interns()),
              ?_test(filters_by_field()),
              ?_test(dedups_multi_field_referrer()),
              ?_test(drops_self_reference()),
              ?_test(is_a_core_view())]
     end}.

setup() ->
    catch gen_server:stop(?MODULE),
    catch gen_server:stop(config),
    Home = filename:join("/tmp", "ssb_links_"
                         ++ integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    [catch ets:new(Tab, [kind_of(Tab), named_table, public])
     || {Tab, _} <- ?TABLES],
    Home.

cleanup(Home) ->
    catch gen_server:stop(?MODULE),
    catch gen_server:stop(config),
    [catch ets:delete(Tab) || {Tab, _} <- ?TABLES],
    os:cmd("rm -rf " ++ Home),
    application:unset_env(ssb, ssb_home),
    ok.

msg(Id) -> <<"%", Id/binary, ".sha256">>.
feed(Id) -> <<"@", Id/binary, ".ed25519">>.
blob(Id) -> <<"&", Id/binary, ".sha256">>.

%% Every sigil, at top level, nested in an object, and inside an array —
%% with the field path each was found at.
extracts_every_shape() ->
    Content = {[{~"type",   ~"post"},
                {~"root",   msg(~"root")},
                {~"branch", [msg(~"b1"), msg(~"b2")]},
                {~"about",  feed(~"who")},
                {~"vote",   {[{~"link", msg(~"voted")}]}},
                {~"mentions", [{[{~"link", blob(~"pic")}]}]}]},
    ?assertEqual(
       lists:usort([{msg(~"root"),   ~"root",          msg},
                    {msg(~"b1"),     ~"branch",        msg},
                    {msg(~"b2"),     ~"branch",        msg},
                    {feed(~"who"),   ~"about",         feed},
                    {msg(~"voted"),  ~"vote.link",     msg},
                    {blob(~"pic"),   ~"mentions.link", blob}]),
       links_of(Content)).

%% Anything not shaped like a reference is not one — the extractor is
%% lexical, so this is the guard against it over-matching.
ignores_non_references() ->
    Content = {[{~"text",    ~"a post mentioning % and @ and &"},
                {~"almost",  ~"%missing-suffix"},
                {~"wrong",   ~"%abc.ed25519xyz"},
                {~"number",  42},
                {~"bare",    ~"%"},
                {~"nothing", null}]},
    ?assertEqual([], links_of(Content)).

%% Edges are stored by interned integer but read back as ids.
indexes_and_interns() ->
    Target = msg(~"target1"),
    Source = msg(~"source1"),
    {events, _} = view_entry(#message{id = Source,
                                      content = {[{~"root", Target}]}}),
    ?assertEqual([Source], refs(Target)),
    ?assertEqual([], refs(msg(~"never-seen"))),
    %% the ids really are interned: the edge table holds integers
    [{To, From, _F, _K}] = ets:lookup(?LINKS, element(2, id_of(Target))),
    ?assert(is_integer(To) andalso is_integer(From)),
    %% and interning is stable
    ?assertEqual(id_of(Target), id_of(Target)),
    %% a query for an unknown id must not grow the intern table
    Before = ets:info(?IDS, size),
    ?assertEqual([], refs(msg(~"still-never-seen"))),
    ?assertEqual(Before, ets:info(?IDS, size)).

filters_by_field() ->
    Target = msg(~"target2"),
    Rooted = msg(~"rooted"),
    Mentioned = msg(~"mentioned"),
    {events, _} = view_entry(#message{id = Rooted,
                                      content = {[{~"root", Target}]}}),
    {events, _} = view_entry(#message{id = Mentioned,
                                      content = {[{~"mentions", [Target]}]}}),
    ?assertEqual([Mentioned, Rooted], lists:sort(refs(Target))),
    ?assertEqual([Rooted],    refs(Target, ~"root")),
    ?assertEqual([Mentioned], refs(Target, ~"mentions")),
    ?assertEqual([],          refs(Target, ~"branch")).

%% A reply names its root as both `root` and `branch`: two edges, but one
%% referrer as far as a backlinks query is concerned.
dedups_multi_field_referrer() ->
    Target = msg(~"target3"),
    Reply  = msg(~"reply3"),
    {events, _} = view_entry(#message{id = Reply,
                                      content = {[{~"root",   Target},
                                                  {~"branch", Target}]}}),
    ?assertEqual([Reply], refs(Target)),
    %% both edges are really there, they just collapse in refs/1
    ?assertEqual([Reply], refs(Target, ~"root")),
    ?assertEqual([Reply], refs(Target, ~"branch")),
    ?assertEqual(2, length(edges_to(Target))).

%% A message naming its own id is not an edge.
drops_self_reference() ->
    Self = msg(~"selfref"),
    ?assertEqual(ok, view_entry(#message{id = Self,
                                         content = {[{~"root", Self}]}})),
    ?assertEqual([], refs(Self)).

is_a_core_view() ->
    ?assertEqual(core, view_class()),
    ?assertEqual(core, ssb_view:class(?MODULE)).

%%%-------------------------------------------------------------------
%%% Through the real store: ssb_feed -> view_manager -> this view
%%%-------------------------------------------------------------------

integration_test_() ->
    {setup, fun int_setup/0, fun int_cleanup/1,
     fun(_) ->
             [?_test(indexes_a_stored_message()),
              ?_test(indexes_a_private_message_for_us()),
              ?_test(rebuilds_from_the_log())]
     end}.

int_setup() ->
    int_cleanup(ignore),
    Home = filename:join("/tmp", "links_int_"
                         ++ integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = view_manager:start_link(),
    {ok, _} = start_link(),
    ok = int_wait(),
    Home.

int_cleanup(Home) ->
    [catch gen_server:stop(N)
     || N <- [?MODULE, view_manager, ssb_feed_sup, blobs, mess_auth, keys,
              config]],
    [catch ets:delete(Tab) || {Tab, _} <- ?TABLES],
    case Home of
        ignore -> ok;
        _ -> os:cmd("rm -rf " ++ Home),
             application:unset_env(ssb, ssb_home)
    end,
    ok.

%% Registration schedules the fold; wait for it before asserting.
int_wait() -> int_wait(250).
int_wait(0) -> error(never_caught_up);
int_wait(N) ->
    case view_manager:caught_up(?MODULE) of
        true  -> ok;
        false -> timer:sleep(20), int_wait(N - 1)
    end.

int_post(Content) ->
    OwnId = keys:pub_key_disp(),
    Pid   = utils:find_or_create_feed_pid(OwnId),
    ok = ssb_feed:post_content(Pid, Content),
    #message{id = Id} = ssb_feed:fetch_last_msg(Pid),
    Id.

%% A message stored the normal way is indexed without anyone calling
%% view_entry directly.
indexes_a_stored_message() ->
    Root = int_post({[{~"type", ~"post"}, {~"text", ~"root post"}]}),
    Reply = int_post({[{~"type",   ~"post"},
                       {~"text",   ~"a reply"},
                       {~"root",   Root},
                       {~"branch", Root}]}),
    ?assertEqual([Reply], refs(Root)),
    ?assertEqual([Reply], refs(Root, ~"root")),
    ?assertEqual([Reply], refs(Root, ~"branch")).

%% A DM addressed to us is decrypted and indexed — otherwise a private
%% reply is invisible in the thread it belongs to.
indexes_a_private_message_for_us() ->
    Root = int_post({[{~"type", ~"post"}, {~"text", ~"public root"}]}),
    Me   = keys:pub_key_disp(),
    Pid  = utils:find_or_create_feed_pid(Me),
    ok = ssb_feed:post_private(Pid, {[{~"type", ~"post"},
                                      {~"text", ~"secret reply"},
                                      {~"root", Root}]}, [Me]),
    #message{id = Secret} = ssb_feed:fetch_last_msg(Pid),
    ?assert(lists:member(Secret, refs(Root))).

%% The index is derived: wiping it and refolding the log restores it.
rebuilds_from_the_log() ->
    Root  = int_post({[{~"type", ~"post"}, {~"text", ~"rebuild root"}]}),
    Reply = int_post({[{~"type", ~"post"}, {~"root", Root}]}),
    ?assert(lists:member(Reply, refs(Root))),
    ok = view_manager:rebuild(?MODULE),
    ok = int_wait(),
    ?assert(lists:member(Reply, refs(Root))),
    ?assert(edge_count() > 0).

-endif.
