%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Thread rollup: the index behind the public feed.  For each thread
%% root it tracks the reply count, the most recent replies, and the
%% last-activity time used to order (bump) the feed.  This is the
%% erlbutt-native equivalent of patchwork's publicFeed/thread-summary
%% pipeline (JS composed it from createFeedStream + LookupRoots +
%% threadSummary; here it is a single fold over the log).
%%
%% An ssb_view over a named public ETS set
%%   RootId => #{author, ts, total, recent :: [{ReplyId, ReplyTs}], last}
%% plus an ssb_plugin serving publicFeed.roots (source, owner).
%%
%% Message bodies are NOT stored: the view holds ids, counts and
%% timestamps, and bodies are fetched from the per-feed store at query
%% time (as backlinks/by_type do).
%%
%% A thread root is a type=post message with no `root` field; a reply
%% is a type post|about carrying content.root.  Deferred: the live
%% publicFeed.latest stream, fork handling, and patchwork's channel /
%% subscription filterResult policy.
-module(silkpurse_threads).

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

-define(TAB, silkpurse_threads).
-define(MARKER, '$complete').
-define(RECENT_KEEP, 8).       %% recent replies retained per thread
-define(RECENT_SHOW, 3).       %% recent replies returned to the client

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% ssb_view callbacks (run in the view_manager process)
%%%===================================================================

%% 2: summaries gained participants/mentions/channel for the feed
%% rollups (participating/mentions/profile/channel), so upgrading nodes
%% must refold.
view_version() -> 2.

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

view_entry(#message{id = Id, author = Author, timestamp = Ts,
                    content = {Props}}) ->
    Type = ?pgv(~"type", Props),
    Root = ?pgv(~"root", Props),
    case classify(Type, Root) of
        root ->
            set_root(Id, Author, Ts, Props),  %% the root's own id keys the thread
            {events, [{thread, Id}]};
        {reply, RootId} ->
            add_reply(RootId, Author, Id, Ts, Props),
            {events, [{thread, RootId}]};
        ignore ->
            ok
    end;
view_entry(_) ->
    ok.

%%%===================================================================
%%% ssb_plugin callbacks (run in each connection's rpc_processor)
%%%===================================================================

%% Every feed tab is thread roots matching a predicate, ordered by
%% activity — publicFeed with a filter.  roots is the paginated history,
%% latest the live prepend.
manifest() ->
    lists:flatten(
      [[{[~"patchwork", Feed, ~"roots"],  source, owner},
        {[~"patchwork", Feed, ~"latest"], source, owner}]
       || Feed <- [~"publicFeed", ~"networkFeed", ~"participatingFeed",
                   ~"mentionsFeed", ~"profile", ~"channelFeed"]]).

handle_rpc([~"patchwork", Feed, ~"roots"], Args, _Caller) ->
    roots(feed_filter(Feed, Args), Args);
handle_rpc([~"patchwork", Feed, ~"latest"], Args, _Caller) ->
    latest(feed_filter(Feed, Args)).

%% Paginated thread roots passing Filter, newest activity first.
roots(Filter, Args) ->
    Opts    = opts(Args),
    Reverse = maps:get(reverse, Opts, true),
    Limit   = maps:get(limit, Opts, undefined),
    Resume  = maps:get(resume, Opts, undefined),
    Threads = [T || {_Id, S} = T <- gather(blocked_set()), Filter(S)],
    Ordered = order(Threads, Reverse, Resume),
    Limited = take(Ordered, Limit),
    {source, [{json, encode_json(Item)}
              || {RootId, Summary} <- Limited,
                 (Item = item(RootId, Summary)) =/= undefined]}.

%% Live prepend: a root item each time a passing thread gains activity.
latest(Filter) ->
    EventFun =
        fun({thread, RootId}) ->
                Blocked = blocked_set(),
                case ets:lookup(?TAB, RootId) of
                    [{RootId, #{author := A} = Summary}] when is_binary(A) ->
                        case (not sets:is_element(A, Blocked))
                             andalso Filter(Summary) of
                            true ->
                                case item(RootId, Summary) of
                                    undefined -> skip;
                                    Item      -> {send, encode_json(Item)}
                                end;
                            false -> skip
                        end;
                    _ -> skip
                end
        end,
    {live_source, [], ?MODULE, EventFun}.

%% The predicate for each feed tab.  owner-relative feeds use the node's
%% own id; profile/channel take their target from the request options.
feed_filter(Feed, _Args) when Feed =:= ~"publicFeed";
                              Feed =:= ~"networkFeed" ->
    fun(_S) -> true end;
feed_filter(~"participatingFeed", _Args) ->
    Owner = keys:pub_key_disp(),
    fun(S) -> maps:is_key(Owner, maps:get(participants, S, #{})) end;
feed_filter(~"mentionsFeed", _Args) ->
    Owner = keys:pub_key_disp(),
    fun(S) -> maps:is_key(Owner, maps:get(mentions, S, #{})) end;
feed_filter(~"profile", Args) ->
    Id = arg(~"id", Args),
    fun(S) -> maps:get(author, S) =:= Id end;
feed_filter(~"channelFeed", Args) ->
    Ch = arg(~"channel", Args),
    fun(S) -> maps:get(channel, S, undefined) =:= Ch end.

arg(Key, [{Props}]) -> ?pgv(Key, Props);
arg(_Key, _)        -> undefined.

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
%%% Internal: indexing
%%%===================================================================

table_file() ->
    <<(config:ssb_repo_loc())/binary, "views/threads.tab">>.

classify(~"post", undefined) ->
    root;
classify(~"post", Root) when is_binary(Root) ->
    {reply, Root};
classify(~"about", Root) when is_binary(Root) ->
    {reply, Root};
classify(_, _) ->
    ignore.

%% A root message: record its author/ts/channel, bump last activity,
%% and add the author + its mentions to the thread's participants /
%% mentions.  A reply may have created the thread first, so preserve
%% total/recent/participants/mentions.
set_root(RootId, Author, Ts, Props) ->
    Cur = current(RootId),
    New = Cur#{author  => Author,
               ts      => Ts,
               last    => max_ts(maps:get(last, Cur), Ts),
               channel => channel_of(Props),
               participants => add(Author, maps:get(participants, Cur)),
               mentions     => add_all(mentions_of(Props),
                                       maps:get(mentions, Cur))},
    ets:insert(?TAB, {RootId, New}).

add_reply(RootId, ReplyAuthor, ReplyId, Ts, Props) ->
    Cur = current(RootId),
    Recent = insert_recent({ReplyId, Ts}, maps:get(recent, Cur)),
    New = Cur#{total  => maps:get(total, Cur) + 1,
               recent => Recent,
               last   => max_ts(maps:get(last, Cur), Ts),
               participants => add(ReplyAuthor, maps:get(participants, Cur)),
               mentions     => add_all(mentions_of(Props),
                                       maps:get(mentions, Cur))},
    ets:insert(?TAB, {RootId, New}).

current(RootId) ->
    case ets:lookup(?TAB, RootId) of
        [{RootId, Summary}] -> Summary;
        []                  -> #{author => undefined, ts => undefined,
                                 total => 0, recent => [], last => 0,
                                 participants => #{}, mentions => #{},
                                 channel => undefined}
    end.

add(FeedId, Map) when is_binary(FeedId) -> Map#{FeedId => true};
add(_, Map)                             -> Map.

add_all(Ids, Map) -> lists:foldl(fun add/2, Map, Ids).

%% content.channel, when a plain string.
channel_of(Props) ->
    case ?pgv(~"channel", Props) of
        Ch when is_binary(Ch) -> Ch;
        _                     -> undefined
    end.

%% Feed ids named in content.mentions ([{link, "@..."}]).
mentions_of(Props) ->
    case ?pgv(~"mentions", Props) of
        Ms when is_list(Ms) ->
            [Link || {MProps} <- Ms,
                     (Link = ?pgv(~"link", MProps)) =/= undefined,
                     is_binary(Link),
                     binary:part(Link, 0, 1) =:= ~"@"];
        _ -> []
    end.

%% Keep recent replies newest-first by timestamp, capped.
insert_recent(Entry, Recent) ->
    Deduped = lists:keydelete(element(1, Entry), 1, Recent),
    Sorted = lists:sort(fun({_, A}, {_, B}) -> A >= B end, [Entry | Deduped]),
    lists:sublist(Sorted, ?RECENT_KEEP).

max_ts(A, B) when is_integer(A), is_integer(B) -> max(A, B);
max_ts(undefined, B) -> B;
max_ts(A, undefined) -> A;
max_ts(_, _)         -> 0.

%%%===================================================================
%%% Internal: query
%%%===================================================================

opts([{Props}]) ->
    lists:foldl(
      fun({K, V}, Acc) ->
              case K of
                  ~"reverse" when is_boolean(V) -> Acc#{reverse => V};
                  ~"limit"   when is_integer(V) -> Acc#{limit => V};
                  ~"resume"  when is_integer(V) -> Acc#{resume => V};
                  _ -> Acc
              end
      end, #{}, Props);
opts(_) ->
    #{}.

%% Feeds the node owner blocks — their threads are hidden.
blocked_set() ->
    sets:from_list(friends:blocks(keys:pub_key_disp())).

%% Threads whose root has been seen and whose author is not blocked.
gather(Blocked) ->
    ets:foldl(
      fun({?MARKER}, Acc) -> Acc;
         ({RootId, #{author := A} = S}, Acc) when is_binary(A) ->
              case sets:is_element(A, Blocked) of
                  true  -> Acc;
                  false -> [{RootId, S} | Acc]
              end;
         (_, Acc) -> Acc
      end, [], ?TAB).

order(Threads, Reverse, Resume) ->
    Cmp = case Reverse of
              true  -> fun({_, #{last := A}}, {_, #{last := B}}) -> A >= B end;
              false -> fun({_, #{last := A}}, {_, #{last := B}}) -> A =< B end
          end,
    Sorted = lists:sort(Cmp, Threads),
    case Resume of
        undefined -> Sorted;
        _ ->
            [T || {_, #{last := L}} = T <- Sorted,
                  case Reverse of true -> L < Resume; false -> L > Resume end]
    end.

take(List, undefined) -> List;
take(List, N) when is_integer(N), N >= 0 -> lists:sublist(List, N);
take(List, _) -> List.

%% Build the roots item: the root message envelope extended with
%% totalReplies, latestReplies (full messages) and bumps, plus rts (the
%% activity time) as the pagination cursor.
item(RootId, #{total := Total, recent := Recent, last := Last}) ->
    case decoded(RootId) of
        {RootProps} ->
            Replies = [R || {Id, _Ts} <- lists:sublist(Recent, ?RECENT_SHOW),
                            (R = decoded(Id)) =/= undefined],
            Bumps = [bump(R) || R <- Replies],
            {RootProps ++ [{~"totalReplies", Total},
                           {~"latestReplies", Replies},
                           {~"bumps", Bumps},
                           {~"rts", Last}]};
        undefined ->
            undefined                    %% root body not fetchable; skip
    end.

%% The stored message as {key, value, timestamp} EJSON, or undefined.
decoded(MsgId) ->
    case fetch_encoded(MsgId) of
        undefined -> undefined;
        Bin       -> utils:nat_decode(Bin)
    end.

bump({Props}) ->
    Value = ?pgv(~"value", Props),
    Author = case Value of {VP} -> ?pgv(~"author", VP); _ -> undefined end,
    {[{~"type", ~"reply"},
      {~"author", Author},
      {~"id", ?pgv(~"key", Props)}]}.

fetch_encoded(MsgId) ->
    case mess_auth:get(MsgId) of
        not_found -> undefined;
        Author ->
            try
                Pid = utils:find_or_create_feed_pid(Author),
                message:encode(ssb_feed:fetch_msg(Pid, MsgId))
            catch _:_ -> undefined
            end
    end.

encode_json(Term) ->
    iolist_to_binary(message:ssb_encoder(Term, fun message:ssb_encoder/3, [pretty])).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

classify_test() ->
    ?assertEqual(root, classify(~"post", undefined)),
    ?assertEqual({reply, ~"%r.sha256"}, classify(~"post", ~"%r.sha256")),
    ?assertEqual({reply, ~"%r.sha256"}, classify(~"about", ~"%r.sha256")),
    ?assertEqual(ignore, classify(~"vote", undefined)),
    ?assertEqual(ignore, classify(~"contact", undefined)).

insert_recent_test() ->
    R0 = [],
    R1 = insert_recent({~"a", 10}, R0),
    R2 = insert_recent({~"b", 30}, R1),
    R3 = insert_recent({~"c", 20}, R2),
    ?assertEqual([{~"b", 30}, {~"c", 20}, {~"a", 10}], R3),
    %% re-inserting an id updates rather than duplicates
    R4 = insert_recent({~"a", 40}, R3),
    ?assertEqual([{~"a", 40}, {~"b", 30}, {~"c", 20}], R4).

threads_test_() ->
    {foreach, fun th_setup/0, fun th_teardown/1,
     [fun(_) -> ?_test(rollup_counts_and_recent()) end,
      fun(_) -> ?_test(reply_before_root()) end,
      fun(_) -> ?_test(block_filtering()) end]}.

th_setup() ->
    th_teardown(ignore),
    Home = filename:join("/tmp", "th_" ++
                          integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = view_manager:start_link(),
    {ok, _} = friends:start_link(),
    {ok, _} = silkpurse_threads:start_link(),
    Home.

th_teardown(Home) ->
    [catch gen_server:stop(Name)
     || Name <- [silkpurse_threads, friends, view_manager, ssb_feed_sup,
                 blobs, mess_auth, keys, config]],
    case Home of
        ignore -> ok;
        _ ->
            os:cmd("rm -rf " ++ Home),
            application:unset_env(ssb, ssb_home)
    end,
    ok.

post(Pid, Content) ->
    ok = ssb_feed:post_content(Pid, Content),
    ssb_feed:fetch_last_msg(Pid).

roots() ->
    {source, Items} =
        handle_rpc([~"patchwork", ~"publicFeed", ~"roots"], [{[]}],
                   #{class => owner, feed_id => keys:pub_key_disp()}),
    [utils:nat_decode(B) || {json, B} <- Items].

rollup_counts_and_recent() ->
    OwnPid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    #message{id = RootId} = post(OwnPid, {[{~"type", ~"post"},
                                           {~"text", ~"root post"}]}),
    _ = post(OwnPid, {[{~"type", ~"post"}, {~"text", ~"r1"},
                       {~"root", RootId}]}),
    _ = post(OwnPid, {[{~"type", ~"post"}, {~"text", ~"r2"},
                       {~"root", RootId}]}),
    [{Props}] = roots(),
    ?assertEqual(RootId, proplists:get_value(~"key", Props)),
    ?assertEqual(2, proplists:get_value(~"totalReplies", Props)),
    Replies = proplists:get_value(~"latestReplies", Props),
    ?assertEqual(2, length(Replies)).

%% A reply ingested before its root: the thread is created with an
%% unknown author (hidden from the feed) and completed when the root
%% arrives.  Tested at the index level because forcing cross-feed
%% ordering with real stored bodies is impractical here.
reply_before_root() ->
    Fake  = ~"%unseenrootxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx=.sha256",
    Reply = ~"%replyaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa=.sha256",
    add_reply(Fake, ~"@replier=.ed25519", Reply, 100, []),
    %% reply-only thread: author unknown, so not surfaced
    ?assertEqual([], gather(sets:new())),
    set_root(Fake, keys:pub_key_disp(), 50, []),
    %% now complete, with the earlier reply counted and activity bumped
    [{Fake, Summary}] = gather(sets:new()),
    ?assertEqual(1, maps:get(total, Summary)),
    ?assertEqual(100, maps:get(last, Summary)).

block_filtering() ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    #message{id = MineRoot} = post(OwnPid, {[{~"type", ~"post"},
                                             {~"text", ~"mine"}]}),
    %% a root from another author, whom we block
    Other = ~"@blockedauthorrrrrrrrrrrrrrrrrrrrrrrrrrrrr=.ed25519",
    set_root(~"%theirroot0000000000000000000000000000000=.sha256", Other, 999, []),
    ets:insert(ssb_block_graph, {OwnId, #{Other => true}}),
    Keys = [proplists:get_value(~"key", P) || {P} <- roots()],
    ?assert(lists:member(MineRoot, Keys)),
    ?assertNot(lists:member(~"%theirroot0000000000000000000000000000000=.sha256",
                            Keys)).

-endif.