%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Backlinks: which stored messages reference a given target (message,
%% feed or blob id) anywhere in their content.  The silkpurse UI's thread
%% and mention views are built on this (JS: ssb-backlinks).
%%
%% This used to keep its OWN index — an ssb_view over a bag of
%% {Target, MsgId}, snapshotted under <repo>/views/.  That index was an
%% edge set, which is exactly what the ssb_links core view now holds for
%% the whole node (doc/persistence.md §5), so this module keeps no state
%% at all: it is a stateless ssb_plugin that queries ssb_links and
%% renders the answers in the shape the JS client expects.
%%
%% It serves `backlinks.read` (source, owner-only) with the
%% flumeview-query argument shape the client sends:
%%   {query: [{$filter: {dest: Target}}], ...}
%% Results are full stored messages in index order.  live and old are
%% honoured ({live: true} keeps the stream open, fed by ssb_links' view
%% events via view_stream); reverse is not yet.
-module(silkpurse_backlinks).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

%% API
-export([refs/1]).

%% ssb_plugin callbacks
-export([manifest/0, handle_rpc/3]).

%%%===================================================================
%%% API
%%%===================================================================

%% Message ids that reference Target anywhere in their content.  Used by
%% silkpurse_thread to find a thread's replies.  Kept as a named function
%% rather than inlining ssb_links:refs/1 at the call sites, so the
%% silkpurse side has one place to change if the query ever needs
%% app-level filtering.
refs(Target) ->
    ssb_links:refs(Target).

%%%===================================================================
%%% ssb_plugin callbacks (run in each connection's rpc_processor)
%%%===================================================================

manifest() ->
    [{[~"backlinks", ~"read"],                          source, owner},
     {[~"patchwork", ~"backlinks", ~"referencesStream"], source, owner},
     {[~"patchwork", ~"backlinks", ~"forksStream"],      source, owner},
     {[~"patchwork", ~"liveBacklinks", ~"stream"],      source, owner}].

handle_rpc([~"backlinks", ~"read"], Args, _Caller) ->
    case dest_of(Args) of
        undefined ->
            {error, ~"backlinks.read: no $filter dest in query"};
        Target ->
            Ids = refs(Target),
            Pairs = [{Id, Bin} || Id <- Ids,
                                  (Bin = fetch_encoded(Id)) =/= undefined],
            case flag_of(~"live", Args, false) of
                false ->
                    {source, [{json, B} || {_, B} <- Pairs]};
                true ->
                    Snapshot = case flag_of(~"old", Args, true) of
                                   false -> [];
                                   _     -> Pairs
                               end,
                    EventFun =
                        fun({link, T, MsgId}) when T =:= Target ->
                                case fetch_encoded(MsgId) of
                                    undefined -> skip;
                                    Bin       -> {send, MsgId, Bin}
                                end;
                           (_) -> skip
                        end,
                    {live_source, Snapshot, ssb_links, EventFun}
            end
    end;

%% referencesStream({id, since}): messages that reference id, those after
%% `since` as a snapshot, then live new ones — the per-message references
%% shown in the UI (backlinks.obs.references).
%%
%% Items are flat reference SUMMARIES ({id, author, timestamp}), not the
%% message envelope: message/html/references.js reads link.author and
%% link.id off the top level (an envelope gave it author = undefined, and
%% about.obs.name(undefined) then threw inside mutant's update loop and
%% took the page down).  liveBacklinks.stream below keeps the envelope —
%% backlinks.obs.for sorts whole messages there.
handle_rpc([~"patchwork", ~"backlinks", ~"referencesStream"], [{Opts}], _Caller) ->
    summary_stream(Opts, fun is_reference/3, ~"referencesStream needs an id");

%% forksStream({id, since}): the replies rooted AT id — the sub-thread
%% that forked off it — in the same flat summary shape.
%%
%% Patchwork splits a message's backlinks in two, and message/html/forks.js
%% renders this half as "N forked this discussion".  While this was stubbed
%% empty a fork fell through to referencesStream, which filtered nothing,
%% so replying to a mid-thread post rendered as a bare "X referenced this
%% message" — the reply was on the right thread, but shown as a passing
%% mention of it.
handle_rpc([~"patchwork", ~"backlinks", ~"forksStream"], [{Opts}], _Caller) ->
    summary_stream(Opts, fun is_fork/3, ~"forksStream needs an id");

%% liveBacklinks.stream(): every new backlink, tagged with the dest it
%% references, for the whole connection.  The client routes each frame by
%% its `dest` to the message it is viewing, so subscribe/unsubscribe are
%% just traffic hints (harmless no-ops here) and no per-id state is kept.
handle_rpc([~"patchwork", ~"liveBacklinks", ~"stream"], _Args, _Caller) ->
    EventFun = fun({link, Target, MsgId}) ->
                       case dest_tagged(Target, MsgId) of
                           undefined -> skip;
                           Bin       -> {send, Bin}  %% distinct per (dest,msg)
                       end;
                  (_) -> skip
               end,
    {live_source, [], ssb_links, EventFun}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% referencesStream and forksStream are one query over one edge set,
%% separated by a single predicate on the referencing message.  Sharing
%% the machinery is what keeps the two halves DISJOINT: if both were to
%% match the same message the client would render it twice, once as a
%% fork and once as a mention, which is the failure the split exists to
%% avoid.
summary_stream(Opts, Pred, ErrMsg) ->
    case ?pgv(~"id", Opts) of
        Id when is_binary(Id) ->
            Since  = ?pgv(~"since", Opts),
            Thread = thread_of(Id),
            Keep   = fun(Bin) -> Pred(root_of(Bin), Id, Thread) end,
            Snapshot =
                lists:filtermap(
                  fun(MsgId) ->
                          case fetch_encoded(MsgId) of
                              undefined -> false;
                              Bin ->
                                  case Keep(Bin)
                                      andalso after_since(Bin, Since)
                                      andalso ref_summary(Bin) of
                                      false     -> false;
                                      undefined -> false;
                                      Sum       -> {true, {MsgId, Sum}}
                                  end
                          end
                  end, refs(Id)),
            EventFun = fun({link, T, MsgId}) when T =:= Id ->
                               case fetch_encoded(MsgId) of
                                   undefined -> skip;
                                   Bin ->
                                       case Keep(Bin)
                                           andalso ref_summary(Bin) of
                                           false     -> skip;
                                           undefined -> skip;
                                           Sum       -> {send, MsgId, Sum}
                                       end
                               end;
                          (_) -> skip
                       end,
            {live_source, Snapshot, ssb_links, EventFun};
        _ ->
            {error, ErrMsg}
    end.

%% Rooted AT the target: the sub-thread that forked off it.
is_fork(Root, Id, _Thread) ->
    Root =:= Id.

%% Names the target from OUTSIDE the target's own thread.
%%
%% Excluding the thread is what stops this being noise.  In a linear
%% thread each reply branches from the one before it, so a plain "not
%% rooted at the target" filter would hang an "X referenced this message"
%% line off EVERY reply, pointing at its own successor — messages already
%% on screen a few pixels below.  A reference worth showing is one from
%% somewhere else.
is_reference(Root, Id, Thread) ->
    Root =/= Id andalso Root =/= Thread.

%% The thread a message belongs to: its root if it has one, otherwise
%% itself (a thread root is its own thread).  Falling back to Id also
%% covers a target we cannot fetch, which then admits only references
%% that are genuinely rooted elsewhere.
thread_of(Id) ->
    case fetch_encoded(Id) of
        undefined -> Id;
        Bin ->
            case root_of(Bin) of
                undefined -> Id;
                Root      -> Root
            end
    end.

%% The referencing message's content.root, or undefined.
root_of(Bin) ->
    try
        {Env} = utils:nat_decode(Bin),
        {Val} = ?pgv(~"value", Env),
        root_of_content(?pgv(~"content", Val))
    catch _:_ -> undefined
    end.

%% A private message's content is a box, so its root is legible only if
%% the message is addressed to us — the same decrypt silkpurse_thread
%% does to place a DM reply in its thread.  Without it a private fork
%% would be filed as a plain mention, which is the asymmetry that made
%% DM replies invisible in threads before.
root_of_content({Props}) when is_list(Props) ->
    ?pgv(~"root", Props);
root_of_content(Box) when is_binary(Box) ->
    case private_box:is_private(Box) andalso private_box:decrypt(Box) of
        {ok, Plain} ->
            try utils:nat_decode(Plain) of
                {Props} when is_list(Props) -> ?pgv(~"root", Props);
                _                           -> undefined
            catch _:_ -> undefined
            end;
        _ ->
            undefined
    end;
root_of_content(_) ->
    undefined.

%% The stored (encoded) form of a message by id, via the id->author
%% index and the author's feed.
fetch_encoded(MsgId) ->
    case mess_auth:get(MsgId) of
        not_found -> undefined;
        Author ->
            try
                Pid = utils:find_or_create_feed_pid(Author),
                case ssb_feed:fetch_msg(Pid, MsgId) of
                    not_found -> undefined;
                    Msg       -> message:encode(Msg)
                end
            catch _:_ -> undefined
            end
    end.

%% The message envelope with a `dest` field naming the target it links
%% to, for liveBacklinks.stream, or undefined if it can't be fetched.
dest_tagged(Target, MsgId) ->
    case fetch_encoded(MsgId) of
        undefined -> undefined;
        Bin ->
            try
                {Env} = utils:nat_decode(Bin),
                encode_json({Env ++ [{~"dest", Target}]})
            catch _:_ -> undefined
            end
    end.

%% A reference as backlinks.obs.references wants it: the referencing
%% message's id and author, plus the asserted timestamp the client sends
%% back as its `since` cursor (so it must be the field after_since/2
%% compares).
ref_summary(Bin) ->
    try
        {Env} = utils:nat_decode(Bin),
        {Val} = ?pgv(~"value", Env),
        encode_json({[{~"id",        ?pgv(~"key", Env)},
                      {~"author",    ?pgv(~"author", Val)},
                      {~"timestamp", ?pgv(~"timestamp", Val)}]})
    catch _:_ -> undefined
    end.

%% True when the message's asserted timestamp is past Since (or Since is
%% absent) — the incremental cursor referencesStream is polled with.
after_since(_Bin, Since) when not is_integer(Since) ->
    true;
after_since(Bin, Since) ->
    try
        {Env} = utils:nat_decode(Bin),
        {Val} = ?pgv(~"value", Env),
        ?pgv(~"timestamp", Val) > Since
    catch _:_ -> true
    end.

encode_json(Term) ->
    iolist_to_binary(message:ssb_encoder(Term, fun message:ssb_encoder/3, [pretty])).

%% Boolean option (live, old) from the request's option object.
flag_of(Key, [{Props}], Default) ->
    case ?pgv(Key, Props) of
        B when is_boolean(B) -> B;
        _                    -> Default
    end;
flag_of(_Key, _Args, Default) ->
    Default.

%% {query: [{$filter: {dest: Target}}], ...} — the shape ssb-backlinks
%% clients send.  Anything else -> undefined.
dest_of([{Props}]) ->
    case ?pgv(~"query", Props) of
        [{QProps} | _] ->
            case ?pgv(~"$filter", QProps) of
                {FProps} -> ?pgv(~"dest", FProps);
                _        -> undefined
            end;
        _ -> undefined
    end;
dest_of(_) ->
    undefined.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

dest_of_test() ->
    Root = ~"%aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa=.sha256",
    Args = [{[{~"query", [{[{~"$filter", {[{~"dest", Root}]}}]}]},
              {~"live", false}]}],
    ?assertEqual(Root, dest_of(Args)),
    ?assertEqual(undefined, dest_of([])),
    ?assertEqual(undefined, dest_of([{[{~"live", true}]}])).

backlinks_test_() ->
    {setup, fun bl_setup/0, fun bl_teardown/1,
     fun(_) -> [?_test(index_and_read())] end}.

bl_setup() ->
    bl_teardown(ignore),
    Home = filename:join("/tmp", "bl_" ++
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
    {ok, _} = ssb_links:start_link(),
    ok = bl_wait(250),
    Home.

%% ssb_links registers asynchronously; a view mid-catch-up takes no
%% ingests, so wait before storing anything.
bl_wait(0) -> error(never_caught_up);
bl_wait(N) ->
    case view_manager:caught_up(ssb_links) of
        true  -> ok;
        false -> timer:sleep(20), bl_wait(N - 1)
    end.

bl_teardown(Home) ->
    [catch gen_server:stop(Name)
     || Name <- [ssb_links, view_manager, ssb_store, ssb_feed_sup,
                 blobs, mess_auth,  keys, config]],
    case Home of
        ignore -> ok;
        _ ->
            os:cmd("rm -rf " ++ Home),
            application:unset_env(ssb, ssb_home)
    end,
    ok.

index_and_read() ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"},
                                         {~"text", ~"the root"}]}),
    #message{id = RootId} = ssb_feed:fetch_last_msg(OwnPid),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"},
                                         {~"text", ~"a reply"},
                                         {~"root", RootId}]}),
    #message{id = ReplyId} = ssb_feed:fetch_last_msg(OwnPid),
    Args = [{[{~"query", [{[{~"$filter", {[{~"dest", RootId}]}}]}]}]}],
    {source, [{json, Bin}]} =
        handle_rpc([~"backlinks", ~"read"], Args,
                   #{class => owner, feed_id => OwnId}),
    #message{id = ReplyId} = message:decode(Bin, false),
    %% unknown target -> empty stream, not an error
    NoArgs = [{[{~"query",
                 [{[{~"$filter",
                     {[{~"dest", ~"%nope00000000000000000000000000000000000000=.sha256"}]}}]}]}]}],
    {source, []} = handle_rpc([~"backlinks", ~"read"], NoArgs,
                              #{class => owner, feed_id => OwnId}).

-endif.