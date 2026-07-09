%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Backlinks index: which stored messages reference a given target
%% (message, feed or blob id) anywhere in their content.  The silkpurse
%% UI's thread and mention views are built on this (JS: ssb-backlinks).
%%
%% An ssb_view over a named public ETS bag {Target, MsgId}, fed and
%% rebuilt by erlbutt's view_manager, snapshotted under <repo>/views/;
%% and an ssb_plugin serving `backlinks.read` (source, owner-only) with
%% the flumeview-query argument shape the JS client sends:
%%   {query: [{$filter: {dest: Target}}], ...}
%% Results are full stored messages in ingest order.  live and old are
%% honoured ({live: true} keeps the stream open, fed by view events via
%% view_stream); reverse is not yet.
-module(silkpurse_backlinks).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).
-behaviour(ssb_view).
-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

%% API
-export([start_link/0, refs/1]).

%% ssb_view callbacks
-export([view_version/0, view_load/0, view_reset/0, view_save/0,
         view_entry/1]).

%% ssb_plugin callbacks
-export([manifest/0, handle_rpc/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         handle_info/2, terminate/2, code_change/3]).

-define(TAB, silkpurse_backlinks).
-define(MARKER, '$complete').

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Message ids that reference Target anywhere in their content.  Used by
%% silkpurse_thread to find a thread's replies.
refs(Target) ->
    try [Id || {_T, Id} <- ets:lookup(?TAB, Target), is_binary(Id)]
    catch error:badarg -> []
    end.

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

view_entry(#message{id = MsgId, content = Content}) ->
    case [T || T <- collect_links(Content), T =/= MsgId] of
        [] -> ok;
        Targets ->
            [ets:insert(?TAB, {Target, MsgId}) || Target <- Targets],
            {events, [{link, Target, MsgId} || Target <- Targets]}
    end.

%%%===================================================================
%%% ssb_plugin callbacks (run in each connection's rpc_processor)
%%%===================================================================

manifest() ->
    [{[~"backlinks", ~"read"], source, owner}].

handle_rpc([~"backlinks", ~"read"], Args, _Caller) ->
    case dest_of(Args) of
        undefined ->
            {error, ~"backlinks.read: no $filter dest in query"};
        Target ->
            Ids = [Id || {_T, Id} <- ets:lookup(?TAB, Target), is_binary(Id)],
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
                    {live_source, Snapshot, ?MODULE, EventFun}
            end
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
        _          -> ets:new(?TAB, [bag, named_table, public])
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
    %% snapshot before the table dies with this process (views stop
    %% before view_manager at shutdown — see friends)
    catch view_save(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

table_file() ->
    <<(config:ssb_repo_loc())/binary, "views/backlinks.tab">>.

%% Every SSB reference (%msg, @feed, &blob) anywhere in the content.
%% Private (still-encrypted binary) content has no visible links.
collect_links(Content) ->
    lists:usort(walk(Content)).

walk({Props}) when is_list(Props) ->
    lists:flatmap(fun({_K, V}) -> walk(V) end, Props);
walk(L) when is_list(L) ->
    lists:flatmap(fun walk/1, L);
walk(B) when is_binary(B) ->
    case is_link(B) of
        true  -> [B];
        false -> []
    end;
walk(_) ->
    [].

is_link(<<"%", _/binary>> = B) -> plausible_ref(B);
is_link(<<"@", _/binary>> = B) -> plausible_ref(B);
is_link(<<"&", _/binary>> = B) -> plausible_ref(B);
is_link(_)                     -> false.

%% sigil + base64 + ".suffix"; keep it loose but bounded
plausible_ref(B) ->
    byte_size(B) >= 40 andalso byte_size(B) =< 128
        andalso binary:match(B, ~".") =/= nomatch.

%% The stored (encoded) form of a message by id, via the id->author
%% index and the author's feed.
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

collect_links_test() ->
    Root = ~"%aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa=.sha256",
    Feed = ~"@bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb=.ed25519",
    Blob = ~"&ccccccccccccccccccccccccccccccccccccccccccc=.sha256",
    Content = {[{~"type", ~"post"},
                {~"text", ~"see this"},
                {~"root", Root},
                {~"branch", [Root]},
                {~"mentions", [{[{~"link", Feed}]},
                               {[{~"link", Blob}]}]}]},
    ?assertEqual(lists:sort([Root, Feed, Blob]), collect_links(Content)),
    %% private content is opaque
    ?assertEqual([], collect_links(~"gibberish.box")),
    %% short/plain strings are not refs
    ?assertEqual([], collect_links({[{~"text", ~"@you & %us"}]})).

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
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = view_manager:start_link(),
    {ok, _} = silkpurse_backlinks:start_link(),
    Home.

bl_teardown(Home) ->
    [catch gen_server:stop(Name)
     || Name <- [silkpurse_backlinks, view_manager, ssb_feed_sup,
                 blobs, mess_auth, keys, config]],
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