%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Message-type index: every stored public message keyed by its content
%% type (post, about, contact, vote, ...).  Serves `messagesByType`
%% (JS: ssb-db), which clients use for type-scoped scans.
%%
%% Same shape as silkpurse_backlinks: an ssb_view over a named public
%% ETS bag {Type, MsgId} plus an ssb_plugin, in one gen_server.
%% Private (still-encrypted) content has no visible type and is not
%% indexed.  live/old are honoured; a live stream emits a {sync: true}
%% sentinel between the backlog and the live tail (ssb-db convention —
%% the silkpurse search indexer waits for it).  gt is NOT honoured:
%% live callers get the full backlog and must skip below their cursor.
-module(silkpurse_by_type).

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

-define(TAB, silkpurse_by_type).
-define(MARKER, '$complete').

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

view_entry(#message{id = MsgId, content = {Props}}) ->
    case ?pgv(~"type", Props) of
        Type when is_binary(Type) ->
            ets:insert(?TAB, {Type, MsgId}),
            {events, [{typed, Type, MsgId}]};
        _ ->
            ok
    end;
view_entry(_) ->
    ok.

%%%===================================================================
%%% ssb_plugin callbacks (run in each connection's rpc_processor)
%%%===================================================================

manifest() ->
    [{[~"messagesByType"], source, owner}].

handle_rpc([~"messagesByType"], Args, _Caller) ->
    case type_of(Args) of
        undefined ->
            {error, ~"messagesByType takes a type"};
        Type ->
            Ids = [Id || {_T, Id} <- ets:lookup(?TAB, Type), is_binary(Id)],
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
                        fun({typed, T, MsgId}) when T =:= Type ->
                                case fetch_encoded(MsgId) of
                                    undefined -> skip;
                                    Bin       -> {send, MsgId, Bin}
                                end;
                           (_) -> skip
                        end,
                    {live_source, Snapshot ++ [sync_sentinel()], ?MODULE,
                     EventFun}
            end
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Create the (empty) table now so it always exists, but defer the
    %% snapshot restore to handle_continue: file2tab of a large snapshot
    %% would otherwise block silkpurse_sup:start_link and thus the whole
    %% node boot (and the shell).  The restores then run concurrently
    %% across the views rather than serialized by the supervisor.
    ets:new(?TAB, [bag, named_table, public]),
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
                _          -> ets:new(?TAB, [bag, named_table, public])
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
%%% Internal functions
%%%===================================================================

table_file() ->
    <<(config:ssb_repo_loc())/binary, "views/by_type.tab">>.

%% Boolean option (live, old) from the request's option object.
flag_of(Key, [{Props}], Default) ->
    case ?pgv(Key, Props) of
        B when is_boolean(B) -> B;
        _                    -> Default
    end;
flag_of(_Key, _Args, Default) ->
    Default.

%% JS accepts a bare type string or {type: T, live, ...}.
type_of([Type]) when is_binary(Type) ->
    Type;
type_of([{Props}]) ->
    case ?pgv(~"type", Props) of
        Type when is_binary(Type) -> Type;
        _                         -> undefined
    end;
type_of(_) ->
    undefined.

%% Closes the backlog of a live stream, before the live tail begins.
sync_sentinel() ->
    {make_ref(),
     iolist_to_binary(message:ssb_encoder({[{~"sync", true}]},
                                          fun message:ssb_encoder/3,
                                          [pretty]))}.

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

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

type_of_test() ->
    ?assertEqual(~"post", type_of([~"post"])),
    ?assertEqual(~"vote", type_of([{[{~"type", ~"vote"}, {~"live", false}]}])),
    ?assertEqual(undefined, type_of([])),
    ?assertEqual(undefined, type_of([{[{~"live", true}]}])).

by_type_test_() ->
    {setup, fun bt_setup/0, fun bt_teardown/1,
     fun(_) -> [?_test(index_and_read_by_type())] end}.

bt_setup() ->
    bt_teardown(ignore),
    Home = filename:join("/tmp", "bt_" ++
                          integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = view_manager:start_link(),
    {ok, _} = silkpurse_by_type:start_link(),
    Home.

bt_teardown(Home) ->
    [catch gen_server:stop(Name)
     || Name <- [silkpurse_by_type, view_manager, ssb_feed_sup,
                 blobs, mess_auth, keys, config]],
    case Home of
        ignore -> ok;
        _ ->
            os:cmd("rm -rf " ++ Home),
            application:unset_env(ssb, ssb_home)
    end,
    ok.

index_and_read_by_type() ->
    OwnId  = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(OwnId),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"},
                                         {~"text", ~"a post"}]}),
    #message{id = PostId} = ssb_feed:fetch_last_msg(OwnPid),
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"vote"},
                                         {~"vote", {[{~"link", PostId},
                                                     {~"value", 1}]}}]}),
    #message{id = VoteId} = ssb_feed:fetch_last_msg(OwnPid),
    Caller = #{class => owner, feed_id => OwnId},
    {source, [{json, PostBin}]} =
        handle_rpc([~"messagesByType"], [~"post"], Caller),
    #message{id = PostId} = message:decode(PostBin, false),
    {source, [{json, VoteBin}]} =
        handle_rpc([~"messagesByType"], [{[{~"type", ~"vote"}]}], Caller),
    #message{id = VoteId} = message:decode(VoteBin, false),
    {source, []} = handle_rpc([~"messagesByType"], [~"gathering"], Caller),
    %% live mode: backlog, then the {sync:true} sentinel as the last
    %% snapshot pair
    {live_source, LivePairs, ?MODULE, _Fun} =
        handle_rpc([~"messagesByType"],
                   [{[{~"type", ~"post"}, {~"live", true}]}], Caller),
    [{_, PostBin2}, {_, SyncBin}] = LivePairs,
    #message{id = PostId} = message:decode(PostBin2, false),
    ?assertEqual({[{~"sync", true}]}, utils:nat_decode(SyncBin)),
    %% old:false still carries the sentinel so the client knows the
    %% (empty) backlog is done
    {live_source, [{_, OnlySync}], ?MODULE, _} =
        handle_rpc([~"messagesByType"],
                   [{[{~"type", ~"post"}, {~"live", true}, {~"old", false}]}],
                   Caller),
    ?assertEqual({[{~"sync", true}]}, utils:nat_decode(OnlySync)).

-endif.