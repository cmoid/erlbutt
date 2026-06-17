%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Member registry for a room.  Holds the feed ids of users who have joined
%% (community/restricted rooms).  Membership is granted by redeeming a room
%% invite — the same pub-invite mechanism, but invite.use adds a member here
%% instead of posting a follow (see rpc_processor).  Persisted across restarts
%% via ets:tab2file so a room does not forget its members.
-module(room_store).

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

-export([start_link/0, add_member/1, remove_member/1, is_member/1, members/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tab, file}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_member(FeedId) when is_binary(FeedId) ->
    gen_server:call(?MODULE, {add_member, FeedId}).

remove_member(FeedId) when is_binary(FeedId) ->
    gen_server:call(?MODULE, {remove_member, FeedId}).

is_member(undefined) -> false;
is_member(FeedId) when is_binary(FeedId) ->
    gen_server:call(?MODULE, {is_member, FeedId}).

members() ->
    gen_server:call(?MODULE, members).

init([]) ->
    process_flag(trap_exit, true),
    File = binary_to_list(config:ssb_repo_loc()) ++ "room_members.tab",
    ok = filelib:ensure_dir(File),
    Tab = case ets:file2tab(File) of
        {ok, T}    -> T;
        {error, _} -> ets:new(room_members, [set, private])
    end,
    {ok, #state{tab = Tab, file = File}}.

handle_call({add_member, FeedId}, _From, #state{tab = Tab} = State) ->
    ets:insert(Tab, {FeedId, true}),
    persist(State),
    {reply, ok, State};

handle_call({remove_member, FeedId}, _From, #state{tab = Tab} = State) ->
    ets:delete(Tab, FeedId),
    persist(State),
    {reply, ok, State};

handle_call({is_member, FeedId}, _From, #state{tab = Tab} = State) ->
    {reply, ets:member(Tab, FeedId), State};

handle_call(members, _From, #state{tab = Tab} = State) ->
    {reply, [K || {K, _} <- ets:tab2list(Tab)], State};

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

persist(#state{tab = Tab, file = File}) ->
    ets:tab2file(Tab, File).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

room_store_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> ?_test(membership()) end}.

setup() ->
    catch gen_server:stop(room_store),
    catch gen_server:stop(config),
    Home = filename:join("/tmp", "room_store_"
                         ++ integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("test/ssb.cfg"),
    {ok, _} = room_store:start_link(),
    Home.

cleanup(Home) ->
    catch gen_server:stop(room_store),
    catch gen_server:stop(config),
    os:cmd("rm -rf " ++ Home),
    application:unset_env(ssb, ssb_home),
    ok.

membership() ->
    Feed = ~"@abcdef.ed25519",
    ?assertNot(is_member(Feed)),
    ok = add_member(Feed),
    ?assert(is_member(Feed)),
    ?assert(lists:member(Feed, members())),
    %% Survives a restart (persisted to disk).
    ok = gen_server:stop(room_store),
    {ok, _} = room_store:start_link(),
    ?assert(is_member(Feed)),
    ok = remove_member(Feed),
    ?assertNot(is_member(Feed)).

-endif.
