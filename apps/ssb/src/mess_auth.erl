%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(mess_auth).

%% A thin shell around dets store used as a simple message-author cache

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("ssb/include/ssb.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
         put/2,
         get/1,
         close/0,
         sync/0,
         all_auths/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {m_a}).

%%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

put(Key, Val) ->
    gen_server:call(?MODULE, {put, Key, Val}, infinity).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

close() ->
    gen_server:call(?MODULE, {close}).

sync() ->
    gen_server:call(?MODULE, {sync}).

all_auths() ->
    gen_server:call(?MODULE, {auths}, infinity).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    TabName = ?b2l(config:ssb_repo_loc()) ++ "mess_auth",
    {ok, DetsTab} = dets:open_file(mess, [{file, TabName}]),
    {ok, #state{m_a = DetsTab}}.

handle_call({put, Key, Val}, _From, #state{m_a = BitHand} = State) ->
    Ok = dets:insert(BitHand, {Key, Val}),
    {reply, Ok, State};

handle_call({get, Key}, _From, #state{m_a = BitHand} = State) ->
    Res = dets:lookup(BitHand, Key),
    case Res of
        [{Key, Val}] ->
            {reply, Val, State};
        [] ->
            {reply, not_found, State};
        {error, Reason} ->
            {reply, Reason, State}
    end;

handle_call({sync}, _From, #state{m_a = BitHand} = State) ->
    ok = dets:sync(BitHand),
    {reply, ok, State};

handle_call({close}, _From, #state{m_a = BitHand} = State) ->
    ok = dets:close(BitHand),
    {reply, ok, State};

handle_call({auths}, _From, #state{m_a = BitHand} = State) ->
    Fun = fun({_Mess, Auth}) ->
                  {continue, Auth}
          end,
    Auths = dets:traverse(BitHand, Fun),
    {reply, Auths, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{m_a = BitHand}) ->
    ?LOG_INFO("Terminate called for reason: ~p ~n",[Reason]),
    dets:close(BitHand).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifdef(TEST).

-endif.
