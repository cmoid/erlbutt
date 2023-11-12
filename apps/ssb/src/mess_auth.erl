%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(mess_auth).

%% A thin shell around bitcask store used as a simple message-author cache

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ssb.hrl").


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
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {mess_auth}).

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
    CacheDir = ?b2l(config:feed_store_loc()) ++ "mess_auth/",
    Handle = bitcask:open(CacheDir, [read_write]),
    {ok, #state{mess_auth = Handle}}.

handle_call({put, Key, Val}, _From, #state{mess_auth = BitHand} = State) ->
    Ok = bitcask:put(BitHand, Key, Val),
    {reply, Ok, State};

handle_call({get, Key}, _From, #state{mess_auth = BitHand} = State) ->
    Res = bitcask:get(BitHand, Key),
    case Res of
        {ok, Value} ->
            {reply, Value, State};
        not_found ->
            {reply, not_found, State}
    end;

handle_call({sync}, _From, #state{mess_auth = BitHand} = State) ->
    ok = bitcask:sync(BitHand),
    {reply, ok, State};

handle_call({close}, _From, #state{mess_auth = BitHand} = State) ->
    ok = bitcask:close(BitHand),
    {reply, ok, State};

handle_call({auths}, _From, #state{mess_auth = BitHand} = State) ->
    Fun = fun(_M, Auth, Acc) ->
                  Exists = lists:member(Auth, Acc),
                  if Exists ->
                          Acc;
                     true ->
                          [Auth | Acc]
                  end
          end,
    Auths = bitcask:fold(BitHand, Fun, []),
    {reply, Auths, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{mess_auth = BitHand}) ->
    ?LOG_INFO("terminate called: ~p close bitcask ~n",[Reason]),
    bitcask:close(BitHand).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifdef(TEST).

-endif.
