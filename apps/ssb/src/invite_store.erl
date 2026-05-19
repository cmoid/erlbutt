%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Stores valid (unconsumed) pub invite public keys.
%% Each invite is an Ed25519 public key (raw binary) derived from the seed
%% embedded in the invite code.
-module(invite_store).

-behaviour(gen_server).

-export([start_link/0, store/1, is_invite/1, validate_and_consume/1, list/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {invites}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

store(InvPk) ->
    gen_server:call(?SERVER, {store, InvPk}).

is_invite(undefined) -> false;
is_invite(InvPk) ->
    gen_server:call(?SERVER, {is_invite, InvPk}).

validate_and_consume(undefined) ->
    {error, invalid};
validate_and_consume(InvPk) ->
    gen_server:call(?SERVER, {validate_and_consume, InvPk}).

list() ->
    gen_server:call(?SERVER, list).

init([]) ->
    process_flag(trap_exit, true),
    Invites = ets:new(invites, [set, private]),
    {ok, #state{invites = Invites}}.

handle_call({store, InvPk}, _From, #state{invites = Invites} = State) ->
    ets:insert(Invites, {InvPk, valid}),
    {reply, ok, State};

handle_call({is_invite, InvPk}, _From, #state{invites = Invites} = State) ->
    {reply, ets:member(Invites, InvPk), State};

handle_call({validate_and_consume, InvPk}, _From, #state{invites = Invites} = State) ->
    Result = case ets:lookup(Invites, InvPk) of
        [{InvPk, valid}] ->
            ets:delete(Invites, InvPk),
            ok;
        _ ->
            {error, invalid}
    end,
    {reply, Result, State};

handle_call(list, _From, #state{invites = Invites} = State) ->
    {reply, [K || {K, _} <- ets:tab2list(Invites)], State};

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
