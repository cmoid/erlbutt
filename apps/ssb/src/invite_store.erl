%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Stores valid (unconsumed) pub invite public keys.
%% Each invite is an Ed25519 public key (raw binary) derived from the seed
%% embedded in the invite code.
%%
%% Persisted across restarts via ets:tab2file (same pattern as room_store):
%% an invite is a promise made to someone who is not here yet, so a node
%% restart between issuing a code and its being redeemed must not silently
%% invalidate it.  The table is small and changes only when an invite is
%% issued or consumed, so it is written out on every mutation.
%%
%% Every mutation is logged with the resulting count.  An invite is a
%% standing offer to a stranger, and "how many are outstanding, and which
%% one did that peer use" is not answerable after the fact from anything
%% else the node writes down — the table records what is still valid, not
%% what happened to it.  The invite's public key identifies it; the seed
%% it was derived from is the redeemable secret and is never logged.
-module(invite_store).

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

-export([start_link/0, store/1, is_invite/1, validate_and_consume/1, list/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {invites, file}).

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
    File = binary_to_list(config:ssb_repo_loc()) ++ "invites.tab",
    ok = filelib:ensure_dir(File),
    Invites = case ets:file2tab(File) of
        {ok, T}    -> T;
        {error, _} -> ets:new(invites, [set, private])
    end,
    {ok, #state{invites = Invites, file = File}}.

handle_call({store, InvPk}, _From, #state{invites = Invites} = State) ->
    ets:insert(Invites, {InvPk, valid}),
    persist(State),
    ?SSB_INFO("invite_store: issued invite ~s (~p outstanding)~n",
              [disp(InvPk), count(Invites)]),
    {reply, ok, State};

handle_call({is_invite, InvPk}, _From, #state{invites = Invites} = State) ->
    {reply, ets:member(Invites, InvPk), State};

handle_call({validate_and_consume, InvPk}, _From, #state{invites = Invites} = State) ->
    Result = case ets:lookup(Invites, InvPk) of
        [{InvPk, valid}] ->
            ets:delete(Invites, InvPk),
            persist(State),
            ?SSB_INFO("invite_store: redeemed invite ~s (~p outstanding)~n",
                      [disp(InvPk), count(Invites)]),
            ok;
        _ ->
            %% Worth a line even though it is the expected answer to a
            %% replayed or guessed code: on a public pub it is the only
            %% sign that someone is trying codes that were never valid.
            ?SSB_INFO("invite_store: rejected unknown or spent invite ~s"
                      " (~p outstanding)~n", [disp(InvPk), count(Invites)]),
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

persist(#state{invites = Invites, file = File}) ->
    ets:tab2file(Invites, File).

count(Invites) ->
    ets:info(Invites, size).

%% The invite's public key, in the @…​.ed25519 form it takes on the wire as
%% the redeeming client's handshake identity.  Guarded because a malformed
%% key must not turn a log line into a crash in the store.
disp(InvPk) when is_binary(InvPk) ->
    try utils:display_pub(base64:encode(InvPk))
    catch _:_ -> <<"?">>
    end;
disp(_) ->
    <<"?">>.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

invite_store_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> ?_test(survives_restart()) end}.

setup() ->
    catch gen_server:stop(?SERVER),
    catch gen_server:stop(config),
    Home = filename:join("/tmp", "invite_store_"
                         ++ integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = start_link(),
    Home.

cleanup(Home) ->
    catch gen_server:stop(?SERVER),
    catch gen_server:stop(config),
    os:cmd("rm -rf " ++ Home),
    application:unset_env(ssb, ssb_home),
    ok.

%% An invite issued before a restart is still redeemable after it, and
%% redeeming it is still single-use across a second restart.
survives_restart() ->
    InvPk = crypto:strong_rand_bytes(32),
    ?assertNot(is_invite(InvPk)),
    ok = store(InvPk),
    ?assert(is_invite(InvPk)),
    ok = gen_server:stop(?SERVER),
    {ok, _} = start_link(),
    ?assert(is_invite(InvPk)),
    ?assertEqual(ok, validate_and_consume(InvPk)),
    ?assertEqual({error, invalid}, validate_and_consume(InvPk)),
    ok = gen_server:stop(?SERVER),
    {ok, _} = start_link(),
    ?assertNot(is_invite(InvPk)),
    ?assertEqual({error, invalid}, validate_and_consume(InvPk)).

-endif.
