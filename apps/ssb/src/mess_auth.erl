%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(mess_auth).

%% msg_id -> author cache.  ETS is the primary store for O(1) concurrent
%% reads and writes with no gen_server serialisation on the hot path.
%% Persistence uses ets:tab2file/file2tab — a single binary snapshot, no
%% DETS overhead or temp files.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("ssb/include/ssb.hrl").

-behaviour(gen_server).

-export([start_link/0,
         put/2,
         get/1,
         close/0,
         sync/0,
         all_auths/0,
         rebuild/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ETS_TAB, mess_auth_cache).
-define(FLUSH_MS, 60000).

-record(state, {file}).

%%%===================================================================
%%% API — hot path bypasses the gen_server entirely
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Direct ETS insert — no gen_server round-trip.
put(Key, Val) ->
    ets:insert(?ETS_TAB, {Key, Val}).

%% Direct ETS lookup — no gen_server round-trip.
get(Key) ->
    case ets:lookup(?ETS_TAB, Key) of
        [{Key, Val}] -> Val;
        []           -> not_found
    end.

%% Rebuild the table by scanning the per-feed store (archives included).
%% Useful after a DETS→ETS migration that lost old mappings, or after
%% any restart where the .ets snapshot was incomplete.
rebuild() ->
    feed_store:fold_all(
        fun(MsgData, _Acc) ->
            try
                #message{id = Id, author = Auth} = message:decode(MsgData, false),
                ets:insert(?ETS_TAB, {Id, Auth})
            catch _:_ -> ok
            end
        end, ok),
    ok.

%% All unique authors — ETS fold with map dedup.
all_auths() ->
    Seen = ets:foldl(fun({_Msg, Auth}, Acc) ->
                         Acc#{Auth => true}
                     end, #{}, ?ETS_TAB),
    maps:keys(Seen).

close() ->
    gen_server:call(?MODULE, sync, infinity).

sync() ->
    gen_server:call(?MODULE, sync, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    File = ?b2l(config:ssb_repo_loc()) ++ "mess_auth.ets",
    case ets:file2tab(File) of
        {ok, ?ETS_TAB} ->
            ok;
        _ ->
            ets:new(?ETS_TAB, [set, public, named_table]),
            rebuild()
    end,
    erlang:send_after(?FLUSH_MS, self(), flush),
    {ok, #state{file = File}}.

handle_call(sync, _From, #state{file = File} = State) ->
    save(File),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush, #state{file = File} = State) ->
    save(File),
    erlang:send_after(?FLUSH_MS, self(), flush),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{file = File}) ->
    ?LOG_INFO("Terminate called for reason: ~p ~n", [Reason]),
    save(File).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

save(File) ->
    ets:tab2file(?ETS_TAB, File, [{sync, true}]).

-ifdef(TEST).
-endif.
