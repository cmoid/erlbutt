%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2025 Charles Moid
-module(blobs).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-behaviour(gen_server).

%% API
-export([start_link/0,
         fetch/1,
         store/1,
         store_verified/2,
         has/1,
         size_of/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/1]).

-define(SERVER, ?MODULE).
-define(CHUNK_SIZE, 65536).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

fetch(BlobId) ->
    gen_server:call(?MODULE, {fetch, BlobId}).

store(Blob) ->
    gen_server:call(?MODULE, {store, Blob}).

%% Store Blob only if its hash matches BlobId; ok | {error, hash_mismatch}.
%% Used when fetching blobs from remote peers.
store_verified(BlobId, Blob) ->
    gen_server:call(?MODULE, {store_verified, BlobId, Blob}).

has(BlobId) ->
    gen_server:call(?MODULE, {has, BlobId}).

size_of(BlobId) ->
    gen_server:call(?MODULE, {size_of, BlobId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%% Blob ids arrive from the network; a degenerate id (e.g. all-zero
%% bytes, whose decoded hex is shorter than the 2-char dir prefix)
%% must answer not_found, not crash this singleton and cascade into
%% the connection.
handle_call({fetch, BlobId}, _From, State) ->
    Reply = try lookup(utils:decode_id(BlobId))
            catch _:_ -> {error, not_found}
            end,
    {reply, Reply, State};

handle_call({store, Blob}, _From, State) ->
    {reply, insert(Blob), State};

handle_call({store_verified, BlobId, Blob}, _From, State) ->
    Reply = case hash_id(Blob) of
        BlobId -> insert(Blob), ok;
        _      -> {error, hash_mismatch}
    end,
    {reply, Reply, State};

handle_call({has, BlobId}, _From, State) ->
    Reply = try filelib:is_regular(blob_path(utils:decode_id(BlobId)))
            catch _:_ -> false
            end,
    {reply, Reply, State};

handle_call({size_of, BlobId}, _From, State) ->
    Result = try
                 BlobFile = blob_path(utils:decode_id(BlobId)),
                 case filelib:is_regular(BlobFile) of
                     true  -> {ok, filelib:file_size(BlobFile)};
                     false -> {error, not_found}
                 end
             catch _:_ -> {error, not_found}
             end,
    {reply, Result, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% leave this for possible debugging use
format_status(Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

blob_path(DecBId) ->
    BlobLoc = config:blob_loc(),
    <<Dir:2/binary, RestBlob/binary>> = DecBId,
    <<BlobLoc/binary, Dir/binary, "/", RestBlob/binary>>.

lookup(DecBId) ->
    BlobFile = blob_path(DecBId),
    case file:read_file(BlobFile) of
        {ok, Blob}       -> {ok, Blob};
        {error, _Reason} -> {error, not_found}
    end.

hash_id(Blob) ->
    list_to_binary("&" ++
                       utils:base_64(crypto:hash(sha256, Blob))
                   ++
                       ".sha256").

insert(Blob) ->
    CodedBlob = hash_id(Blob),
    PathName = utils:decode_id(CodedBlob),
    BlobFile = blob_path(PathName),
    filelib:ensure_dir(BlobFile),
    %% note that opening with write truncates if file exists
    case file:open(BlobFile, [write]) of
        {ok, F} ->
            file:write(F, Blob),
            file:close(F),
            %% feeds live blobs.ls streams (silkpurse_blobs); catch so a
            %% missing view_manager/pg (eunit) never fails the store
            catch view_manager:notify(blobs, {blob, CodedBlob});
        Else ->
            ?LOG_INFO("Tried to open failed: ~p ~n", [Else])
    end,
    CodedBlob.

-ifdef(TEST).

simple_blob_round_trip_test() ->
    Coded = ~"&ybENuaMAdmfjmwR852FNDsj3biaMl5P4HF/jJj7OtQQ=.sha256",

    {ok, Pid} = config:start_link("test/ssb.cfg"),
    {ok, Pid2} = blobs:start_link(),

    %% read a blob
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "b10db9a3007667e39b047ce7614d0ec8f76e268c9793f81c5fe3263eceb504",
    {ok, Blob} = file:read_file(F),

    %% store blob
    StoreCode = blobs:store(Blob),

    ?assert(StoreCode == Coded),
    {ok, NewBlob} = blobs:fetch(StoreCode),
    ?assert(Blob == NewBlob),
    ?assert(blobs:has(StoreCode) == true),
    {ok, BlobSize} = blobs:size_of(StoreCode),
    ?assert(BlobSize == byte_size(Blob)),
    gen_server:stop(Pid),
    gen_server:stop(Pid2).

store_verified_test() ->
    {ok, Pid} = config:start_link("test/ssb.cfg"),
    {ok, Pid2} = blobs:start_link(),

    Blob = ~"store verified test payload",
    GoodId = list_to_binary("&" ++ utils:base_64(crypto:hash(sha256, Blob))
                            ++ ".sha256"),
    ?assertEqual(ok, blobs:store_verified(GoodId, Blob)),
    ?assert(blobs:has(GoodId)),

    BadId = list_to_binary("&" ++ utils:base_64(crypto:hash(sha256, ~"other"))
                           ++ ".sha256"),
    ?assertEqual({error, hash_mismatch}, blobs:store_verified(BadId, Blob)),
    ?assertNot(blobs:has(BadId)),
    gen_server:stop(Pid),
    gen_server:stop(Pid2).

degenerate_id_test() ->
    {ok, Pid} = config:start_link("test/ssb.cfg"),
    {ok, Pid2} = blobs:start_link(),
    %% all-zero bytes: decode_id strips leading zeros, leaving less than
    %% the 2-char dir prefix — must answer not_found, not crash
    Zero = list_to_binary("&" ++ utils:base_64(<<0:256>>) ++ ".sha256"),
    ?assertNot(blobs:has(Zero)),
    ?assertEqual({error, not_found}, blobs:fetch(Zero)),
    ?assertEqual({error, not_found}, blobs:size_of(Zero)),
    ?assert(is_process_alive(whereis(blobs))),
    gen_server:stop(Pid),
    gen_server:stop(Pid2).

-endif.
