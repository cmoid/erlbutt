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

handle_call({fetch, BlobId}, _From, State) ->
    DecBId = utils:decode_id(BlobId),
    {reply, lookup(DecBId), State};

handle_call({store, Blob}, _From, State) ->
    {reply, insert(Blob), State};

handle_call({has, BlobId}, _From, State) ->
    DecBId = utils:decode_id(BlobId),
    {reply, filelib:is_regular(blob_path(DecBId)), State};

handle_call({size_of, BlobId}, _From, State) ->
    DecBId = utils:decode_id(BlobId),
    BlobFile = blob_path(DecBId),
    Result = case filelib:is_regular(BlobFile) of
        true  -> {ok, filelib:file_size(BlobFile)};
        false -> {error, not_found}
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

insert(Blob) ->
    CodedBlob = list_to_binary("&" ++
                                   utils:base_64(crypto:hash(sha256, Blob))
                               ++
                                   ".sha256"),
    PathName = utils:decode_id(CodedBlob),
    BlobFile = blob_path(PathName),
    filelib:ensure_dir(BlobFile),
    %% note that opening with write truncates if file exists
    case file:open(BlobFile, [write]) of
        {ok, F} ->
            file:write(F, Blob),
            file:close(F);
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

-endif.
