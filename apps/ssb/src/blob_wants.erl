%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2025 Charles Moid
%%
%% Handles the blobs.createWants duplex stream.
%%
%% The SSB blob want/have protocol works as follows:
%%   - Negative values (-1, -2, …) are "want" signals. The magnitude
%%     indicates propagation distance; -1 means direct interest.
%%   - Positive values are "have" signals whose value is the blob size
%%     in bytes.
%%
%% When a peer sends us wants we check local storage.  For any blob we
%% hold we respond with a have message on the same stream.
-module(blob_wants).

-include_lib("ssb/include/ssb.hrl").

-behaviour(rpc_behavior).

-export([handle_data/3]).

-compile({no_auto_import, [size/1]}).
-import(utils, [size/1]).

%% Called by rpc_processor for each subsequent message on the open
%% createWants duplex stream.
handle_data(ReqNo, Body, #ssb_conn{socket = Socket,
                                   nonce = Nonce,
                                   secret_box = Key}) ->
    case utils:nat_decode(Body) of
        {Props} ->
            respond_to_wants(ReqNo, Props, Socket, Nonce, Key);
        _Other ->
            Nonce
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% For each want entry check whether we hold the blob.
%% Collect all haves into a single response object.
respond_to_wants(ReqNo, Props, Socket, Nonce, Key) ->
    Haves = [{BlobId, BlobSize}
             || {BlobId, Val} <- Props,
                Val < 0,
                {ok, BlobSize} <- [blobs:size_of(BlobId)]],
    case Haves of
        [] ->
            Nonce;
        _ ->
            ?SSB_DEBUG("blob_wants: ~p have ~p~n", [self(), Haves]),
            HaveMsg = utils:encode_rec({Haves}),
            Flags   = rpc_processor:create_flags(1, 0, 2),
            Header  = rpc_processor:create_header(Flags, size(HaveMsg), -ReqNo),
            utils:send_data(utils:combine(Header, HaveMsg), Socket, Nonce, Key)
    end.
