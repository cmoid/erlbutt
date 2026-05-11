%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2025 Charles Moid
%%
%% Handles incoming messages on a peer's blobs.createWants source stream.
%%
%% The SSB blob want/have protocol works as follows:
%%   - Negative values (-1, -2, …) are "want" signals. The magnitude
%%     indicates propagation distance; -1 means direct interest.
%%   - Positive values are "have" signals whose value is the blob size
%%     in bytes.
%%
%% Both peers open independent source streams. When a peer sends us wants
%% we check local storage and respond with haves on OUR OWN stream
%% (ssb_conn.our_wants_req), not on their response channel.
-module(blob_wants).

-include_lib("ssb/include/ssb.hrl").

-behaviour(rpc_behavior).

-export([handle_data/3, handle_data/4]).

-compile({no_auto_import, [size/1]}).
-import(utils, [size/1]).

handle_data(ReqNo, Body, Conn) ->
    handle_data(ReqNo, Body, Conn, undefined).

handle_data(_ReqNo, Body, #ssb_conn{socket = Socket,
                                    nonce = Nonce,
                                    secret_box = Key,
                                    our_wants_req = OurWantsReq}, SinkPid) ->
    case utils:nat_decode(Body) of
        {Props} ->
            maybe_forward_haves(Props, SinkPid),
            respond_to_wants(OurWantsReq, Props, Socket, Nonce, Key);
        _Other ->
            Nonce
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_forward_haves(_Props, undefined) ->
    ok;
maybe_forward_haves(Props, SinkPid) ->
    lists:foreach(fun({BlobId, Val}) when is_integer(Val), Val > 0 ->
                          SinkPid ! {have, BlobId, Val};
                     (_) ->
                          ok
                  end, Props).

respond_to_wants(undefined, _Props, _Socket, Nonce, _Key) ->
    Nonce;
respond_to_wants(OurWantsReq, Props, Socket, Nonce, Key) ->
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
            Header  = rpc_processor:create_header(Flags, size(HaveMsg), OurWantsReq),
            utils:send_data(utils:combine(Header, HaveMsg), Socket, Nonce, Key)
    end.
