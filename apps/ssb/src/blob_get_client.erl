%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Client-side handler for the blobs.get source stream.
%%
%% The server sends one or more binary frames containing the blob data,
%% followed by a JSON `true` frame that signals end-of-stream.
%%
%% handle_data/4 (used by ssb_peer:fetch_blob) sends directly to a
%% provided SinkPid so no global registration is needed.
%%
%% handle_data/3 falls back to looking up the registered `blob_fetch_sink`
%% name, kept for the two-node CT test.
-module(blob_get_client).

-include_lib("ssb/include/ssb.hrl").

-behaviour(rpc_behavior).

-export([handle_data/3, handle_data/4]).

handle_data(_ReqNo, _Body, #ssb_conn{nonce = Nonce}) ->
    Nonce.

handle_data(_ReqNo, Body, #ssb_conn{nonce = Nonce}, SinkPid) ->
    IsEnd = try utils:nat_decode(Body) =:= true
            catch _:_ -> false
            end,
    Msg = case IsEnd of
        true  -> done;
        false -> {chunk, Body}
    end,
    ?SSB_DEBUG("blob_get_client: ~p~n", [Msg]),
    SinkPid ! Msg,
    Nonce.
