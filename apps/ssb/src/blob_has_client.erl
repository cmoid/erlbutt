%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Client-side handler for the blobs.has async RPC response.
-module(blob_has_client).

-include_lib("ssb/include/ssb.hrl").

-behaviour(rpc_behavior).

-export([handle_data/3, handle_data/4]).

handle_data(_ReqNo, _Body, #ssb_conn{nonce = Nonce}) ->
    Nonce.

handle_data(_ReqNo, Body, #ssb_conn{nonce = Nonce}, SinkPid) ->
    Result = try utils:nat_decode(Body) =:= true
             catch _:_ -> false
             end,
    SinkPid ! {has_result, Result},
    Nonce.
