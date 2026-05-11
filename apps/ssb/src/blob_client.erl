%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Client-side handler for the blobs.createWants duplex stream.
%%
%% When a peer (server) responds to our createWants request it sends
%% have messages: JSON objects keyed by blob ID with positive integer
%% values (the blob's size in bytes).
%%
%% If a process is registered under the name `blob_haves_sink` on the
%% local node, each received have is forwarded to it as
%% `{have, BlobId, Size}`.  In production this process normally does not
%% exist and haves are just logged.  Tests register the sink before
%% initiating wants so they can assert on the responses.
-module(blob_client).

-include_lib("ssb/include/ssb.hrl").

-behaviour(rpc_behavior).

-export([handle_data/3, handle_data/4]).

handle_data(_ReqNo, _Body, #ssb_conn{nonce = Nonce}) ->
    Nonce.

handle_data(ReqNo, Body, #ssb_conn{nonce = Nonce}, SinkPid) ->
    ?SSB_DEBUG("blob_client: handle_data req ~p body ~p~n", [ReqNo, Body]),
    case utils:nat_decode(Body) of
        {Props} ->
            lists:foreach(fun({BlobId, Size}) when is_integer(Size), Size > 0 ->
                                  ?SSB_DEBUG("blob_client: have ~p size ~p~n", [BlobId, Size]),
                                  SinkPid ! {have, BlobId, Size};
                             (_) ->
                                  ok
                          end, Props);
        _ ->
            ok
    end,
    Nonce.
