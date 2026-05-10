%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Client-side handler for the blobs.get source stream.
%%
%% The server sends one or more binary frames containing the blob data,
%% followed by a JSON `true` frame that signals end-of-stream.
%%
%% Each binary frame is forwarded as `{chunk, Data}` to the process
%% registered as `blob_fetch_sink` on the local node (if any).  On the
%% terminal `true` frame, `done` is sent instead.  Tests register the
%% sink before initiating the fetch; in production the sink is absent
%% and frames are only logged.
-module(blob_get_client).

-include_lib("ssb/include/ssb.hrl").

-behaviour(rpc_behavior).

-export([handle_data/3]).

handle_data(_ReqNo, Body, #ssb_conn{nonce = Nonce}) ->
    IsEnd = try utils:nat_decode(Body) =:= true
            catch _:_ -> false
            end,
    Msg = case IsEnd of
        true  -> done;
        false -> {chunk, Body}
    end,
    ?SSB_DEBUG("blob_get_client: ~p~n", [Msg]),
    case whereis(blob_fetch_sink) of
        undefined -> ok;
        Pid       -> Pid ! Msg
    end,
    Nonce.
