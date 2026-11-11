%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Stream sink for tunnelled muxrpc frames.  Registered with rpc_processor as
%% {tunnel_relay, SinkPid}; each frame on the stream is forwarded verbatim to
%% SinkPid as {tunnel_data, ReqNo, Body}.  No socket write happens here, so the
%% connection nonce is returned unchanged (replies go out via ssb_peer on the
%% peer that owns the destination socket).
-module(tunnel_relay).

-include_lib("ssb/include/ssb.hrl").

-export([handle_data/4]).

handle_data(ReqNo, Body, #ssb_conn{nonce = Nonce}, SinkPid) ->
    SinkPid ! {tunnel_data, ReqNo, Body},
    Nonce.
