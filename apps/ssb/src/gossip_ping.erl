%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% gossip.ping is a long-lived liveness duplex.  The peer (re-)sends a
%% timestamp every few seconds and treats the connection as dead if it stops
%% getting responses.  We answer every frame with our own timestamp, keeping
%% the stream alive for the life of the connection.
-module(gossip_ping).

-include_lib("ssb/include/ssb.hrl").

-compile({no_auto_import,[size/1]}).
-import(utils, [combine/2, size/1, send_data/4]).

-export([handle_data/3]).

handle_data(ReqNo, _Body, #ssb_conn{socket = Socket,
                                    nonce = Nonce,
                                    secret_box = Key}) ->
    TimeStamp = iolist_to_binary(
                  message:ssb_encoder(integer_to_binary(erlang:system_time(millisecond)),
                                      fun message:ssb_encoder/3, [pretty])),
    Flags  = rpc_processor:create_flags(1, 0, 2),
    %% PROBE: a keepalive ping answered on -ReqNo. Should recur every few
    %% seconds for the life of a connection; if these stop, the peer will drop.
    ?SSB_INFO("ROOMDBG gossip.ping keepalive answered on reqno=~p~n", [ReqNo]),
    Header = rpc_processor:create_header(Flags, size(TimeStamp), -ReqNo),
    send_data(combine(Header, TimeStamp), Socket, Nonce, Key).
