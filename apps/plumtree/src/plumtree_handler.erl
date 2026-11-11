%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Behaviour that application-level handlers must implement.
%% plumtree_broadcast calls deliver/2 when a new message arrives,
%% and retrieve/1 when a GRAFT requests a payload we have stored.
-module(plumtree_handler).

-callback deliver(MsgId :: binary(), Payload :: binary()) -> ok.
-callback retrieve(MsgId :: binary()) -> {ok, binary()} | not_found.
