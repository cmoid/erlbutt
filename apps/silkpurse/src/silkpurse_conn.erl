%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% The conn.* surface the UI's connection panel binds on connect
%% (ssb-conn's api).  For now these are minimal: the peer lists are
%% empty snapshots and connect/remember are accepted no-ops, enough for
%% the renderer to bind without crashing.  They will grow into real
%% peer data backed by peer_registry / conn_db.
%%
%% Also replicate.request (ssb-replicate): the profile page calls it to
%% ask for a once-off replication of a feed ("fog of war" discovery).
%% Accepted no-op — erlbutt's EBT replicates by follow graph; wiring a
%% one-shot feed request into ebt is possible later work.
%%
%% Registered by silkpurse_app (stateless; no view).
-module(silkpurse_conn).

-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

-export([manifest/0, handle_rpc/3]).

manifest() ->
    [{[~"conn", ~"peers"],       source, owner},
     {[~"conn", ~"stagedPeers"], source, owner},
     {[~"conn", ~"connect"],     async,  owner},
     {[~"conn", ~"remember"],    async,  owner},
     {[~"conn", ~"forget"],      async,  owner},
     {[~"replicate", ~"request"], sync,  owner}].

%% A live list stream in ssb-conn; a single empty snapshot is a valid
%% "no peers" state for the panel.  (Entries would be [address, data].)
handle_rpc([~"conn", ~"peers"], _Args, _Caller) ->
    {source, [[]]};
handle_rpc([~"conn", ~"stagedPeers"], _Args, _Caller) ->
    {source, [[]]};

handle_rpc([~"conn", ~"connect"], _Args, _Caller) ->
    {reply, true};
handle_rpc([~"conn", ~"remember"], _Args, _Caller) ->
    {reply, true};
handle_rpc([~"conn", ~"forget"], _Args, _Caller) ->
    {reply, true};

handle_rpc([~"replicate", ~"request"], _Args, _Caller) ->
    {reply, true}.
