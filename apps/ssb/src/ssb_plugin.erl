%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Behaviour for erlbutt plugins: a plugin owns one or more muxrpc
%% methods, declared in manifest/0 and dispatched to handle_rpc/3 by
%% rpc_processor via plugin_registry.
%%
%% A method name is the muxrpc name path, e.g. [~"whoami"] or
%% [~"blobs", ~"has"].  Kinds follow the muxrpc manifest vocabulary;
%% sync and async are framed identically on the wire (one JSON reply),
%% the distinction only matters to JS clients building their API.
%% duplex methods cannot be registered yet — they need per-connection
%% stream state, which stays in rpc_processor/ssb_peer for now.
%%
%% Permission classes gate who may call a method, derived from the
%% connection's authenticated key:
%%   owner  — the caller proved the node's own keypair (a local client,
%%            e.g. the UI talking to its own server; cf. ssb-master)
%%   member — a registered room member (room_store)
%%   anyone — any peer that completed the handshake on our network
%%
%% handle_rpc/3 receives the caller as a map:
%%   #{feed_id => binary() | undefined, class => owner | member | peer}
%% and returns:
%%   {reply, Term}     for sync/async methods (Term is EJSON:
%%                     {Proplist}, binary, number, boolean, list)
%%   {source, [Term]}  for source methods: each Term is one stream
%%                     item; the stream is closed after the last
%%   {error, Reason}   Reason :: binary(), sent as a muxrpc error frame
%%
%% A reply or stream item may also be {json, Bin}: pre-encoded JSON
%% sent as-is, for plugins that serve stored message bytes without a
%% decode/re-encode round trip.
%%
%% A source method may instead return
%%   {live_source, [{MsgId, Bin}], ViewMod, EventFun}
%% to honour {live: true}: the snapshot pairs are sent, then the
%% stream stays open and a view_stream process pushes every later
%% match — EventFun(ViewEvent) -> {send, MsgId, Bin} | skip, run in
%% the stream process, deduplicated against the snapshot ids.  The
%% stream ends when the client cancels or the connection closes.
-module(ssb_plugin).

-callback manifest() ->
    [{Name :: [binary()],
      Kind :: sync | async | source,
      Perm :: anyone | member | owner}].

-callback handle_rpc(Name :: [binary()], Args :: list(), Caller :: map()) ->
    {reply, term()} | {source, [term()]} | {error, binary()}.
