%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Behaviour for materialized views over the message log.
%%
%% A view folds stored messages into derived, queryable state (a follow
%% graph, a name index, thread roots, ...).  view_manager owns the fold
%% lifecycle: it tracks a per-feed sequence checkpoint for every view,
%% replays missed messages at registration, rebuilds from scratch when a
%% view's version bumps, and fans out each newly stored message.  See
%% doc/plugin-architecture.md.
%%
%% Every callback here is called from the view_manager process, never
%% from the view's own process — so a view backed by a gen_server must
%% keep its tables public (or otherwise writable from the manager) and
%% must not implement these callbacks as calls into itself while it is
%% registering.  The convention (see friends) is: the view's gen_server
%% owns named public ETS tables created (or file2tab-restored) in its
%% init, and these callbacks are plain functions over those tables.
%%
%% Callbacks:
%%   view_version() — bump to force a full rebuild after the fold logic
%%                    changes.
%%   view_load()    — report whether durable state survived from the
%%                    last run: ok means "my state matches whatever the
%%                    manager's checkpoints say I consumed"; empty
%%                    forces reset + rebuild.  Must not call into the
%%                    view's own process (deadlocks registration).
%%   view_reset()   — drop all derived state before a rebuild.
%%   view_save()    — flush durable state to disk; called periodically
%%                    and at manager shutdown, always after the
%%                    checkpoint table reflects what was delivered.
%%   view_entry(Msg) — fold one stored #message{} into the view.
%%                    Return {events, [Event]} to have the manager
%%                    publish {view_event, Module, Event} to
%%                    subscribers (view_manager:subscribe/1).
-module(ssb_view).

-include_lib("ssb/include/ssb.hrl").

-callback view_version() -> pos_integer().
-callback view_load() -> ok | empty.
-callback view_reset() -> ok.
-callback view_save() -> ok.
-callback view_entry(#message{}) -> ok | {events, [term()]}.
