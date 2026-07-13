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

-export([ensure_registered/1,
         ensure_registered/2]).

-callback view_version() -> pos_integer().
-callback view_load() -> ok | empty.
-callback view_reset() -> ok.
-callback view_save() -> ok.
-callback view_entry(#message{}) -> ok | {events, [term()]}.

%% Register Mod with its services, LOUDLY.  The old per-module pattern
%% (try ... catch exit:{noproc,_} -> ok) turned a failed registration
%% into silently missing RPC methods: EarlButt booted with
%% messagesByType unregistered and nothing in the log (July 13 2026).
%%
%% Returns ok when every service accepted (or rejected — see below) the
%% registration, retry when any attempt failed transiently; the caller
%% re-sends itself a timer message and calls again.  Both registries
%% are idempotent, so re-attempting an already-registered service is
%% free.
%%
%% A deterministic rejection ({error, method_taken | invalid_manifest})
%% cannot be fixed by retrying: it is logged at error level and treated
%% as final.  An exception (noproc while the service is down, a timeout,
%% a crash) is transient: logged and retried.
ensure_registered(Mod) ->
    ensure_registered(Mod, [plugin, view]).

ensure_registered(Mod, Services) ->
    Results = [attempt(Service, Mod) || Service <- Services],
    case lists:member(retry, Results) of
        true  -> retry;
        false -> ok
    end.

attempt(plugin, Mod) ->
    classify(plugin, Mod, fun() -> plugin_registry:register_plugin(Mod) end);
attempt(view, Mod) ->
    classify(view, Mod, fun() -> view_manager:register_view(Mod) end).

classify(Service, Mod, F) ->
    try F() of
        ok ->
            ok;
        Rejected ->
            ?SSB_ERROR("~p: ~p registration REJECTED: ~p — its methods "
                       "will be MISSING (not retrying: deterministic)",
                       [Mod, Service, Rejected]),
            ok
    catch
        Class:Reason ->
            ?SSB_ERROR("~p: ~p registration attempt failed: ~p:~p — "
                       "will retry",
                       [Mod, Service, Class, Reason]),
            retry
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ensure_registered_test() ->
    %% no services requested: trivially done
    ?assertEqual(ok, ensure_registered(nosuch_view, [])),
    %% with the registries down, attempts are transient -> retry
    case {whereis(plugin_registry), whereis(view_manager)} of
        {undefined, undefined} ->
            ?assertEqual(retry, ensure_registered(nosuch_view)),
            ?assertEqual(retry, ensure_registered(nosuch_view, [view]));
        _ ->
            ok  %% another fixture's services are up; skip the down-path
    end.
-endif.
