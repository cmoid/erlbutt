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
%% match — EventFun(ViewEvent) run in the stream process returns
%%   {send, MsgId, Bin} — push Bin unless MsgId was in the snapshot
%%                        (for streams of distinct stored messages)
%%   {send, Bin}        — push Bin unconditionally (for value streams
%%                        that re-emit a recomputed value on change)
%%   skip               — emit nothing for this event
%% The stream ends when the client cancels or the connection closes.
%%
%% Plugin API version: a module is registered only if it was compiled
%% against the node's own ?SSB_PLUGIN_API (see ssb.hrl).  The version a
%% module was built with is the -ssb_plugin_api attribute ssb.hrl stamps
%% on it; a module that never included ssb.hrl carries none, has no record
%% layout compiled in, and has nothing to check.
-module(ssb_plugin).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("ssb/include/ssb.hrl").

-export([api_version/0,
         built_api/1,
         check_module/1,
         check_app/1]).

-callback manifest() ->
    [{Name :: [binary()],
      Kind :: sync | async | source,
      Perm :: anyone | member | owner}].

-callback handle_rpc(Name :: [binary()], Args :: list(), Caller :: map()) ->
    {reply, term()} | {source, [term()]} | {error, binary()}.

%%%===================================================================
%%% Plugin API version
%%%===================================================================

%% The version this node implements.
api_version() ->
    ?SSB_PLUGIN_API.

%% The versions Mod was compiled against — [] when it never included
%% ssb.hrl — or not_found.  A loaded module is asked directly; one that
%% is not is read from its beam, since in an embedded-mode release
%% nothing is loaded until someone asks, and a check should not be the
%% thing that loads it.
built_api(Mod) when is_atom(Mod) ->
    case attributes(Mod) of
        {ok, Attrs} -> lists:append([V || {ssb_plugin_api, V} <- Attrs]);
        not_found   -> not_found
    end.

%% ok, or {error, {plugin_api_mismatch, #{module, built, node}}} when Mod
%% was compiled against a version other than the node's.
check_module(Mod) when is_atom(Mod) ->
    Node = api_version(),
    case built_api(Mod) of
        not_found ->
            {error, {no_such_module, Mod}};
        Built ->
            case [V || V <- Built, V =/= Node] of
                [] ->
                    ok;
                [V | _] ->
                    {error, {plugin_api_mismatch,
                             #{module => Mod, built => V, node => Node}}}
            end
    end.

%% check_module/1 over every module App's .app lists: ok or the first
%% error.  For a loader (or a plugin's own start/2) to refuse a whole
%% application before starting any of it, rather than discover the
%% mismatch one registration at a time.
check_app(App) when is_atom(App) ->
    _ = application:load(App),
    case application:get_key(App, modules) of
        {ok, Mods} -> first_error([check_module(M) || M <- Mods]);
        undefined  -> {error, {no_such_app, App}}
    end.

attributes(Mod) ->
    case erlang:module_loaded(Mod) of
        true ->
            {ok, Mod:module_info(attributes)};
        false ->
            case code:which(Mod) of
                Path when is_list(Path) ->
                    case beam_lib:chunks(Path, [attributes]) of
                        {ok, {Mod, [{attributes, Attrs}]}} -> {ok, Attrs};
                        _                                  -> not_found
                    end;
                _ ->
                    not_found
            end
    end.

first_error([])              -> ok;
first_error([ok | Rest])     -> first_error(Rest);
first_error([Err | _])       -> Err.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

%% The #message{} layout of every plugin API version, oldest first.
%% APPEND a new entry when the record changes — never edit an old one: the
%% point of the list is that a version names one layout forever.
-define(MESSAGE_LAYOUTS,
        [{1, [id, previous, author, sequence, timestamp, hash, content,
              signature, received, validated, swapped]}]).

%% If this fails, #message{} changed: append its new layout above and bump
%% ?SSB_PLUGIN_API in ssb.hrl.  Every plugin built against the old one
%% will then be refused instead of crashing.
message_layout_is_pinned_to_the_api_version_test() ->
    {Latest, _} = lists:last(?MESSAGE_LAYOUTS),
    ?assertEqual(Latest, ?SSB_PLUGIN_API),
    ?assertEqual(proplists:get_value(?SSB_PLUGIN_API, ?MESSAGE_LAYOUTS),
                 record_info(fields, message)).

check_module_test_() ->
    [%% compiled with ssb.hrl, so stamped with the current version
     ?_assertEqual([?SSB_PLUGIN_API], built_api(?MODULE)),
     ?_assertEqual(ok, check_module(?MODULE)),
     %% never included ssb.hrl: nothing compiled in, nothing to check
     ?_assertEqual([], built_api(test_echo_plugin)),
     ?_assertEqual(ok, check_module(test_echo_plugin)),
     %% built against a version this node does not implement
     ?_assertEqual({error, {plugin_api_mismatch,
                            #{module => test_stale_plugin, built => 0,
                              node => ?SSB_PLUGIN_API}}},
                   check_module(test_stale_plugin)),
     ?_assertEqual({error, {no_such_module, no_such_module_here}},
                   check_module(no_such_module_here)),
     ?_assertEqual({error, {no_such_app, no_such_app_here}},
                   check_app(no_such_app_here))].

%% Read from the beam when the module is not loaded — the embedded-mode
%% case, where a loader checks before anything is loaded.
built_api_from_an_unloaded_beam_test() ->
    _ = code:purge(test_stale_plugin),
    _ = code:delete(test_stale_plugin),
    _ = code:purge(test_stale_plugin),
    ?assertNot(erlang:module_loaded(test_stale_plugin)),
    ?assertEqual([0], built_api(test_stale_plugin)),
    ?assertNot(erlang:module_loaded(test_stale_plugin)).

-endif.
