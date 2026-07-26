%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% The admin muxrpc namespace.  Every method is owner-only; see
%% admin.app.src for why that matters here more than elsewhere.
%%
%% The starting set is deliberately small — what was already reachable
%% only by hand: the dialer toggle (maxbutt's `D` key), view status and
%% rebuild, the peer tables, and a node summary.  It is meant to grow as
%% operations prove they are wanted, not to be a framework up front.
-module(admin_rpc).

-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([manifest/0, handle_rpc/3]).

manifest() ->
    [{[~"admin", ~"status"],          async, owner},
     {[~"admin", ~"dialer", ~"status"],  async, owner},
     {[~"admin", ~"dialer", ~"enable"],  async, owner},
     {[~"admin", ~"dialer", ~"disable"], async, owner},
     {[~"admin", ~"dialer", ~"trigger"], async, owner},
     {[~"admin", ~"views", ~"list"],     async, owner},
     {[~"admin", ~"views", ~"rebuild"],  async, owner},
     {[~"admin", ~"peers", ~"known"],     async, owner},
     {[~"admin", ~"peers", ~"connected"], async, owner}].

%%%===================================================================
%%% status
%%%===================================================================

%% A one-call summary of what this node is and how it is configured —
%% the thing you actually want first in a remsh session.
handle_rpc([~"admin", ~"status"], _Args, _Caller) ->
    {WallMs, _} = erlang:statistics(wall_clock),
    {reply, {[{~"id",              keys:pub_key_disp()},
              {~"uptimeMs",        WallMs},
              {~"feeds",           feed_count()},
              {~"dialerEnabled",   dialer_enabled()},
              {~"replicationHops", config:replication_hops()},
              {~"archiveLength",   null_for_undefined(config:archive_length())},
              {~"isRoom",          config:is_room()},
              {~"networkIds",      length(config:network_ids())},
              {~"views",           length(view_manager:views())}]}};

%%%===================================================================
%%% dialer
%%%===================================================================

handle_rpc([~"admin", ~"dialer", ~"status"], _Args, _Caller) ->
    {reply, {[{~"enabled", dialer_enabled()}]}};

handle_rpc([~"admin", ~"dialer", ~"enable"], _Args, _Caller) ->
    ok = peer_dialer:enable(),
    {reply, {[{~"enabled", true}]}};

handle_rpc([~"admin", ~"dialer", ~"disable"], _Args, _Caller) ->
    ok = peer_dialer:disable(),
    {reply, {[{~"enabled", false}]}};

%% Force a dial round now rather than waiting for the next heartbeat.
handle_rpc([~"admin", ~"dialer", ~"trigger"], _Args, _Caller) ->
    peer_dialer:trigger(),
    {reply, {[{~"triggered", true}]}};

%%%===================================================================
%%% views
%%%===================================================================

handle_rpc([~"admin", ~"views", ~"list"], _Args, _Caller) ->
    {reply, [{[{~"module",   atom_to_binary(Mod)},
               {~"class",    atom_to_binary(Class)},
               {~"version",  null_for_undefined(Version)},
               {~"feeds",    Feeds},
               {~"caughtUp", view_manager:caught_up(Mod)}]}
             || {Mod, Class, Version, Feeds} <- view_manager:info()]};

%% Wipe a view's derived state and refold it from the whole log.  Only a
%% currently-registered view may be named: binary_to_existing_atom keeps
%% a caller from minting atoms, and the membership check keeps this from
%% being a way to poke arbitrary modules.
%%
%% The refold is scheduled, not awaited — view_manager runs it in chunks
%% so the node keeps serving.  The reply says `rebuilding`, and
%% admin.views.list reports progress via each view's checkpoint count.
handle_rpc([~"admin", ~"views", ~"rebuild"], [Name], _Caller)
  when is_binary(Name) ->
    case registered_view(Name) of
        {ok, Mod} ->
            case view_manager:rebuild(Mod) of
                ok    -> {reply, {[{~"rebuilding", Name}]}};
                Other -> {error, iolist_to_binary(io_lib:format("~p", [Other]))}
            end;
        error ->
            {error, <<"no such registered view: ", Name/binary>>}
    end;
handle_rpc([~"admin", ~"views", ~"rebuild"], _Args, _Caller) ->
    {error, ~"views.rebuild takes a view module name"};

%%%===================================================================
%%% peers
%%%===================================================================

%% The address book: peers we know how to reach (conn.json).
handle_rpc([~"admin", ~"peers", ~"known"], _Args, _Caller) ->
    Peers = try conn_db:all() catch _:_ -> #{} end,
    {reply, [{[{~"address", Addr} | meta_props(Meta)]}
             || Addr := Meta <- Peers]};

%% Peers with a live connection right now (peer_registry).
handle_rpc([~"admin", ~"peers", ~"connected"], _Args, _Caller) ->
    Rows = try peer_registry:all() catch _:_ -> [] end,
    {reply, [{[{~"id", PubKey}]} || {PubKey, _Pid} <- Rows,
                                    is_binary(PubKey)]}.

%%%===================================================================
%%% Internal
%%%===================================================================

%% peer_dialer may be down (it is a supervised worker, but admin must not
%% crash a connection's rpc_processor if it is restarting).
dialer_enabled() ->
    try peer_dialer:is_enabled() catch _:_ -> false end.

feed_count() ->
    try length(feed_store:feed_dirs()) catch _:_ -> 0 end.

%% JSON has no `undefined`; an unset config value is null.
null_for_undefined(undefined) -> null;
null_for_undefined(V)         -> V.

%% conn.json metadata is a map of binary keys; pass it through as EJSON.
meta_props(Meta) when is_map(Meta) ->
    [{K, V} || K := V <- Meta, is_binary(K)];
meta_props(_) ->
    [].

registered_view(Name) ->
    try
        Mod = binary_to_existing_atom(Name, utf8),
        case lists:member(Mod, view_manager:views()) of
            true  -> {ok, Mod};
            false -> error
        end
    catch _:_ -> error
    end.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

%% Every method is owner-only.  This is the invariant worth a test: the
%% cost of getting it wrong is a remotely triggerable rebuild, not a
%% wrong answer.
manifest_is_owner_only_test() ->
    Perms = lists:usort([Perm || {_Name, _Kind, Perm} <- manifest()]),
    ?assertEqual([owner], Perms).

%% Every method sits under the `admin` prefix, so the namespace cannot
%% collide with the built-in manifest or an application's methods.
manifest_is_namespaced_test() ->
    ?assert(lists:all(fun({[~"admin" | _], _, _}) -> true;
                         (_)                      -> false
                      end, manifest())),
    %% and each is a declared kind the registry accepts
    ?assert(lists:all(fun({_, Kind, _}) -> Kind =:= async end, manifest())).

%% rebuild refuses anything that is not a currently-registered view —
%% including names that are not existing atoms at all.
rebuild_rejects_unknown_view_test() ->
    ?assertMatch({error, _},
                 handle_rpc([~"admin", ~"views", ~"rebuild"],
                            [~"definitely_not_a_module_9f3c"], #{})),
    ?assertMatch({error, _},
                 handle_rpc([~"admin", ~"views", ~"rebuild"],
                            [~"lists"], #{})),
    ?assertMatch({error, _},
                 handle_rpc([~"admin", ~"views", ~"rebuild"], [], #{})).

%% End-to-end through the real registry: the manifest is accepted (not
%% rejected as invalid or method_taken), every method resolves to this
%% module, and the permission lattice actually refuses a non-owner.  The
%% unit tests above check the manifest data; this checks that the
%% registry agrees.
registers_and_gates_test_() ->
    {setup,
     fun() -> {ok, P} = plugin_registry:start_link(), P end,
     fun(P) -> gen_server:stop(P) end,
     fun(_) ->
             [?_assertEqual(ok, plugin_registry:register_plugin(admin_rpc)),
              ?_test(every_method_resolves()),
              ?_test(non_owner_is_refused())]
     end}.

every_method_resolves() ->
    ok = plugin_registry:register_plugin(admin_rpc),
    [?assertEqual({ok, {admin_rpc, Kind, Perm}},
                  plugin_registry:lookup(Name))
     || {Name, Kind, Perm} <- manifest()],
    ok.

non_owner_is_refused() ->
    ok = plugin_registry:register_plugin(admin_rpc),
    [begin
         {ok, {admin_rpc, _Kind, Perm}} = plugin_registry:lookup(Name),
         ?assert(plugin_registry:allowed(owner, Perm)),
         ?assertNot(plugin_registry:allowed(member, Perm)),
         ?assertNot(plugin_registry:allowed(peer, Perm))
     end || {Name, _, _} <- manifest()],
    ok.

%% With the services down these must answer, not crash a connection.
degrades_without_services_test() ->
    case whereis(peer_dialer) of
        undefined ->
            ?assertEqual({reply, {[{~"enabled", false}]}},
                         handle_rpc([~"admin", ~"dialer", ~"status"], [], #{})),
            ?assertMatch({reply, _},
                         handle_rpc([~"admin", ~"peers", ~"known"], [], #{})),
            ?assertMatch({reply, _},
                         handle_rpc([~"admin", ~"peers", ~"connected"], [], #{}));
        _ ->
            ok      %% another fixture's services are up; skip the down-path
    end.

-endif.
