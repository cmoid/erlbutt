%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Registry of ssb_plugin modules.  rpc_processor consults it for any
%% incoming muxrpc method it does not handle itself, so plugins extend
%% the node's RPC surface without touching the dispatcher.
%%
%% The registry also owns the node's manifest: the union of the
%% registered plugins' manifests and the builtin (hardcoded) methods,
%% filtered by the caller's permission class.  It serves the `manifest`
%% muxrpc method itself — the first plugin.
-module(plugin_registry).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).
-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

%% API
-export([start_link/0,
         register_plugin/1,
         unregister_plugin/1,
         lookup/1,
         manifest/1,
         allowed/2]).

%% ssb_plugin callbacks
-export([manifest/0, handle_rpc/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, ssb_plugins).

%% Methods implemented directly by rpc_processor.  Listed here so the
%% served manifest covers the full RPC surface; lookup/1 reports them
%% as builtin and dispatch stays where it is.  The perm column controls
%% manifest visibility only — builtin dispatch is not yet gated.
-define(BUILTINS,
        [{[?createhistorystream],       source, anyone},
         {[~"createLogStream"],         source, anyone},
         {[~"createFeedStream"],        source, anyone},
         {[?whoami],                    sync,   anyone},
         {[~"publish"],                 async,  owner},
         {[~"get"],                     async,  anyone},
         {[?gossip, ?ping],             duplex, anyone},
         {[?blobs, ?blobshas],          async,  anyone},
         {[?blobs, ?blobsget],          source, anyone},
         {[?blobs, ?createwants],       source, anyone},
         {[?ebt, ~"replicate"],         duplex, anyone},
         {[?tunnel, ?isRoom],           sync,   anyone},
         {[?tunnel, ?connect],          duplex, anyone},
         {[?tunnel, ?endpoints],        source, anyone},
         {[?room, ?metadata],           async,  anyone},
         {[?room, ?attendants],         source, anyone},
         {[~"invite", ~"create"],       async,  owner},
         {[~"invite", ~"use"],          async,  anyone}]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Register every method in Mod:manifest().  Duplex methods are
%% rejected: the generic dispatch cannot host them yet.
register_plugin(Mod) when is_atom(Mod) ->
    gen_server:call(?MODULE, {register_plugin, Mod}, infinity).

unregister_plugin(Mod) when is_atom(Mod) ->
    gen_server:call(?MODULE, {unregister_plugin, Mod}, infinity).

%% {ok, {Mod, Kind, Perm}} for a registered plugin method,
%% builtin for a method rpc_processor handles itself,
%% unknown otherwise (including when the registry is not running —
%% callers fall back to the pre-registry behavior).
lookup(Name) ->
    try ets:lookup(?TABLE, Name) of
        [{Name, builtin, _Kind, _Perm}] -> builtin;
        [{Name, Mod, Kind, Perm}]       -> {ok, {Mod, Kind, Perm}};
        []                              -> unknown
    catch
        error:badarg -> unknown
    end.

%% Nested EJSON manifest ({Props} form) of every method visible to
%% Class, e.g. {[{~"whoami", ~"sync"}, {~"blobs", {[{~"has", ~"async"}]}}]}.
manifest(Class) ->
    Rows = lists:sort(ets:tab2list(?TABLE)),
    Visible = [{Name, Kind} || {Name, _Mod, Kind, Perm} <- Rows,
                               allowed(Class, Perm)],
    nest_manifest(Visible).

%% Permission lattice: owner > member > peer.
allowed(owner, _Perm)        -> true;
allowed(member, owner)       -> false;
allowed(member, _Perm)       -> true;
allowed(peer, anyone)        -> true;
allowed(peer, _Perm)         -> false.

%%%===================================================================
%%% ssb_plugin callbacks (the registry serves `manifest` itself)
%%%===================================================================

manifest() ->
    [{[~"manifest"], sync, anyone}].

handle_rpc([~"manifest"], _Args, #{class := Class}) ->
    {reply, manifest(Class)}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    ets:new(?TABLE, [named_table, protected, set, {read_concurrency, true}]),
    [ets:insert(?TABLE, {Name, builtin, Kind, Perm})
     || {Name, Kind, Perm} <- ?BUILTINS],
    ok = do_register(?MODULE),
    {ok, #{}}.

handle_call({register_plugin, Mod}, _From, State) ->
    {reply, do_register(Mod), State};

handle_call({unregister_plugin, Mod}, _From, State) ->
    ets:match_delete(?TABLE, {'_', Mod, '_', '_'}),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_register(Mod) ->
    Manifest = Mod:manifest(),
    case validate(Mod, Manifest) of
        ok ->
            [ets:insert(?TABLE, {Name, Mod, Kind, Perm})
             || {Name, Kind, Perm} <- Manifest],
            ok;
        {error, _} = Err ->
            Err
    end.

validate(Mod, Manifest) ->
    try
        lists:foreach(
          fun({Name, Kind, Perm}) ->
                  true = is_list(Name) andalso Name =/= []
                      andalso lists:all(fun is_binary/1, Name),
                  true = lists:member(Kind, [sync, async, source]),
                  true = lists:member(Perm, [anyone, member, owner]),
                  case ets:lookup(?TABLE, Name) of
                      []                        -> ok;
                      [{Name, Mod, _, _}]       -> ok;  %% re-register
                      [{Name, Other, _, _}]     -> throw({taken, Name, Other})
                  end
          end, Manifest),
        ok
    catch
        throw:{taken, Name, Other} ->
            ?SSB_ERROR("plugin_registry: ~p method ~p already owned by ~p",
                       [Mod, Name, Other]),
            {error, {method_taken, Name, Other}};
        _:_ ->
            ?SSB_ERROR("plugin_registry: invalid manifest from ~p: ~p",
                       [Mod, Manifest]),
            {error, {invalid_manifest, Mod}}
    end.

%% [{[~"blobs",~"has"], async}, {[~"whoami"], sync}] ->
%% {[{~"blobs", {[{~"has", ~"async"}]}}, {~"whoami", ~"sync"}]}
nest_manifest(Visible) ->
    Grouped = lists:foldr(
                fun({[Method], Kind}, Acc) ->
                        [{Method, kind_bin(Kind)} | Acc];
                   ({[Ns | Rest], Kind}, Acc) ->
                        case lists:keytake(Ns, 1, Acc) of
                            {value, {Ns, {Sub}}, Acc1} ->
                                [{Ns, {[nested(Rest, Kind) | Sub]}} | Acc1];
                            false ->
                                [{Ns, {[nested(Rest, Kind)]}} | Acc]
                        end
                end, [], Visible),
    {Grouped}.

nested([Method], Kind) ->
    {Method, kind_bin(Kind)}.

kind_bin(sync)   -> ~"sync";
kind_bin(async)  -> ~"async";
kind_bin(source) -> ~"source";
kind_bin(duplex) -> ~"duplex".

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

setup_registry() ->
    {ok, Pid} = start_link(),
    Pid.

cleanup(Pid) ->
    unlink(Pid),
    exit(Pid, shutdown),
    wait_down(Pid).

wait_down(Pid) ->
    case is_process_alive(Pid) of
        true  -> timer:sleep(10), wait_down(Pid);
        false -> ok
    end.

registry_test_() ->
    {setup, fun setup_registry/0, fun cleanup/1,
     fun(_) ->
             [?_test(builtin_lookup()),
              ?_test(unknown_lookup()),
              ?_test(manifest_method()),
              ?_test(perm_matrix()),
              ?_test(manifest_filtering()),
              ?_test(register_and_dispatch()),
              ?_test(method_conflict())]
     end}.

builtin_lookup() ->
    ?assertEqual(builtin, lookup([?whoami])),
    ?assertEqual(builtin, lookup([?blobs, ?blobshas])).

unknown_lookup() ->
    ?assertEqual(unknown, lookup([~"noSuch", ~"method"])).

manifest_method() ->
    {ok, {?MODULE, sync, anyone}} = lookup([~"manifest"]),
    {reply, {Props}} = handle_rpc([~"manifest"], [], #{class => peer}),
    ?assertEqual(~"sync", proplists:get_value(~"manifest", Props)),
    {Blobs} = proplists:get_value(?blobs, Props),
    ?assertEqual(~"async", proplists:get_value(?blobshas, Blobs)).

perm_matrix() ->
    ?assert(allowed(owner, owner)),
    ?assert(allowed(owner, member)),
    ?assert(allowed(owner, anyone)),
    ?assertNot(allowed(member, owner)),
    ?assert(allowed(member, member)),
    ?assert(allowed(member, anyone)),
    ?assertNot(allowed(peer, owner)),
    ?assertNot(allowed(peer, member)),
    ?assert(allowed(peer, anyone)).

manifest_filtering() ->
    {PeerProps}  = manifest(peer),
    {OwnerProps} = manifest(owner),
    %% publish is owner-only: hidden from peers, visible to the owner
    ?assertEqual(undefined, proplists:get_value(~"publish", PeerProps)),
    ?assertEqual(~"async",  proplists:get_value(~"publish", OwnerProps)).

register_and_dispatch() ->
    ok = register_plugin(test_echo_plugin),
    {ok, {test_echo_plugin, async, anyone}} = lookup([~"echo", ~"hello"]),
    {reply, [~"hi"]} =
        test_echo_plugin:handle_rpc([~"echo", ~"hello"], [~"hi"],
                                    #{class => peer, feed_id => undefined}),
    %% registered methods appear in the manifest
    {Props} = manifest(peer),
    {Echo} = proplists:get_value(~"echo", Props),
    ?assertEqual(~"async", proplists:get_value(~"hello", Echo)),
    ok = unregister_plugin(test_echo_plugin),
    ?assertEqual(unknown, lookup([~"echo", ~"hello"])).

method_conflict() ->
    %% a plugin may not take a method someone else owns
    {error, {method_taken, _, _}} = do_register(test_clash_plugin).

-endif.
