%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Starts the plugin applications installed beside the node: OTP apps such
%% as ssb_git that are not in the release's boot script, found on disk at
%% boot instead.
%%
%% A plugin is a directory holding an app's ebin (and priv, if it has one):
%%
%%   <data>/plugins/<app>/ebin/<app>.app    added by the operator
%%   <release>/plugins/<app>/ebin/<app>.app shipped in the package
%%
%% The data directory is searched first, so a plugin dropped in there
%% replaces the one the package shipped, and survives an upgrade that
%% replaces the release.  {plugins, all | [App]} in ssb.cfg picks which
%% start (config:plugins/0): `all` is every one installed, and
%% {plugins, []} leaves out even the ones the package ships.
%%
%% For each: the API check (ssb_plugin:check_app/1, which reads the beams
%% without loading them), then the ebin on the code path, then EVERY module
%% the .app lists loaded by hand — the release boots in embedded mode,
%% which loads nothing on demand, so a path alone gives `undef` on the
%% app's own start/2 — then the app started as temporary.  A plugin that
%% fails at any step is logged, its path taken back out, and skipped:
%% nothing a plugin does can keep the node from coming up, or take it
%% down later.
%%
%% Last child of ssb_sup, and the work happens after init returns.  ssb is
%% still starting then, and every plugin depends on it; the application
%% controller holds a start request for an app that is mid-start until it
%% has started, so ensure_all_started here simply waits for ssb.
-module(plugin_loader).

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

-export([start_link/0, list/0, dirs/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% [{App, Dir | none, Status}] for every plugin considered at boot.
%% Status: started | release (in the boot script; left alone) |
%% not_installed | {error, Reason}.
list() ->
    gen_server:call(?MODULE, list, infinity).

%% Where plugins are looked for, first match wins.
dirs() ->
    [filename:join(binary_to_list(config:ssb_repo_loc()), "plugins"),
     filename:join(ssb_paths:release_root(), "plugins")].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, [], {continue, load}}.

handle_continue(load, _State) ->
    {noreply, load_all(config:plugins(), dirs())}.

handle_call(list, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Loading
%%%===================================================================

load_all(Wanted, Dirs) ->
    Installed = installed(Dirs),
    [{App, Dir, start(App, Dir)} || {App, Dir} <- wanted(Wanted, Installed)].

%% [{App, Dir | none}] in the order they will be started.
wanted(all, Installed) ->
    Installed;
wanted(Apps, Installed) ->
    [case lists:keyfind(App, 1, Installed) of
         {App, Dir} -> {App, Dir};
         false      -> {App, none}
     end || App <- Apps].

%% Every plugin under Dirs, one per app, an earlier dir hiding a later
%% one, sorted by name.  A directory counts when its ebin holds exactly
%% one .app file; the app is named by that file, not the directory.
%% (ukeysort keeps the first of equal keys, and sort is stable.)
installed(Dirs) ->
    lists:ukeysort(1, lists:append([in_dir(D) || D <- Dirs])).

in_dir(Dir) ->
    lists:sort(
      [{list_to_atom(filename:basename(AppFile, ".app")), PluginDir}
       || PluginDir <- filelib:wildcard(filename:join(Dir, "*")),
          [AppFile] <- [filelib:wildcard(filename:join([PluginDir, "ebin", "*.app"]))]]).

start(App, none) ->
    ?LOG_ERROR("plugin ~p: not installed in ~s", [App, lists:join(" or ", dirs())]),
    not_installed;
start(App, Dir) ->
    case lists:keymember(App, 1, application:loaded_applications()) of
        true ->
            %% In the boot script (built in the old way), or already here.
            %% Its code is whatever the release loaded; do not add a
            %% second copy to the path.
            ?LOG_NOTICE("plugin ~p: already loaded by the release; ~s is ignored",
                        [App, Dir]),
            release;
        false ->
            Ebin = filename:join(Dir, "ebin"),
            true = code:add_patha(Ebin),
            try load_and_start(App) of
                ok ->
                    {ok, Vsn} = application:get_key(App, vsn),
                    ?LOG_NOTICE("plugin ~p ~s started from ~s", [App, Vsn, Dir]),
                    started;
                {error, Reason} ->
                    give_up(App, Ebin, Reason)
            catch
                Class:Reason ->
                    give_up(App, Ebin, {Class, Reason})
            end
    end.

load_and_start(App) ->
    case ssb_plugin:check_app(App) of
        ok ->
            {ok, Mods} = application:get_key(App, modules),
            case [{M, E} || M <- Mods, {error, _} = E <- [code:load_file(M)]] of
                [] ->
                    case application:ensure_all_started(App, temporary) of
                        {ok, _}          -> ok;
                        {error, _} = Err -> Err
                    end;
                NotLoaded ->
                    {error, {not_loaded, NotLoaded}}
            end;
        {error, _} = Err ->
            Err
    end.

give_up(App, Ebin, Reason) ->
    ?LOG_ERROR("plugin ~p not started: ~p", [App, Reason]),
    _ = application:unload(App),
    _ = code:del_path(Ebin),
    {error, Reason}.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

%% A scratch tree: Spec is [{Root, [{DirName, AppName}]}].
tree(Spec) ->
    Base = filename:join(["/tmp", "plugin_loader_test",
                          integer_to_list(erlang:unique_integer([positive]))]),
    [[begin
          Ebin = filename:join([Base, Root, Dir, "ebin"]),
          ok = filelib:ensure_path(Ebin),
          ok = file:write_file(filename:join(Ebin, App ++ ".app"),
                               io_lib:format("~p.~n", [{application, list_to_atom(App),
                                                        [{vsn, "1"}, {modules, []},
                                                         {applications, [kernel, stdlib]}]}]))
      end || {Dir, App} <- Plugins]
     || {Root, Plugins} <- Spec],
    {Base, [filename:join(Base, R) || {R, _} <- Spec]}.

installed_test() ->
    {Base, [Data, Rel]} = tree([{"data", [{"b", "plug_b"}, {"a-1.0", "plug_a"}]},
                                {"rel",  [{"a", "plug_a"}, {"c", "plug_c"}]}]),
    try
        %% data wins for plug_a; named by the .app, not the directory
        ?assertEqual([{plug_a, filename:join(Data, "a-1.0")},
                      {plug_b, filename:join(Data, "b")},
                      {plug_c, filename:join(Rel, "c")}],
                     installed([Data, Rel])),
        %% a missing directory is no plugins, not an error
        ?assertEqual([], installed([filename:join(Base, "nowhere")]))
    after
        file:del_dir_r(Base)
    end.

%% A directory whose ebin has no .app, or two, is not a plugin.
not_a_plugin_test() ->
    {Base, [Dir]} = tree([{"p", [{"x", "plug_x"}, {"x", "plug_y"}]}]),
    try
        ok = filelib:ensure_path(filename:join([Dir, "empty", "ebin"])),
        ?assertEqual([], installed([Dir]))
    after
        file:del_dir_r(Base)
    end.

wanted_test_() ->
    I = [{a, "/p/a"}, {b, "/p/b"}],
    [?_assertEqual(I, wanted(all, I)),
     ?_assertEqual([], wanted([], I)),
     ?_assertEqual([{b, "/p/b"}, {z, none}], wanted([b, z], I))].

%% A real start, with no ssb involved: an app with no modules starts, and
%% a broken one is given up on with its path removed and nothing left
%% loaded.  check_app passes trivially for an app with no modules.
start_test() ->
    {Base, [Dir]} = tree([{"p", [{"good", "plugin_loader_good"}]}]),
    Bad = filename:join(Dir, "bad"),
    BadEbin = filename:join(Bad, "ebin"),
    ok = filelib:ensure_path(BadEbin),
    ok = file:write_file(filename:join(BadEbin, "plugin_loader_bad.app"),
                         io_lib:format("~p.~n", [{application, plugin_loader_bad,
                                                  [{vsn, "1"}, {modules, [no_such_mod]},
                                                   {applications, [kernel, stdlib]}]}])),
    try
        ?assertEqual(started, start(plugin_loader_good, filename:join(Dir, "good"))),
        ?assert(lists:keymember(plugin_loader_good, 1, application:which_applications())),
        ?assertEqual(release, start(plugin_loader_good, filename:join(Dir, "good"))),
        ?assertMatch({error, _}, start(plugin_loader_bad, Bad)),
        ?assertNot(lists:keymember(plugin_loader_bad, 1, application:loaded_applications())),
        ?assertNot(lists:member(BadEbin, code:get_path()))
    after
        application:stop(plugin_loader_good),
        application:unload(plugin_loader_good),
        code:del_path(filename:join([Dir, "good", "ebin"])),
        file:del_dir_r(Base)
    end.

-endif.
