%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Where things live: the release (code + shipped config) and the data
%% directory (.ssberl — identity, feeds, blobs, derived state).
%%
%% These used to be one value, SSB_HOME, with the data at
%% $SSB_HOME/.ssberl.  That forced the data to live inside the directory an
%% upgrade replaces (hence the /opt/erlbutt/.ssberl symlink on a pub), and
%% with SSB_HOME defaulting to "." it made the working directory decide
%% where your identity was.  Now the release root is worked out rather than
%% configured, and the data directory has its own setting with a default
%% chosen per build profile.
%%
%% Pure functions, no process: data_version:check/0 needs the data
%% directory before the config server exists, and the two must agree.
-module(ssb_paths).
-include_lib("ssb/include/ssb.hrl").

-export([release_root/0,
         release_cfg/0,
         repo_dir/0,
         repo_dir/1,
         expand/1]).

%% The release root: app env `release_root` (the escript sets it from its
%% own location), else code:root_dir() when it holds an ssb.cfg — inside a
%% release that is ROOTDIR, which relx exports — else the working
%% directory, which is what a node run any other way has always used.
release_root() ->
    case application:get_env(ssb, release_root) of
        {ok, Root} when Root =/= "" ->
            Root;
        _ ->
            Root = code:root_dir(),
            case filelib:is_regular(filename:join(Root, "ssb.cfg")) of
                true  -> Root;
                false -> {ok, Cwd} = file:get_cwd(), Cwd
            end
    end.

release_cfg() ->
    filename:join(release_root(), "ssb.cfg").

repo_dir() ->
    repo_dir(release_cfg()).

%% The data directory, as a binary with a trailing slash (the shape
%% config's repo_loc has always had).  First match wins:
%%
%%   1. app env `data_dir`, then `ssb_home` (as <home>/.ssberl)
%%                                     — tests and embedding.  App env is
%%                                       checked before the OS environment
%%                                       so a test's scratch home can never
%%                                       be redirected by an SSB_DATA or
%%                                       SSB_HOME left set in the shell.
%%   2. SSB_DATA                       — the operator's explicit choice
%%   3. SSB_HOME                       — legacy: <home>/.ssberl, exactly as
%%                                       before, so an existing unit file or
%%                                       two-node recipe keeps its data
%%   4. existing <release>/.ssberl     — an install that predates this
%%   5. {data_dir, Path} in CfgFile    — the profile default (prod: ~/.ssberl)
%%   6. <release>/.ssberl              — dev builds: keep their own identity
%%
%% 4 before 5 is the one that matters.  Without it, upgrading a prod
%% tarball that was keeping its data in the release directory would boot
%% against an empty ~/.ssberl and mint a new identity — the worst failure
%% this node has.  It is used, and loudly reported, until the data moves.
repo_dir(CfgFile) ->
    Root = release_root(),
    InRelease = filename:join(Root, ".ssberl"),
    Dir = case first([data(app_env(data_dir)),
                      under_home(app_env(ssb_home)),
                      data(os_env("SSB_DATA")),
                      under_home(os_env("SSB_HOME"))]) of
              {ok, D} -> D;
              none    -> profile_default(CfgFile, InRelease)
          end,
    ?l2b(trailing_slash(Dir)).

profile_default(CfgFile, InRelease) ->
    case cfg_data_dir(CfgFile) of
        none ->
            InRelease;
        {ok, Want} ->
            Expanded = expand(Want),
            Legacy = filelib:is_regular(filename:join(InRelease, "secret")),
            case Legacy andalso not same_dir(Expanded, InRelease) of
                true ->
                    ?SSB_ERROR("ssb_paths: using data inside the release "
                               "directory (~s), which an upgrade replaces. "
                               "Stop the node and move it to ~s, or set "
                               "SSB_DATA to keep it where it is.",
                               [InRelease, Expanded]),
                    InRelease;
                false ->
                    Expanded
            end
    end.

cfg_data_dir(CfgFile) ->
    case file:consult(CfgFile) of
        {ok, Terms} ->
            case [D || {data_dir, D} <- Terms, is_list(D), D =/= ""] of
                [D | _] -> {ok, D};
                []      -> none
            end;
        _ ->
            none
    end.

%% "~" and "~/x" are the user's home.  Anything else is taken as given and
%% made absolute against the working directory.
expand("~") ->
    home();
expand("~/" ++ Rest) ->
    filename:join(home(), Rest);
expand(Path) ->
    filename:absname(Path).

home() ->
    case os:getenv("HOME") of
        H when is_list(H), H =/= "" -> H;
        _ -> error({no_home, "data_dir uses ~ but HOME is not set; "
                             "set SSB_DATA instead"})
    end.

same_dir(A, B) ->
    real(A) =:= real(B).

%% Resolve symlinks so a pub whose <release>/.ssberl links out to the
%% configured directory is not reported as misplaced.
real(Path) ->
    case file:read_link_all(Path) of
        {ok, Target} -> filename:absname(Target, filename:dirname(Path));
        _            -> filename:absname(Path)
    end.

trailing_slash(Dir) ->
    case lists:last(Dir) of
        $/ -> Dir;
        _  -> Dir ++ "/"
    end.

data({ok, D}) -> {ok, expand(D)};
data(none)    -> none.

%% Concatenated, not joined, to give exactly the path the node used before.
under_home({ok, H}) -> {ok, H ++ "/.ssberl"};
under_home(none)    -> none.

app_env(Key) ->
    case application:get_env(ssb, Key) of
        {ok, V} when is_list(V), V =/= "" -> {ok, V};
        _ -> none
    end.

os_env(Name) ->
    case os:getenv(Name) of
        V when is_list(V), V =/= "" -> {ok, V};
        _ -> none
    end.

first([]) -> none;
first([{ok, _} = Hit | _]) -> Hit;
first([none | Rest]) -> first(Rest).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Every test runs with all four inputs cleared and a scratch release root,
%% and puts back whatever was there — SSB_HOME in particular may be set in
%% the shell the tests were started from.
with_clean_env(Fun) ->
    Root = filename:join(["/tmp", "paths_" ++
                          integer_to_list(erlang:unique_integer([positive]))
                          ++ "_" ++ integer_to_list(rand:uniform(1 bsl 30))]),
    ok = filelib:ensure_dir(filename:join(Root, "x")),
    Saved = [{K, application:get_env(ssb, K)}
             || K <- [data_dir, ssb_home, release_root]],
    SavedOs = [{N, os:getenv(N)} || N <- ["SSB_DATA", "SSB_HOME", "HOME"]],
    [application:unset_env(ssb, K) || {K, _} <- Saved],
    [os:unsetenv(N) || N <- ["SSB_DATA", "SSB_HOME"]],
    application:set_env(ssb, release_root, Root),
    try Fun(Root)
    after
        [case V of
             {ok, X}   -> application:set_env(ssb, K, X);
             undefined -> application:unset_env(ssb, K)
         end || {K, V} <- Saved],
        [case V of
             false -> os:unsetenv(N);
             _     -> os:putenv(N, V)
         end || {N, V} <- SavedOs],
        os:cmd("rm -rf " ++ Root)
    end.

write_cfg(Root, Terms) ->
    File = filename:join(Root, "ssb.cfg"),
    ok = file:write_file(File, [io_lib:format("~p.~n", [T]) || T <- Terms]),
    File.

d(Path) -> ?l2b(Path ++ "/").

%% A dev build — no data_dir in its cfg — keeps its data in the release,
%% so it can never pick up the identity a real node is using.
dev_build_keeps_data_in_release_test() ->
    with_clean_env(fun(Root) ->
        write_cfg(Root, [{network_id, "x"}]),
        ?assertEqual(d(Root ++ "/.ssberl"), repo_dir())
    end).

%% An empty data_dir is what default.vars renders: the same as none.
empty_cfg_data_dir_is_unset_test() ->
    with_clean_env(fun(Root) ->
        write_cfg(Root, [{data_dir, ""}]),
        ?assertEqual(d(Root ++ "/.ssberl"), repo_dir())
    end).

prod_default_is_under_home_test() ->
    with_clean_env(fun(Root) ->
        os:putenv("HOME", Root ++ "/home"),
        write_cfg(Root, [{data_dir, "~/.ssberl"}]),
        ?assertEqual(d(Root ++ "/home/.ssberl"), repo_dir())
    end).

ssb_data_beats_the_profile_default_test() ->
    with_clean_env(fun(Root) ->
        os:putenv("HOME", Root ++ "/home"),
        write_cfg(Root, [{data_dir, "~/.ssberl"}]),
        os:putenv("SSB_DATA", Root ++ "/elsewhere"),
        ?assertEqual(d(Root ++ "/elsewhere"), repo_dir())
    end).

%% The VPS unit sets SSB_HOME=/opt/erlbutt; that must keep meaning
%% /opt/erlbutt/.ssberl whatever the cfg says.
legacy_ssb_home_beats_the_profile_default_test() ->
    with_clean_env(fun(Root) ->
        os:putenv("HOME", Root ++ "/home"),
        write_cfg(Root, [{data_dir, "~/.ssberl"}]),
        os:putenv("SSB_HOME", Root ++ "/opt"),
        ?assertEqual(d(Root ++ "/opt/.ssberl"), repo_dir())
    end).

legacy_app_env_ssb_home_test() ->
    with_clean_env(fun(Root) ->
        application:set_env(ssb, ssb_home, Root ++ "/h"),
        ?assertEqual(d(Root ++ "/h/.ssberl"), repo_dir())
    end).

%% The identity-preserving case: data already in the release wins over
%% the profile default, rather than booting a fresh identity under ~.
existing_release_data_is_not_abandoned_test() ->
    with_clean_env(fun(Root) ->
        os:putenv("HOME", Root ++ "/home"),
        write_cfg(Root, [{data_dir, "~/.ssberl"}]),
        Secret = filename:join([Root, ".ssberl", "secret"]),
        ok = filelib:ensure_dir(Secret),
        ok = file:write_file(Secret, ~"{}"),
        ?assertEqual(d(Root ++ "/.ssberl"), repo_dir())
    end).

%% ...but a pub whose <release>/.ssberl is a symlink to the configured
%% directory is already where it should be, and resolves there.
symlinked_release_data_is_not_misplaced_test() ->
    with_clean_env(fun(Root) ->
        os:putenv("HOME", Root ++ "/home"),
        write_cfg(Root, [{data_dir, "~/.ssberl"}]),
        Real = Root ++ "/home/.ssberl",
        ok = filelib:ensure_dir(Real ++ "/x"),
        ok = file:write_file(Real ++ "/secret", ~"{}"),
        ok = file:make_symlink(Real, Root ++ "/.ssberl"),
        ?assertEqual(d(Real), repo_dir())
    end).

expand_test() ->
    with_clean_env(fun(Root) ->
        os:putenv("HOME", Root),
        ?assertEqual(Root, expand("~")),
        ?assertEqual(Root ++ "/a/b", expand("~/a/b")),
        ?assertEqual("/abs/x", expand("/abs/x"))
    end).

-endif.
