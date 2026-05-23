%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Shared helpers for CT suites that start isolated peer BEAM nodes.
-module(ssb_ct_helper).

-export([ensure_distributed/0,
         start_ssb_node/4,
         pa_args/0,
         build_dir/0]).

%% Ensure this node is distributed so peer:start/1 can work.
%% Idempotent — safe to call from multiple suites in the same CT run.
ensure_distributed() ->
    case node() of
        nonode@nohost -> net_kernel:start([erlbutt_ct, shortnames]);
        _             -> ok
    end.

%% Start a named peer BEAM node, load code paths, configure and start ssb.
start_ssb_node(Name, DataDir, Port, PAs) ->
    {ok, Peer, Node} = peer:start(#{name => Name, args => PAs}),
    rpc:call(Node, code, add_paths,
             [[filename:join([build_dir(), "lib", "enacl", "priv"])]]),
    rpc:call(Node, application, set_env, [ssb, ssb_home, DataDir]),
    rpc:call(Node, application, set_env, [ssb, port, Port]),
    {ok, _} = rpc:call(Node, application, ensure_all_started, [ssb]),
    {ok, Peer, Node}.

%% Build -pa flag strings for all ebin and test dirs in the test build.
pa_args() ->
    LibDir = filename:join(build_dir(), "lib"),
    {ok, Libs} = file:list_dir(LibDir),
    lists:flatmap(
        fun(Lib) ->
            EbinDir = filename:join([LibDir, Lib, "ebin"]),
            TestDir = filename:join([LibDir, Lib, "test"]),
            Ebin = case filelib:is_dir(EbinDir) of
                true  -> ["-pa", EbinDir];
                false -> []
            end,
            Test = case filelib:is_dir(TestDir) of
                true  -> ["-pa", TestDir];
                false -> []
            end,
            Ebin ++ Test
        end, Libs).

%% code:lib_dir(ssb) = _build/test/lib/ssb; two levels up = _build/test
build_dir() ->
    filename:join(code:lib_dir(ssb), "../..").
