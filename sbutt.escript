#!/usr/bin/env escript
%%! -noshell -noinput
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%
%% sbot — CLI client for a running erlbutt node.
%%
%% Usage:
%%   sbot <command> [args...]
%%
%% Commands:
%%   whoami          Show the local node's SSB identity
%%   id              Alias for whoami
%%   ping            Ping the local node
%%   about           Set own profile name/description/avatar image
%%
%% The escript connects to the local erlbutt node on port 8008 using the
%% shared ~/.ssberl/secret key (same approach as sbot in Node.js: two processes,
%% one SHS connection, same identity on both sides).

-define(INFO(Fmt, Args), io:format(Fmt, Args)).
-define(PORT, 8008).

main(["whoami"])          -> run(fun cmd_whoami/1);
main(["id"])              -> run(fun cmd_whoami/1);
main(["ping"])            -> run(fun cmd_ping/1);
main(["publish" | Args])  -> run(fun(P) -> cmd_publish(P, Args) end);
main(["about" | Args])    -> run(fun(P) -> cmd_about(P, Args) end);
main(["get", Key])        -> run(fun(P) -> cmd_get(P, Key) end);
main(["log"])             -> run(fun cmd_log/1);
main(["feed"])            -> run(fun cmd_log/1);
main(["hist", "--id", Id | Rest]) ->
    Limit = case Rest of
        ["--limit", N] -> list_to_integer(N);
        _              -> -1
    end,
    run(fun(P) -> cmd_hist(P, Id, Limit) end);
main([Cmd | _])     ->
    io:format("Unknown command: ~s~n~n", [Cmd]),
    usage(),
    erlang:halt(1);
main([]) ->
    usage(),
    erlang:halt(1).

%%% Setup ------------------------------------------------------------------

run(CmdFun) ->
    setup(),
    case connect() of
        {ok, Peer} ->
            CmdFun(Peer),
            gen_server:stop(Peer);
        {error, Reason} ->
            io:format("Failed to connect to local erlbutt: ~p~n", [Reason]),
            erlang:halt(1)
    end.

setup() ->
    code:add_path("./_build/default/lib/ssb/ebin/"),
    code:add_path("./_build/default/lib/enacl/ebin/"),
    code:add_path("./_build/default/lib/enacl/priv/"),
    code:add_path("./_build/default/lib/ranch/ebin/"),
    logger:set_primary_config(level, error),
    %% Default to the release dir so the escript shares keys/data with the
    %% running node. Override by setting SSB_HOME in the environment.
    RelDir = "./_build/default/rel/ssb",
    SSBHome = os:getenv("SSB_HOME", RelDir),
    application:set_env(ssb, ssb_home, SSBHome),
    CfgFile = SSBHome ++ "/ssb.cfg",
    {ok, _} = config:start_link(CfgFile),
    {ok, _} = keys:start_link(),
    {ok, _} = network_id_cache:start_link().

connect() ->
    %% Both sides share the same key — we're connecting to ourselves locally.
    RemotePk = base64:decode(keys:pub_key()),
    case ssb_peer:start("localhost", ?PORT, RemotePk) of
        {ok, Peer} -> {ok, Peer};
        Err        -> {error, Err}
    end.

%%% Commands ---------------------------------------------------------------

cmd_whoami(Peer) ->
    case ssb_peer:rpc_call(Peer, [<<"whoami">>], <<"async">>) of
        {ok, Body} ->
            io:format("~s~n", [Body]);
        Err ->
            io:format("Error: ~p~n", [Err])
    end.

cmd_ping(Peer) ->
    case ssb_peer:rpc_call(Peer, [<<"gossip">>, <<"ping">>], <<"duplex">>) of
        {ok, Body} ->
            io:format("pong: ~s~n", [Body]);
        Err ->
            io:format("Error: ~p~n", [Err])
    end.

cmd_publish(Peer, Args) ->
    KVs = parse_kv(Args, []),
    Type = proplists:get_value(type, KVs, "post"),
    Text = proplists:get_value(text, KVs, ""),
    Content = {[{<<"type">>, list_to_binary(Type)},
                {<<"text">>, list_to_binary(Text)}]},
    case ssb_peer:rpc_call(Peer, [<<"publish">>], <<"async">>, [Content]) of
        {ok, Body} ->
            io:format("~s~n", [Body]);
        Err ->
            io:format("Error: ~p~n", [Err])
    end.

%% Publish an "about" message describing our own feed: name, description and
%% an avatar image. The image is stored as a content-addressed blob (in the
%% repo blob dir shared with the running node) and referenced by its hash.
cmd_about(Peer, Args) ->
    KVs    = parse_kv(Args, []),
    Self   = keys:pub_key_disp(),
    Base   = [{<<"type">>, <<"about">>}, {<<"about">>, Self}],
    Fields = Base
        ++ opt_field(<<"name">>, proplists:get_value(name, KVs))
        ++ opt_field(<<"description">>, proplists:get_value(description, KVs))
        ++ image_field(proplists:get_value(image, KVs)),
    case Fields of
        Base ->
            io:format("Nothing to set: pass at least one of "
                      "--name, --description, --image~n"),
            erlang:halt(1);
        _ ->
            Content = {Fields},
            case ssb_peer:rpc_call(Peer, [<<"publish">>], <<"async">>, [Content]) of
                {ok, Body} -> io:format("~s~n", [Body]);
                Err        -> io:format("Error: ~p~n", [Err])
            end
    end.

opt_field(_Key, undefined) -> [];
opt_field(Key, Value)      -> [{Key, list_to_binary(Value)}].

%% Read the image file, store it as a blob, and return [{<<"image">>, Ref}].
image_field(undefined) -> [];
image_field(Path) ->
    case file:read_file(Path) of
        {ok, Data} ->
            {ok, _} = blobs:start_link(),
            BlobId  = blobs:store(Data),
            io:format("stored avatar blob ~s~n", [BlobId]),
            [{<<"image">>, BlobId}];
        {error, Reason} ->
            io:format("Cannot read image ~s: ~p~n", [Path, Reason]),
            erlang:halt(1)
    end.

cmd_get(Peer, Key) ->
    case ssb_peer:rpc_call(Peer, [<<"get">>], <<"async">>, [list_to_binary(Key)]) of
        {ok, Body} ->
            io:format("~s~n", [Body]);
        Err ->
            io:format("Error: ~p~n", [Err])
    end.

cmd_log(Peer) ->
    case ssb_peer:rpc_stream_call(Peer, [<<"createLogStream">>], []) of
        {ok, Bodies} ->
            lists:foreach(fun(B) -> io:format("~s~n", [B]) end, Bodies);
        Err ->
            io:format("Error: ~p~n", [Err])
    end.

cmd_hist(Peer, Id, Limit) ->
    Args = [{[{<<"id">>, list_to_binary(Id)},
              {<<"limit">>, Limit},
              {<<"keys">>, true}]}],
    case ssb_peer:rpc_stream_call(Peer, [<<"createHistoryStream">>], Args) of
        {ok, Bodies} ->
            lists:foreach(fun(B) -> io:format("~s~n", [B]) end, Bodies);
        Err ->
            io:format("Error: ~p~n", [Err])
    end.

%%% Helpers ----------------------------------------------------------------

parse_kv(["--type", V | Rest], Acc)        -> parse_kv(Rest, [{type, V} | Acc]);
parse_kv(["--text", V | Rest], Acc)        -> parse_kv(Rest, [{text, V} | Acc]);
parse_kv(["--name", V | Rest], Acc)        -> parse_kv(Rest, [{name, V} | Acc]);
parse_kv(["--description", V | Rest], Acc) -> parse_kv(Rest, [{description, V} | Acc]);
parse_kv(["--image", V | Rest], Acc)       -> parse_kv(Rest, [{image, V} | Acc]);
parse_kv([_ | Rest], Acc)                  -> parse_kv(Rest, Acc);
parse_kv([], Acc)                          -> Acc.

usage() ->
    io:format("Usage: sbot <command> [args...]~n~n"),
    io:format("Commands:~n"),
    io:format("  whoami                        Show local SSB identity~n"),
    io:format("  id                            Alias for whoami~n"),
    io:format("  ping                          Ping local erlbutt node~n"),
    io:format("  publish --type T --text TEXT  Publish a message~n"),
    io:format("  about [--name N] [--description D] [--image PATH]~n"),
    io:format("                                Set own profile name/description/avatar~n"),
    io:format("  get MSGKEY                    Fetch a message by key~n"),
    io:format("  log                           Stream all messages~n"),
    io:format("  feed                          Alias for log~n"),
    io:format("  hist --id FEEDID [--limit N]  Stream one feed's history~n").
