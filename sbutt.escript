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
%%   health          Node, view and derived-store health report
%%
%% The escript connects to the local erlbutt node on port 8008 using the
%% shared ~/.ssberl/secret key (same approach as sbot in Node.js: two processes,
%% one SHS connection, same identity on both sides).

-define(INFO(Fmt, Args), io:format(Fmt, Args)).
-define(PORT, 8008).

main(["health"])          -> run(fun cmd_health/1);
main(["whoami"])          -> run(fun cmd_whoami/1);
main(["id"])              -> run(fun cmd_whoami/1);
main(["ping"])            -> run(fun cmd_ping/1);
main(["publish" | Args])  -> run(fun(P) -> cmd_publish(P, Args) end);
main(["about" | Args])    -> run(fun(P) -> cmd_about(P, Args) end);
main(["get", Key])        -> run(fun(P) -> cmd_get(P, Key) end);
main(["invite", "create", Host, PortStr]) ->
    run(fun(P) -> cmd_invite_create(P, Host, list_to_integer(PortStr)) end);
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
    logger:set_primary_config(level, error),
    %% Default to the dev release dir so the escript shares keys/data with
    %% the running node. Override by setting SSB_HOME in the environment.
    SSBHome = os:getenv("SSB_HOME", "./_build/default/rel/ssb"),
    add_code_paths(SSBHome),
    application:set_env(ssb, ssb_home, SSBHome),
    CfgFile = SSBHome ++ "/ssb.cfg",
    {ok, _} = config:start_link(CfgFile),
    {ok, _} = keys:start_link(),
    {ok, _} = network_id_cache:start_link(),
    %% ssb_peer is a full bidirectional peer, so the node we call answers
    %% back — in particular with blob `createWants`, which the connection
    %% tries to serve.  Against a node holding real blobs that arrives
    %% immediately, so the servers those handlers touch have to exist
    %% here too or the connection dies before any reply is read.
    {ok, _} = blobs:start_link(),
    {ok, _} = mess_auth:start_link().

%% Find the beams in either layout.
%%
%% The dev tree (_build/default/lib/*/ebin) exists only in a checkout; a
%% deployed node has the release layout (<home>/lib/<app>-<vsn>/ebin) and
%% no _build at all, which is why this used to crash on the box with
%% `undef` before it could report anything useful.  Both are globbed, so
%% neither has to exist.  esqlite is picked up by the glob too — its NIF
%% is loaded on demand, so a missing one only matters if a command
%% touches the store.
add_code_paths(SSBHome) ->
    Paths = filelib:wildcard("./_build/default/lib/*/ebin")
        ++ filelib:wildcard(filename:join(SSBHome, "lib/*/ebin"))
        ++ filelib:wildcard(filename:join(SSBHome, "lib/*/priv")),
    [code:add_path(P) || P <- Paths],
    case code:ensure_loaded(ssb_peer) of
        {module, _} ->
            ok;
        _ ->
            io:format("Cannot find the erlbutt beams.~n"
                      "Looked in ./_build/default/lib and ~s/lib.~n"
                      "Set SSB_HOME to the node's release directory.~n",
                      [SSBHome]),
            erlang:halt(1)
    end.

connect() ->
    %% Both sides share the same key — we're connecting to ourselves locally.
    RemotePk = base64:decode(keys:pub_key()),
    case ssb_peer:start("localhost", ?PORT, RemotePk) of
        {ok, Peer} -> {ok, Peer};
        Err        -> {error, Err}
    end.

%%% Commands ---------------------------------------------------------------

%% One-shot health report over the admin namespace.
%%
%% Everything here is owner-only, and this escript authenticates with the
%% node's own key, so it connects as owner — which also makes running it
%% an end-to-end check that SHS, muxrpc, the plugin registry and the admin
%% app are all working, before it reports on anything else.
%%
%% Exits non-zero if a view is not caught up or the store looks empty, so
%% it can be used as a post-deploy gate.
%% Each section is fetched and printed before the next is asked for.
%%
%% Gathering all four first meant one failed call threw away the three
%% that had succeeded — and on a node mid-resync the connection really
%% does get dropped, so the report has to survive losing it partway.
cmd_health(Peer) ->
    Status = admin_call(Peer, [<<"admin">>, <<"status">>]),
    io:format("~n== node ==~n"),
    case Status of
        {Props} ->
            io:format("  id              ~s~n", [gv(<<"id">>, Props, <<"?">>)]),
            io:format("  uptime          ~s~n",
                      [fmt_uptime(gv(<<"uptimeMs">>, Props, 0))]),
            io:format("  feeds on disk   ~p~n", [gv(<<"feeds">>, Props, 0)]),
            io:format("  replication     ~p hops, dialer ~s~n",
                      [gv(<<"replicationHops">>, Props, 0),
                       onoff(gv(<<"dialerEnabled">>, Props, false))]),
            io:format("  views           ~p registered~n",
                      [gv(<<"views">>, Props, 0)]);
        Other ->
            io:format("  UNAVAILABLE: ~p~n", [Other])
    end,

    Views = admin_call(Peer, [<<"admin">>, <<"views">>, <<"list">>]),
    io:format("~n== views ==~n"),
    Lagging = report_views(Views),

    %% count(*) over every table; on a large store this is the slow call
    Tables = admin_call(Peer, [<<"admin">>, <<"store">>, <<"tables">>]),
    io:format("~n== store ==~n"),
    EmptyStore = report_tables(Tables),

    Peers = admin_call(Peer, [<<"admin">>, <<"peers">>, <<"connected">>]),
    io:format("~n== peers ==~n"),
    case Peers of
        L when is_list(L), L =/= [] ->
            [io:format("  connected  ~s~n", [gv(<<"id">>, P, <<"?">>)])
             || {P} <- L];
        _ ->
            io:format("  (none connected)~n")
    end,

    io:format("~n"),
    case {Lagging, EmptyStore} of
        {[], false} ->
            io:format("OK~n");
        {[], true} ->
            io:format("WARNING: derived store has no rows yet~n"),
            erlang:halt(2);
        {Mods, _} ->
            io:format("WARNING: still catching up: ~s~n",
                      [lists:join(", ", Mods)]),
            erlang:halt(2)
    end.

%% Returns the modules that are not caught up.
report_views([]) ->
    io:format("  (none registered)~n"),
    [];
report_views(L) when is_list(L) ->
    lists:filtermap(
      fun({P}) ->
              Mod   = gv(<<"module">>, P, <<"?">>),
              Class = gv(<<"class">>, P, <<"?">>),
              Ready = gv(<<"caughtUp">>, P, false),
              io:format("  ~-24s ~-5s v~-3s ~6s feeds  ~s~n",
                        [Mod, Class,
                         num(gv(<<"version">>, P, 0)),
                         num(gv(<<"feeds">>, P, 0)),
                         case Ready of true -> "ready"; _ -> "CATCHING UP" end]),
              case Ready of
                  true -> false;
                  _    -> {true, binary_to_list(Mod)}
              end
      end, L);
report_views(Other) ->
    io:format("  UNAVAILABLE: ~p~n", [Other]),
    [].

%% Returns true when every table is empty (a store that exists but holds
%% nothing, which after a resync means indexing is not happening).
report_tables([]) ->
    io:format("  (store unavailable)~n"),
    true;
report_tables(L) when is_list(L) ->
    Counts = [begin
                  Rows = gv(<<"rows">>, P, -1),
                  io:format("  ~-24s ~8s rows~n",
                            [gv(<<"table">>, P, <<"?">>), num(Rows)]),
                  Rows
              end || {P} <- L],
    %% ssb_schema and ssb_view_state are bookkeeping; ignore them when
    %% deciding whether anything real got indexed.
    Real = [R || {{P}, R} <- lists:zip(L, Counts),
                 not lists:member(gv(<<"table">>, P, <<>>),
                                  [<<"ssb_schema">>, <<"ssb_view_state">>])],
    lists:all(fun(R) -> R =< 0 end, Real);
report_tables(Other) ->
    io:format("  UNAVAILABLE: ~p~n", [Other]),
    true.

%% A dropped connection or a slow reply must degrade this report, not end
%% it — the sections already printed are the ones you came for.
admin_call(Peer, Name) ->
    try ssb_peer:rpc_call(Peer, Name, <<"async">>) of
        {ok, Body} ->
            try utils:nat_decode(Body)
            catch _:_ -> {error, undecodable}
            end;
        Err ->
            {error, Err}
    catch
        exit:{shutdown, conn_closed} -> {error, connection_closed};
        exit:{timeout, _}            -> {error, timeout};
        Class:Reason                 -> {error, {Class, Reason}}
    end.

gv(Key, Props, Default) ->
    case lists:keyfind(Key, 1, Props) of
        {Key, V} -> V;
        false    -> Default
    end.

onoff(true) -> "on";
onoff(_)    -> "off".

%% ~p does not accept a negative (left-justifying) field width, so numbers
%% are rendered to a string first and padded with ~s.
num(N) when is_integer(N) -> integer_to_list(N);
num(N)                    -> io_lib:format("~p", [N]).

fmt_uptime(Ms) when is_integer(Ms) ->
    S = Ms div 1000,
    io_lib:format("~pd ~ph ~pm", [S div 86400, (S rem 86400) div 3600,
                                  (S rem 3600) div 60]);
fmt_uptime(_) ->
    "?".

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

%% Ask the running node to mint a pub invite code for clients reaching us at
%% Host:Port. The node stores the invite keypair in its own invite_store so it
%% can later validate the redemption (invite.use); creating it in this escript's
%% process would store it in a store the node never sees.
cmd_invite_create(Peer, Host, Port) ->
    Args = [list_to_binary(Host), Port],
    case ssb_peer:rpc_call(Peer, [<<"invite">>, <<"create">>], <<"async">>, Args) of
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
    io:format("  invite create HOST PORT       Mint a pub invite code~n"),
    io:format("  log                           Stream all messages~n"),
    io:format("  feed                          Alias for log~n"),
    io:format("  hist --id FEEDID [--limit N]  Stream one feed's history~n"),
    io:format("  health                        Node/view/store health report~n").
