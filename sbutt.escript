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
%%   census encoding Count stored messages that cannot be encoded for a client
%%
%% The escript connects to the local erlbutt node on port 8008 using the
%% shared ~/.ssberl/secret key (same approach as sbot in Node.js: two processes,
%% one SHS connection, same identity on both sides).

-define(INFO(Fmt, Args), io:format(Fmt, Args)).
-define(PORT, 8008).

%% Idle timeout while draining a source stream: how long to wait for the
%% NEXT frame, not for the whole stream.  A long history keeps arriving, so
%% a total timeout would abort a perfectly healthy fetch.
-define(STREAM_IDLE_MS, 30000).

main(["health"])          -> run(fun cmd_health/1);
main(["census", "encoding" | Args]) ->
    run_local(fun() -> cmd_census_encoding(Args) end);
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
            %% The peer process stops itself when the connection drops,
            %% which on a busy node can happen while a command is still
            %% running.  Stopping an already-dead process raised noproc
            %% AFTER the command had printed its results — including
            %% clobbering health's exit code, which is meant to be usable
            %% as a post-deploy gate.
            catch gen_server:stop(Peer),
            ok;
        {error, Reason} ->
            io:format("Failed to connect to local erlbutt: ~p~n", [Reason]),
            erlang:halt(1)
    end.

%% For commands that read the node's files directly instead of calling it.
%%
%% The logs are append-only, so folding them alongside a running node is
%% safe and — more to the point — free: a census of a few million messages
%% takes minutes, and doing that inside the node would block whatever
%% process ran it for the duration.  No connection, no keys, no blobs;
%% config is the only thing needed, to find the feed store.
run_local(CmdFun) ->
    logger:set_primary_config(level, error),
    SSBHome = os:getenv("SSB_HOME", "./_build/default/rel/ssb"),
    add_code_paths(SSBHome),
    application:set_env(ssb, ssb_home, SSBHome),
    {ok, _} = config:start_link(SSBHome ++ "/ssb.cfg"),
    CmdFun().

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
    %% immediately, so blobs has to exist here too or the connection dies
    %% before any reply is read.
    %%
    %% Only blobs.  Starting node services in a client is a slope: each
    %% one drags in whatever IT depends on, and this escript is not the
    %% node.  mess_auth used to be started here as well, on the guess that
    %% an inbound handler might want it; when mess_auth moved into
    %% ssb_store that guess turned into a boot crash on every invocation,
    %% because a client has no business opening the node's database.
    {ok, _} = blobs:start_link().

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
                      [gv(<<"views">>, Props, 0)]),
            io:format("  invalid sigs    ~s~n", [sig_line(Props)]);
        Other ->
            io:format("  UNAVAILABLE: ~p~n", [Other])
    end,
    BadSigs = case Status of
                  {P0} -> gv(<<"invalidSignatures">>, P0, 0);
                  _    -> 0
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
    case {Lagging, EmptyStore, BadSigs} of
        {[], false, 0} ->
            io:format("OK~n");
        {[], false, N} ->
            %% Not a failure in itself — during the measure phase this is
            %% the number we are here to find — but it is exactly what a
            %% gate should stop on rather than let scroll past.
            io:format("WARNING: ~s message(s) failed the signature check~n",
                      [num(N)]),
            erlang:halt(2);
        {[], true, _} ->
            io:format("WARNING: derived store has no rows yet~n"),
            erlang:halt(2);
        {Mods, _, _} ->
            io:format("WARNING: still catching up: ~s~n",
                      [lists:join(", ", Mods)]),
            erlang:halt(2)
    end.

%%% Encoding census ---------------------------------------------------------
%%
%% Count the stored messages that cannot be encoded for a client.
%%
%% This was written to measure a problem that, on the evidence, does not
%% exist in stored content, and it is kept because that is worth being
%% able to demonstrate rather than assume.
%%
%% The premise was that stored SSB content is not guaranteed to be valid
%% UTF-8 — messages from the wild carry latin1 bytes — so a message could
%% replicate, validate and store perfectly and still be unservable, which
%% is what rpc_processor's `{invalid_byte, 252}` looked like.  But Erlang's
%% json module rejects a bad byte symmetrically: raw latin1, an overlong
%% sequence and a lone surrogate escape all fail on DECODE.  A frame that
%% will not decode never gets stored, and one that does decode holds only
%% binaries json already accepted.  So an unencodable STORED message is
%% close to unreachable, and an `{invalid_byte, _}` on a reply points at a
%% term erlbutt built — a raw key or hash that never went through JSON —
%% rather than at anything a peer sent us.
%%
%% Both columns are therefore worth having.  `encode failures` should be
%% zero and its job is to keep saying so, cheaply, whenever the encoder or
%% the json module changes under us.  `decode failures` is the one that
%% can move, and it means damaged storage — a torn or corrupted frame —
%% which is a different problem reported differently.
cmd_census_encoding(Args) ->
    Limit = arg_int(Args, "--limit", -1),
    Feeds = feed_store:feed_dirs(),
    io:format("~nScanning ~s feed(s)", [num(length(Feeds))]),
    case Limit > 0 of
        true  -> io:format(" (stopping after ~s messages)", [num(Limit)]);
        false -> ok
    end,
    io:format("...~n"),
    T0 = erlang:monotonic_time(millisecond),
    Acc = census_fold(Feeds, Limit),
    Ms  = erlang:monotonic_time(millisecond) - T0,
    report_census(Acc, length(Feeds), Ms).

%% Folded feed by feed rather than through feed_store:fold_all/2 so a
%% failure can be attributed to a directory.  A decode failure has no
%% author or sequence to report — that is exactly what could not be read —
%% so without the directory there would be no way to find it again.
census_fold(Feeds, Limit) ->
    Acc0 = #{total => 0, decode => 0, encode => 0,
             reasons => #{}, feeds => #{}, bad_feeds => #{}, samples => []},
    %% foldl cannot stop early, so --limit unwinds with a throw
    try lists:foldl(
          fun(Dir, A) ->
                  feed_store:fold_feed(
                    fun(Data, A1) -> census_one(Data, A1, Dir, Limit) end,
                    A, Dir)
          end, Acc0, Feeds)
    catch throw:{limit, A} -> A
    end.

census_one(_Data, #{total := N} = Acc0, _Dir, Limit) when Limit > 0, N >= Limit ->
    throw({limit, Acc0});
%% Deliberately NOT message:decode/2 + message:encode/1: the term that
%% actually fails is the one rpc_processor hands to encode_json, which is
%% the decoded stored frame itself.  Going through the record would also
%% mean matching on #message{} from an escript, where -include_lib cannot
%% resolve until the code path is set — which is too late.
census_one(Data, #{total := N} = Acc0, Dir, _Limit) ->
    Acc = progress(Acc0#{total := N + 1}),
    case (catch utils:nat_decode(Data)) of
        {Props} when is_list(Props) ->
            try iolist_to_binary(
                  message:ssb_encoder({Props}, fun message:ssb_encoder/3,
                                      [pretty])) of
                Bin when is_binary(Bin) -> Acc
            catch Class:Reason ->
                    note_failure(Props, {Class, Reason}, Acc)
            end;
        Err ->
            note_undecodable(Dir, Err, Acc)
    end.

note_undecodable(Dir, Err, #{decode := D, bad_feeds := B, reasons := R} = Acc) ->
    Reason = case Err of
                 {'EXIT', {Cause, _Stack}} -> Cause;
                 Other                     -> Other
             end,
    Acc#{decode    := D + 1,
         bad_feeds := maps:update_with(Dir, fun(C) -> C + 1 end, 1, B),
         reasons   := maps:update_with({decode, Reason},
                                       fun(C) -> C + 1 end, 1, R)}.

note_failure(Props, Reason, #{encode := E, reasons := R, feeds := F,
                              samples := S} = Acc) ->
    Id = gv(<<"key">>, Props, <<"?">>),
    {Author, Seq} =
        case gv(<<"value">>, Props, undefined) of
            {V} when is_list(V) -> {gv(<<"author">>, V, <<"?">>),
                                    gv(<<"sequence">>, V, 0)};
            _                   -> {<<"?">>, 0}
        end,
    Acc#{encode  := E + 1,
         reasons := maps:update_with(Reason, fun(C) -> C + 1 end, 1, R),
         feeds   := maps:update_with(Author, fun(C) -> C + 1 end, 1, F),
         samples := case length(S) < 10 of
                        true  -> S ++ [{Author, Seq, Id, Reason}];
                        false -> S
                    end}.

%% A full corpus takes minutes; say something so it does not look hung.
progress(#{total := N} = Acc) when N rem 250000 =:= 0 ->
    io:format("  ...~s messages~n", [num(N)]),
    Acc;
progress(Acc) ->
    Acc.

report_census(#{total := Total, decode := Dec, encode := Enc,
                reasons := Reasons, feeds := Feeds, bad_feeds := BadFeeds,
                samples := Samples},
              NumFeeds, Ms) ->
    io:format("~n== encoding census ==~n"),
    io:format("  scanned         ~s messages in ~s feeds (~s ms)~n",
              [num(Total), num(NumFeeds), num(Ms)]),
    io:format("  decode failures ~s~s~n", [num(Dec), pct(Dec, Total)]),
    io:format("  encode failures ~s~s~n", [num(Enc), pct(Enc, Total)]),
    case Reasons =:= #{} of
        true  -> ok;
        false ->
            io:format("~n  by reason~n"),
            [io:format("    ~-30s ~s~n", [io_lib:format("~p", [R]), num(C)])
             || {R, C} <- sort_desc(Reasons)]
    end,
    case BadFeeds =:= #{} of
        true  -> ok;
        false ->
            %% by directory, not by author: an undecodable frame is one we
            %% could not read an author out of
            io:format("~n  undecodable frames by feed directory~n"),
            [io:format("    ~s  ~s~n", [D, num(C)]) || {D, C} <- sort_desc(BadFeeds)]
    end,
    case Feeds =:= #{} of
        true  -> ok;
        false ->
            io:format("~n  unencodable messages by feed~n"),
            [io:format("    ~s  ~s~n", [A, num(C)]) || {A, C} <- sort_desc(Feeds)],
            io:format("~n  first failures~n"),
            [io:format("    ~s seq ~s  ~s~n", [A, num(Sq), Id])
             || {A, Sq, Id, _R} <- Samples]
    end,
    io:format("~n"),
    case {Dec, Enc} of
        {0, 0} ->
            io:format("OK — every stored message decodes and encodes~n"),
            erlang:halt(0);
        {_, 0} ->
            %% Not the encoder: a frame that will not decode was never
            %% readable, so this is damage on disk, not a serving bug.
            io:format("WARNING: ~s frame(s) could not be decoded — this is "
                      "damaged storage,~n         not an encoding problem~n",
                      [num(Dec)]),
            erlang:halt(2);
        _ ->
            io:format("WARNING: ~s message(s) cannot be served to a client~n",
                      [num(Enc)]),
            erlang:halt(2)
    end.

sort_desc(Map) ->
    lists:sort(fun({_, A}, {_, B}) -> A >= B end, maps:to_list(Map)).

pct(_N, 0)     -> "";
pct(N, Total)  -> io_lib:format("  (~.4f%)", [N * 100 / Total]).

arg_int(Args, Flag, Default) ->
    case lists:dropwhile(fun(A) -> A =/= Flag end, Args) of
        [_, V | _] -> try list_to_integer(V) catch _:_ -> Default end;
        _          -> Default
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

%% Peer messages whose signature did not verify, since boot, plus which
%% mode the node is in.  A count above zero during the measure phase is
%% the whole point of the exercise, so it is stated plainly rather than
%% only when something is wrong.
sig_line(Props) ->
    N    = gv(<<"invalidSignatures">>, Props, 0),
    Mode = case gv(<<"requireValidSigs">>, Props, false) of
               true -> "rejecting";
               _    -> "warn only, still stored"
           end,
    %% Plain ASCII only: this escript formats with ~s throughout, and a
    %% non-ASCII character in the format string (an em dash, say) is a
    %% codepoint above 255 that ~s refuses.
    case N of
        0 -> io_lib:format("none (~s)", [Mode]);
        _ -> io_lib:format("~s SEEN (~s); see the per-feed lines in the log",
                           [num(N), Mode])
    end.

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

%% Print each message as it arrives.  This used to use rpc_stream_call/3,
%% which accumulates the whole history and returns it as one list — so
%% nothing reached stdout until the last message had been fetched, and with
%% `--limit` defaulting to -1 (unlimited) that is the entire feed.  At a
%% terminal it looked slow; through a pipe it looked hung, because Erlang
%% block-buffers stdout when it is not a tty, so the buffer did not even
%% flush until the end.  rpc_stream_call also imposes a single 30s call
%% timeout on the whole fetch, which a long feed will exceed.
%%
%% open_source/4 delivers frames as they arrive instead, so output starts
%% immediately, memory stays flat, and the timeout below is per-frame
%% (idle) rather than for the entire history.
cmd_hist(Peer, Id, Limit) ->
    Args = [{[{<<"id">>, list_to_binary(Id)},
              {<<"limit">>, Limit},
              {<<"keys">>, true}]}],
    case ssb_peer:open_source(Peer, [<<"createHistoryStream">>], Args, self()) of
        {ok, Ref} ->
            stream_hist(Ref);
        Err ->
            io:format("Error: ~p~n", [Err])
    end.

stream_hist(Ref) ->
    receive
        {stream_data, Ref, Body} ->
            io:format("~s~n", [Body]),
            stream_hist(Ref);
        {stream_done, Ref} ->
            ok
    after ?STREAM_IDLE_MS ->
        %% stderr, so a stalled stream does not inject a non-JSON line into
        %% a pipeline that is feeding jq
        io:format(standard_error,
                  "sbutt: no data for ~ps, giving up~n", [?STREAM_IDLE_MS div 1000]),
        ok
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
    io:format("  health                        Node/view/store health report~n"),
    io:format("  census encoding [--limit N]   Count stored messages that cannot~n"),
    io:format("                                be encoded for a client (reads the~n"),
    io:format("                                logs directly; no node needed)~n").
