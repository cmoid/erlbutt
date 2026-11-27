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
     {[~"admin", ~"peers", ~"connected"], async, owner},
     {[~"admin", ~"store", ~"tables"],    async, owner},
     {[~"admin", ~"export"],              async, owner},
     {[~"admin", ~"import"],              async, owner},
     {[~"admin", ~"gc", ~"plan"],         async, owner},
     {[~"admin", ~"gc", ~"blobs", ~"plan"], async, owner},
     {[~"admin", ~"gc", ~"blobs", ~"run"], async, owner},
     {[~"admin", ~"gc", ~"blobs", ~"restore"], async, owner}].

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
              %% peer messages whose signature did not verify since boot;
              %% non-zero means either a hostile peer or a canonical-encoding
              %% disagreement, and either way wants looking at before
              %% require_valid_sigs is turned on
              {~"invalidSignatures", ssb_feed_sup:invalid_signature_count()},
              {~"requireValidSigs", config:require_valid_sigs()},
              %% peer messages that broke a structural rule (msg_format)
              {~"invalidFormat",    ssb_feed_sup:invalid_format_count()},
              {~"requireValidFormat", config:require_valid_format()},
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
    {reply, [{[{~"id", peer_id(PubKey)}]} || {PubKey, _Pid} <- Rows,
                                             is_binary(PubKey)]};

%%%===================================================================
%%% export
%%%===================================================================

%% Write a portable bundle of feeds and their blobs (feed_export) into a
%% directory ON THE NODE'S DISK — the caller fetches it from there.
%% Args: [Dir] for our own feed, or [Dir, [FeedId, ...]].  Answers with
%% the bundle's manifest.
%%
%% This blocks the calling connection for the length of the export.  It
%% is a few seconds for one feed of ordinary size; for a big multi-feed
%% bundle use `sbutt export`, which reads the store without going
%% through the node at all.
handle_rpc([~"admin", ~"export"], [Dir], Caller) ->
    handle_rpc([~"admin", ~"export"], [Dir, own], Caller);
handle_rpc([~"admin", ~"export"], [Dir, Feeds], _Caller)
  when is_binary(Dir), Dir =/= ~"", (Feeds =:= own orelse is_list(Feeds)) ->
    Opts = case Feeds of
               own -> #{};
               _   -> #{feeds => Feeds}
           end,
    case filename:pathtype(Dir) of
        absolute ->
            case feed_export:export(binary_to_list(Dir), Opts) of
                {ok, Manifest} -> {reply, {json, feed_export:manifest_json(Manifest)}};
                {error, Why}   -> {error, feed_export:describe_error(Why)}
            end;
        _ ->
            %% relative to what?  The node's cwd is the release dir.
            {error, ~"export needs an absolute directory"}
    end;
handle_rpc([~"admin", ~"export"], _Args, _Caller) ->
    {error, ~"export takes a directory, and optionally a list of feed ids"};

%% Read a bundle written by admin.export / sbutt export (feed_import):
%% verify every message and blob, store what is new.  Dir is on the
%% node's disk.  Answers with a per-feed report — a refused or stopped
%% feed is a normal answer, not an error; only an unreadable bundle is.
%%
%% Unlike export this cannot run outside the node: new messages go in
%% through the feed processes, like replicated ones.  It blocks the
%% calling connection for its length.
handle_rpc([~"admin", ~"import"], [Dir], _Caller)
  when is_binary(Dir), Dir =/= ~"" ->
    case filename:pathtype(Dir) of
        absolute ->
            case feed_import:import(binary_to_list(Dir)) of
                {ok, Report} -> {reply, {json, feed_import:report_json(Report)}};
                {error, Why} -> {error, iolist_to_binary(
                                          io_lib:format("import failed: ~p",
                                                        [Why]))}
            end;
        _ ->
            {error, ~"import needs an absolute directory"}
    end;
handle_rpc([~"admin", ~"import"], _Args, _Caller) ->
    {error, ~"import takes a bundle directory"};

%%%===================================================================
%%% gc
%%%===================================================================

%% Which stored feeds have fallen out of the replication set, and what
%% removing them would touch (feed_gc).  Removes nothing; it updates only
%% the candidates ledger, unless {"record": false}.  It walks every feed
%% directory and the whole ingest journal, so it blocks the calling
%% connection for seconds to a minute on a large store.
handle_rpc([~"admin", ~"gc", ~"plan"], Args, _Caller) ->
    case gc_opts(Args) of
        {ok, Opts} ->
            case feed_gc:plan(Opts) of
                {ok, Report} -> {reply, feed_gc:report_json(Report)};
                {error, Why} -> {error, iolist_to_binary(
                                          io_lib:format("gc plan refused: ~p",
                                                        [Why]))}
            end;
        error ->
            {error, <<"gc.plan takes an optional object: "
                      "{graceDays: N, top: N | \"all\", record: bool}">>}
    end;

%% Which stored blobs nothing references any more (blob_gc).  Removes
%% nothing and writes nothing.  Scans every stored message, so it blocks
%% the calling connection for a minute or more on a large store.
handle_rpc([~"admin", ~"gc", ~"blobs", ~"plan"], Args, _Caller) ->
    case gc_opts(Args) of
        {ok, Opts} when not is_map_key(record, Opts) ->
            case blob_gc:plan(Opts) of
                {ok, Report} -> {reply, blob_gc:report_json(Report)};
                {error, Why} -> {error, iolist_to_binary(
                                          io_lib:format("gc blobs plan refused: ~p",
                                                        [Why]))}
            end;
        _ ->
            {error, <<"gc.blobs.plan takes an optional object: "
                      "{graceDays: N, top: N | \"all\"}">>}
    end;

%% Move the blobs nothing references into a quarantine (blob_gc:run/1),
%% on the live node.  {"dryRun": true} only says what it would move.
%% Marks first, exactly as the plan does, so it takes as long.
handle_rpc([~"admin", ~"gc", ~"blobs", ~"run"], Args, _Caller) ->
    case gc_opts(Args) of
        {ok, Opts} when not is_map_key(record, Opts), not is_map_key(top, Opts) ->
            case blob_gc:run(Opts) of
                {ok, R} ->
                    {reply, {[{~"done",       maps:get(done, R)},
                              {~"graceDays",  maps:get(grace_days, R)},
                              {~"candidates", maps:get(candidates, R)},
                              {~"bytes",      maps:get(bytes, R)},
                              {~"stamp",      maps:get(stamp, R, null)},
                              {~"quarantine", maps:get(quarantine, R, null)},
                              {~"moved",      maps:get(moved, R, 0)},
                              {~"movedBytes", maps:get(moved_bytes, R, 0)},
                              {~"putBack",    maps:get(put_back, R, 0)},
                              {~"failed",     [{[{~"id", Id},
                                                 {~"error", iolist_to_binary(
                                                              io_lib:format("~p", [W]))}]}
                                               || {Id, W} <- maps:get(failed, R, [])]}]}};
                {error, Why} ->
                    {error, iolist_to_binary(io_lib:format("gc blobs run refused: ~p",
                                                           [Why]))}
            end;
        _ ->
            {error, <<"gc.blobs.run takes an optional object: "
                      "{graceDays: N, dryRun: bool}">>}
    end;

%% Put a blob quarantine back (blob_gc:restore/1), on the live node.
handle_rpc([~"admin", ~"gc", ~"blobs", ~"restore"], [Stamp], _Caller)
  when is_binary(Stamp) ->
    case blob_gc:restore(Stamp) of
        {ok, #{restored := N, already_present := D}} ->
            {reply, {[{~"stamp", Stamp}, {~"restored", N}, {~"alreadyPresent", D}]}};
        {error, Why} ->
            {error, iolist_to_binary(io_lib:format("gc blobs restore refused: ~p",
                                                   [Why]))}
    end;
handle_rpc([~"admin", ~"gc", ~"blobs", ~"restore"], _Args, _Caller) ->
    {error, ~"gc.blobs.restore takes a quarantine stamp"};

%%%===================================================================
%%% store
%%%===================================================================

%% Every table in the derived store with its row count — the "is anything
%% actually being indexed" question, which has no other owner-reachable
%% answer.  Deliberately generic: it reads sqlite_master rather than
%% naming views, so a view added later shows up without touching admin.
%%
%% count(*) is a scan, so this is an on-demand command, not something to
%% poll.
handle_rpc([~"admin", ~"store", ~"tables"], _Args, _Caller) ->
    case ssb_store:available() of
        false ->
            {reply, []};
        true ->
            Names = [N || [N] <- ssb_store:q(
                                   "SELECT name FROM sqlite_master"
                                   " WHERE type='table'"
                                   "   AND name NOT LIKE 'sqlite_%'"
                                   " ORDER BY name")],
            {reply, [{[{~"table", N}, {~"rows", table_rows(N)}]} || N <- Names]}
    end.

%% A table name cannot be a bound parameter, so it is interpolated — the
%% names come from sqlite_master (our own schema), and double quotes make
%% it an identifier rather than a literal, which matters because esqlite
%% is built with SQLITE_DQS=0.
table_rows(Name) ->
    try ssb_store:q(["SELECT count(*) FROM \"", Name, "\""]) of
        [[N]] when is_integer(N) -> N;
        _                        -> -1
    catch _:_ -> -1
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

gc_opts([]) -> {ok, #{}};
gc_opts([{Props}]) when is_list(Props) ->
    lists:foldl(
      fun(_, error) -> error;
         ({~"graceDays", N}, {ok, O}) when is_integer(N), N >= 0 ->
              {ok, O#{grace_days => N}};
         ({~"top", ~"all"}, {ok, O}) -> {ok, O#{top => all}};
         ({~"top", N}, {ok, O}) when is_integer(N), N >= 0 ->
              {ok, O#{top => N}};
         ({~"record", B}, {ok, O}) when is_boolean(B) ->
              {ok, O#{record => B}};
         ({~"dryRun", B}, {ok, O}) when is_boolean(B) ->
              {ok, O#{dry_run => B}};
         (_, _) -> error
      end, {ok, #{}}, Props);
gc_opts(_) -> error.

%% peer_registry keys connections by the RAW 32-byte public key — that is
%% what the handshake yields and what ssb_peer registers — not the
%% @base64.ed25519 display form.  Raw key bytes are arbitrary binary, so
%% emitting them as a JSON string fails on the first one that is not valid
%% UTF-8, which for random 32-byte keys is most of them.
%%
%% That is not a cosmetic bug: before rpc_processor guarded its encoding,
%% it killed the whole connection, and the client reconnected and asked
%% again.
peer_id(PubKey) when byte_size(PubKey) =:= 32 ->
    utils:display_pub(base64:encode(PubKey));
peer_id(Other) ->
    %% already in display form (or something unexpected) — pass it through
    %% only if it is safe to encode
    case is_printable(Other) of
        true  -> Other;
        false -> utils:display_pub(base64:encode(Other))
    end.

is_printable(Bin) ->
    try
        _ = json:encode(Bin),
        true
    catch _:_ -> false
    end.

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
%% Values nest — every entry carries a `duration` map — and the reply
%% encoder takes EJSON objects only, so a nested map must be converted
%% too or the whole reply fails to encode.
meta_props(Meta) when is_map(Meta) ->
    [{K, ejson(V)} || K := V <- Meta, is_binary(K)];
meta_props(_) ->
    [].

ejson(M) when is_map(M)  -> {meta_props(M)};
ejson(L) when is_list(L) -> [ejson(V) || V <- L];
ejson(V)                 -> V.

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
%% peer_registry holds RAW 32-byte keys.  Rendering one straight into
%% JSON is what killed connections before rpc_processor guarded encoding,
%% so this pins the conversion rather than the symptom.
peer_id_renders_raw_keys_test() ->
    %% a key containing 0xFC — not valid UTF-8, and perfectly ordinary
    Raw = <<252, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
            16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31>>,
    ?assertEqual(32, byte_size(Raw)),
    ?assertError(_, json:encode(Raw)),          %% the original problem
    Id = peer_id(Raw),
    ?assertMatch(<<"@", _/binary>>, Id),
    ?assertMatch(<<_:1/binary, _/binary>>, Id),
    %% and the rendered form encodes cleanly
    ?assertMatch(<<_/binary>>, iolist_to_binary(json:encode(Id))),
    %% a display-form id is passed through untouched
    Display = ~"@abcdefghijklmnopqrstuvwxyz0123456789ABCDEFX=.ed25519",
    ?assertEqual(Display, peer_id(Display)).

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

%% Bad arguments answer an error frame rather than crashing into the
%% plugin's catch-all "internal error".
blobs_restore_rejects_bad_args_test() ->
    [?assertMatch({error, _},
                  handle_rpc([~"admin", ~"gc", ~"blobs", ~"restore"], A, #{}))
     || A <- [[], [42], [~"a", ~"b"]]],
    ?assertMatch({error, _},
                 handle_rpc([~"admin", ~"gc", ~"blobs", ~"restore"],
                            [~"not-a-stamp"], #{})).

import_rejects_bad_args_test() ->
    [?assertMatch({error, _}, handle_rpc([~"admin", ~"import"], Args, #{}))
     || Args <- [[], [~""], [42], [~"relative/dir"], [~"/abs", extra]]].

gc_opts_test() ->
    ?assertEqual({ok, #{}}, gc_opts([])),
    ?assertEqual({ok, #{grace_days => 0, top => all, record => false,
                        dry_run => true}},
                 gc_opts([{[{~"graceDays", 0}, {~"top", ~"all"},
                            {~"record", false}, {~"dryRun", true}]}])),
    [?assertEqual(error, gc_opts(A))
     || A <- [[{[{~"graceDays", -1}]}], [{[{~"top", ~"some"}]}],
              [{[{~"record", 1}]}], [{[{~"bogus", 1}]}], [1], [{[]}, extra]]].

export_rejects_bad_args_test() ->
    [?assertMatch({error, _}, handle_rpc([~"admin", ~"export"], Args, #{}))
     || Args <- [[], [~""], [42], [~"relative/dir"], [~"/abs", ~"notalist"],
                 [~"/abs", [], extra]]].

%% A real conn_db entry nests maps (`duration`); the reply must still
%% encode.  It failed with function_clause in json:do_encode_map before.
peers_known_reply_encodes_test() ->
    Meta = #{~"host" => ~"ssb.example.com", ~"port" => 8008,
             ~"duration" => #{~"mean" => 0, ~"stdev" => 0},
             ~"pool" => [#{~"a" => 1}, 2]},
    Props = meta_props(Meta),
    ?assertEqual({[{~"mean", 0}, {~"stdev", 0}]},
                 proplists:get_value(~"duration", Props)),
    Bin = iolist_to_binary(
            message:ssb_encoder([{[{~"address", ~"net:x"} | Props]}],
                                fun message:ssb_encoder/3, [])),
    [Row] = json:decode(Bin),
    ?assertEqual(#{~"mean" => 0, ~"stdev" => 0}, maps:get(~"duration", Row)),
    ?assertEqual([#{~"a" => 1}, 2], maps:get(~"pool", Row)).

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
