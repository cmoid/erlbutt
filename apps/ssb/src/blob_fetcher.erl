%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Want manager for blob replication.
%%
%% When messages are stored, social_msg:dispatch/1 scans their content for
%% blob references (&<base64>.sha256) and calls want/1 for each.  Blobs we
%% do not hold locally are recorded as wants and advertised to every
%% connected peer over the blobs.createWants protocol.  When a peer answers
%% with a have ({have, BlobId, Size, PeerPid}, forwarded by blob_wants),
%% the blob is fetched via ssb_peer:fetch_blob/2, hash-verified, and stored.
%%
%% Outstanding wants are re-sent to each newly established connection
%% (ssb_peer calls peer_connected/1) and re-broadcast periodically so that
%% blobs missing from the current peer set are eventually found.
%%
%% Wants are persisted in ssb_store, so a restart does not forget what the
%% node was looking for.  They used to be memory-only, rebuilt either from
%% newly arriving messages or from the optional startup scan — and that
%% scan is a fold over every feed, decoding every message and attempting a
%% decrypt on every encrypted one, which is the same order of work as
%% rebuilding a view.  Paying that on every boot to recover a set of
%% hashes was the wrong trade; the set is small and belongs on disk.
%%
%% The scan is now a recovery tool rather than the only way back: it finds
%% refs from before wants were persisted, or after the table is lost.
%%
%% A want is durable from the moment it is recorded and removed when the
%% blob is stored.  Failure leaves it in place — a fetch that failed
%% should be retried against the next peer, which is what the rebroadcast
%% is for.
%%
%% Note: references are only extracted from JSON structure (mention links,
%% image fields, etc.), not from inside free-form markdown text — same as
%% the JS implementations, which rely on the mentions array.
-module(blob_fetcher).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

%% API
-export([start_link/0,
         want/1,
         want_refs/1,
         extract_blob_refs/1,
         peer_connected/1,
         wanted/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
%% ssb-blobs default maximum blob size (5 MiB); larger haves are ignored.
-define(MAX_BLOB_SIZE, 5242880).
%% Re-advertise outstanding wants to all connected peers this often.
-define(REBROADCAST_MS, 300000).
%% Delay before the optional startup scan of existing messages, giving the rest
%% of the node (feeds, keys, peers) time to settle first.
-define(SCAN_DELAY_MS, 30000).

-define(SCHEMA_VERSION, 1).
%% `added` is not read yet.  It is here because the obvious follow-up is
%% expiring wants nothing has answered in a long time (a blob referenced
%% by a message whose author is gone is wanted forever otherwise), and
%% that needs an age — which cannot be reconstructed after the fact.
-define(DDL,
        ["CREATE TABLE IF NOT EXISTS blob_want("
         "  id    TEXT PRIMARY KEY,"
         "  added INTEGER NOT NULL) WITHOUT ROWID;"]).

-record(state, {
          %% #{BlobId => true} — blobs we are looking for
          wants = #{},
          %% #{BlobId => FetcherPid} — fetches currently in progress
          in_flight = #{}
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Note a blob reference; it will be fetched from peers if not held locally.
%% Cast to a registered name is a no-op when the server is not running, so
%% this is safe to call from contexts (tests) without the full app.
want(BlobId) ->
    gen_server:cast(?SERVER, {want, BlobId}).

%% Walk decoded message content and want every blob reference found.
want_refs(Content) ->
    lists:foreach(fun want/1, extract_blob_refs(Content)).

%% Called by ssb_peer once a connection is up: advertise current wants.
peer_connected(PeerPid) ->
    gen_server:cast(?SERVER, {peer_connected, PeerPid}).

%% List of currently wanted blob ids.
wanted() ->
    gen_server:call(?SERVER, wanted).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    erlang:send_after(?REBROADCAST_MS, self(), rebroadcast),
    maybe_schedule_scan(),
    {ok, #state{wants = load_wants()}}.

%% Wants recorded by a previous run, minus any the node has since obtained
%% by another route (blobs.add, or a fetch whose removal did not land).
%% Those are cleared here rather than left to be re-advertised forever.
%%
%% A store that is unavailable costs the want set, not the process: this
%% is exactly the state the scan exists to rebuild.
load_wants() ->
    case catch ssb_store:declare(?MODULE, ?SCHEMA_VERSION, ?DDL) of
        ok ->
            Ids = [Id || [Id] <- rows("SELECT id FROM blob_want", [])],
            {Held, Want} = lists:partition(fun has_local/1, Ids),
            forget_wants(Held),
            maps:from_list([{Id, true} || Id <- Want]);
        Err ->
            ?SSB_ERROR("blob_fetcher: could not declare its schema (~p) — "
                       "wants start empty and are rediscovered from arriving "
                       "messages or a startup scan", [Err]),
            #{}
    end.

%% Schedule a one-shot scan of existing on-disk messages when enabled in config.
maybe_schedule_scan() ->
    case (catch config:blob_scan_enabled()) of
        true -> erlang:send_after(?SCAN_DELAY_MS, self(), scan);
        _    -> ok
    end.

handle_call(wanted, _From, #state{wants = Wants} = State) ->
    {reply, maps:keys(Wants), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({want, BlobId}, #state{wants = Wants} = State) ->
    case maps:is_key(BlobId, Wants) orelse has_local(BlobId) of
        true ->
            {noreply, State};
        false ->
            remember_wants([BlobId]),
            broadcast_wants([BlobId], connected_peers()),
            {noreply, State#state{wants = Wants#{BlobId => true}}}
    end;

handle_cast({peer_connected, PeerPid}, #state{wants = Wants} = State) ->
    case maps:keys(Wants) of
        []  -> ok;
        Ids -> send_wants(PeerPid, Ids)
    end,
    {noreply, State};

%% Results of the startup scan: want every referenced blob we don't already
%% hold, broadcasting the whole new batch once rather than per blob.
handle_cast({scan_results, Refs}, #state{wants = Wants} = State) ->
    New = [R || R <- Refs,
                not maps:is_key(R, Wants),
                not has_local(R)],
    NewWants = lists:foldl(fun(R, W) -> W#{R => true} end, Wants, New),
    case New of
        [] -> ok;
        _  -> remember_wants(New),
              broadcast_wants(New, connected_peers())
    end,
    ?SSB_INFO("blob_fetcher: startup scan adds ~p new want(s) from ~p ref(s)~n",
              [length(New), length(Refs)]),
    {noreply, State#state{wants = NewWants}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% A peer holds a wanted blob — fetch it unless a fetch is already running.
handle_info({have, BlobId, Size, PeerPid},
            #state{wants = Wants, in_flight = InFlight} = State) ->
    Wanted   = maps:is_key(BlobId, Wants),
    Fetching = maps:is_key(BlobId, InFlight),
    case Wanted andalso (not Fetching) andalso Size =< ?MAX_BLOB_SIZE of
        true ->
            Self = self(),
            {Pid, _Ref} = spawn_monitor(fun() -> do_fetch(Self, BlobId, PeerPid) end),
            {noreply, State#state{in_flight = InFlight#{BlobId => Pid}}};
        false ->
            {noreply, State}
    end;

handle_info({fetched, BlobId, ok},
            #state{wants = Wants, in_flight = InFlight} = State) ->
    ?SSB_INFO("blob_fetcher: stored ~p~n", [BlobId]),
    forget_wants([BlobId]),
    {noreply, State#state{wants     = maps:remove(BlobId, Wants),
                          in_flight = maps:remove(BlobId, InFlight)}};

%% Failed fetch: drop the in-flight entry but keep the want, so the next
%% have (new peer or rebroadcast) retries.
handle_info({fetched, BlobId, Error}, #state{in_flight = InFlight} = State) ->
    ?SSB_INFO("blob_fetcher: fetch of ~p failed: ~p~n", [BlobId, Error]),
    {noreply, State#state{in_flight = maps:remove(BlobId, InFlight)}};

handle_info({'DOWN', _Ref, process, Pid, _Reason},
            #state{in_flight = InFlight} = State) ->
    Cleaned = maps:filter(fun(_, P) -> P =/= Pid end, InFlight),
    {noreply, State#state{in_flight = Cleaned}};

%% One-shot startup scan: fold the whole on-disk log off the gen_server so a
%% large backlog doesn't block it; the worker casts {scan_results, Refs} back.
handle_info(scan, State) ->
    spawn(fun scan_log/0),
    {noreply, State};

%% Drop anything we have since obtained before re-advertising.  Wants used
%% to be cleared by a restart, which quietly covered a blob arriving by
%% another route (blobs.add, or the converter); now that they are durable,
%% such a want would be re-advertised forever.  One stat per want every
%% five minutes is a cheap place to notice.
handle_info(rebroadcast, #state{wants = Wants} = State0) ->
    {Held, Want} = lists:partition(fun has_local/1, maps:keys(Wants)),
    forget_wants(Held),
    State = State0#state{wants = maps:without(Held, Wants)},
    case Want of
        []  -> ok;
        Ids -> broadcast_wants(Ids, connected_peers())
    end,
    erlang:send_after(?REBROADCAST_MS, self(), rebroadcast),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

has_local(BlobId) ->
    try blobs:has(BlobId)
    catch _:_ -> false
    end.

connected_peers() ->
    try [Pid || {_PubKey, Pid} <- peer_registry:all(), is_process_alive(Pid)]
    catch _:_ -> []
    end.

%%%===================================================================
%%% The durable want set
%%%===================================================================
%%
%% Every one of these is guarded.  A want the store did not record is
%% recovered by the next scan, and one it failed to delete is dropped by
%% the next rebroadcast — whereas taking this process down loses the
%% in-memory set as well, and with it any fetch in flight.

remember_wants([]) ->
    ok;
remember_wants(Ids) ->
    Now = erlang:system_time(millisecond),
    _ = catch ssb_store:insert_many(
                "INSERT INTO blob_want(id, added) VALUES(?1, ?2)"
                " ON CONFLICT(id) DO NOTHING",
                [[Id, Now] || Id <- Ids]),
    ok.

forget_wants([]) ->
    ok;
forget_wants(Ids) ->
    [catch ssb_store:write("DELETE FROM blob_want WHERE id=?1", [Id])
     || Id <- Ids],
    ok.

rows(Sql, Params) ->
    try ssb_store:q(Sql, Params) of
        L when is_list(L) -> L;
        _                 -> []
    catch _:_ -> []
    end.

broadcast_wants(Ids, Peers) ->
    lists:foreach(fun(Peer) -> send_wants(Peer, Ids) end, Peers).

%% The {?SERVER, PeerPid} sink makes blob_wants tag forwarded haves with
%% the connection they arrived on, so we know whom to fetch from.
send_wants(PeerPid, Ids) ->
    ssb_peer:request_blob_wants(PeerPid, Ids, {?SERVER, PeerPid}).

%% Fold the per-feed store (archives included), collecting distinct blob
%% refs from every message (decrypting private messages addressed to
%% us), then hand them to the gen_server.  Runs in its own process —
%% see handle_info(scan, ...).
scan_log() ->
    RefSet = feed_store:fold_all(
        fun(Data, Acc) ->
            try
                Msg = message:decode(Data, false),
                lists:foldl(fun(R, A) -> A#{R => true} end, Acc, msg_blob_refs(Msg))
            catch _:_ -> Acc
            end
        end, #{}),
    gen_server:cast(?SERVER, {scan_results, maps:keys(RefSet)}).

%% Blob refs reachable from a stored message: directly from public content, or
%% from the decrypted body of a private message addressed to us.
msg_blob_refs(#message{content = {Props}}) ->
    extract_blob_refs({Props});
msg_blob_refs(#message{content = Content}) when is_binary(Content) ->
    case private_box:decrypt(Content) of
        {ok, Plain} ->
            try extract_blob_refs(utils:nat_decode(Plain))
            catch _:_ -> []
            end;
        _ ->
            []
    end;
msg_blob_refs(_) ->
    [].

do_fetch(Parent, BlobId, PeerPid) ->
    Result = try ssb_peer:fetch_blob(PeerPid, BlobId) of
                 {ok, Data} -> blobs:store_verified(BlobId, Data);
                 Other      -> {error, Other}
             catch _:Reason -> {error, Reason}
             end,
    Parent ! {fetched, BlobId, Result}.

%% Collect every well-formed blob reference appearing anywhere in decoded
%% JSON message content.  Also used by converter when importing a JS log.
extract_blob_refs(Content) ->
    lists:usort(walk(Content, [])).

walk({Props}, Acc) when is_list(Props) ->
    lists:foldl(fun({_K, V}, A) -> walk(V, A) end, Acc, Props);
walk(List, Acc) when is_list(List) ->
    lists:foldl(fun walk/2, Acc, List);
walk(Bin, Acc) when is_binary(Bin) ->
    case is_blob_id(Bin) of
        true  -> [Bin | Acc];
        false -> Acc
    end;
walk(_, Acc) ->
    Acc.

%% "&" ++ 44 base64 chars (32 bytes) ++ ".sha256"
is_blob_id(<<"&", B64:44/binary, ".sha256">>) ->
    try byte_size(base64:decode(B64)) =:= 32
    catch _:_ -> false
    end;
is_blob_id(_) ->
    false.

-ifdef(TEST).

make_blob_id(Data) ->
    <<"&", (base64:encode(crypto:hash(sha256, Data)))/binary, ".sha256">>.

%% The test blob store (_build/test/blobs/) persists across runs, so blobs
%% stored by a previous run would satisfy has/1 and break want tracking.
%% Random payloads keep each run independent (unique_integer is not enough —
%% it restarts per VM run and can repeat values across runs).
unique_payload(Tag) ->
    <<Tag/binary, " ", (binary:encode_hex(crypto:strong_rand_bytes(8)))/binary>>.

wait_until(_F, 0) -> false;
wait_until(F, N) ->
    case F() of
        true  -> true;
        false -> timer:sleep(50), wait_until(F, N - 1)
    end.

is_blob_id_test() ->
    ?assert(is_blob_id(make_blob_id(~"some data"))),
    ?assertNot(is_blob_id(~"&short.sha256")),
    ?assertNot(is_blob_id(~"@author=.ed25519")),
    ?assertNot(is_blob_id(~"%msgid=.sha256")),
    %% right shape, invalid base64 payload
    Bad = <<"&", (binary:copy(~"*", 44))/binary, ".sha256">>,
    ?assertNot(is_blob_id(Bad)).

extract_blob_refs_test() ->
    Blob1 = make_blob_id(~"payload one"),
    Blob2 = make_blob_id(~"payload two"),
    Content = {[{~"type", ~"post"},
                 {~"text", ~"an image for you"},
                 {~"mentions", [{[{~"link", Blob1}, {~"name", ~"a.png"}]},
                                {[{~"link", ~"@feedid=.ed25519"}]}]},
                 {~"image", {[{~"link", Blob2}, {~"size", 1234}]}}]},
    ?assertEqual(lists:usort([Blob1, Blob2]), extract_blob_refs(Content)).

extract_blob_refs_dedup_test() ->
    Blob = make_blob_id(~"same payload"),
    Content = {[{~"image", Blob},
                 {~"mentions", [{[{~"link", Blob}]}]}]},
    ?assertEqual([Blob], extract_blob_refs(Content)),
    ?assertEqual([], extract_blob_refs({[{~"type", ~"post"}, {~"text", ~"plain"}]})),
    ?assertEqual([], extract_blob_refs(~"encrypted.box")).

%% want/1 tracks unknown blobs, ignores held ones, and advertises wants to
%% peers handed to peer_connected/1.
want_tracking_test() ->
    ConfigStarted = case whereis(config) of
        undefined -> {ok, _} = config:start_link("test/ssb.cfg"), true;
        _         -> false
    end,
    {ok, BlobsPid} = blobs:start_link(),
    {ok, Fetcher} = blob_fetcher:start_link(),

    %% a blob we hold locally is never wanted
    Held = blobs:store(unique_payload(~"already stored payload")),
    blob_fetcher:want(Held),
    ?assertEqual([], blob_fetcher:wanted()),

    %% an unknown blob is recorded once
    Missing = make_blob_id(unique_payload(~"missing payload")),
    blob_fetcher:want(Missing),
    blob_fetcher:want(Missing),
    ?assertEqual([Missing], blob_fetcher:wanted()),

    %% a new connection receives the outstanding wants
    Self = self(),
    blob_fetcher:peer_connected(Self),
    receive
        {'$gen_cast', {request_blob_wants, Ids, {blob_fetcher, Self}}} ->
            ?assertEqual([Missing], Ids)
    after 1000 ->
        ?assert(false)
    end,

    gen_server:stop(Fetcher),
    gen_server:stop(BlobsPid),
    case ConfigStarted of
        true  -> gen_server:stop(config);
        false -> ok
    end.

%% A have for a wanted blob triggers a fetch; the blob is verified, stored,
%% and the want cleared.  A corrupt response keeps the want for retry.
have_fetch_test() ->
    ConfigStarted = case whereis(config) of
        undefined -> {ok, _} = config:start_link("test/ssb.cfg"), true;
        _         -> false
    end,
    {ok, BlobsPid} = blobs:start_link(),
    {ok, Fetcher} = blob_fetcher:start_link(),

    BlobData = unique_payload(~"blob fetcher end to end payload"),
    BlobId = make_blob_id(BlobData),
    blob_fetcher:want(BlobId),
    ?assertEqual([BlobId], blob_fetcher:wanted()),

    %% fake peer answers fetch_blob with the right data
    GoodPeer = spawn(fun() ->
        receive {'$gen_call', From, {fetch_blob, _}} ->
            gen_server:reply(From, {ok, BlobData})
        end
    end),
    Fetcher ! {have, BlobId, byte_size(BlobData), GoodPeer},
    ?assert(wait_until(fun() -> blob_fetcher:wanted() =:= [] end, 40)),
    ?assert(blobs:has(BlobId)),

    %% fake peer answers with data that does not hash to the wanted id
    OtherId = make_blob_id(unique_payload(~"other payload")),
    blob_fetcher:want(OtherId),
    BadPeer = spawn(fun() ->
        receive {'$gen_call', From, {fetch_blob, _}} ->
            gen_server:reply(From, {ok, ~"corrupt data"})
        end
    end),
    Fetcher ! {have, OtherId, 12, BadPeer},
    timer:sleep(200),
    ?assertEqual([OtherId], blob_fetcher:wanted()),
    ?assertNot(blobs:has(OtherId)),

    gen_server:stop(Fetcher),
    gen_server:stop(BlobsPid),
    case ConfigStarted of
        true  -> gen_server:stop(config);
        false -> ok
    end.

%% msg_blob_refs/1 (used by the startup scan) extracts refs from public content
%% and from private messages addressed to us, and ignores private messages for
%% someone else.
msg_blob_refs_test() ->
    ConfigStarted = case whereis(config) of
        undefined -> {ok, _} = config:start_link("test/ssb.cfg"), true;
        _         -> false
    end,
    KeysStarted = case whereis(keys) of
        undefined -> {ok, _} = keys:start_link(), true;
        _         -> false
    end,
    Me = keys:pub_key_disp(),

    %% public message: ref in mentions is found
    PubBlob = make_blob_id(unique_payload(~"pub scan")),
    PubMsg = #message{content = {[{~"type", ~"post"},
                                  {~"mentions", [{[{~"link", PubBlob}]}]}]}},
    ?assertEqual([PubBlob], msg_blob_refs(PubMsg)),

    %% private message to us: decrypts, ref found
    PrivBlob = make_blob_id(unique_payload(~"priv scan")),
    PrivBody = utils:encode_rec({[{~"type", ~"post"},
                                  {~"mentions", [{[{~"link", PrivBlob}]}]}]}),
    MineMsg = #message{content = private_box:encrypt(PrivBody, [Me])},
    ?assertEqual([PrivBlob], msg_blob_refs(MineMsg)),

    %% private message to someone else: nothing
    {OtherPub, _} = utils:create_key_pair(),
    OtherId = utils:display_pub(OtherPub),
    TheirBody = utils:encode_rec({[{~"mentions",
                                    [{[{~"link", make_blob_id(unique_payload(~"x"))}]}]}]}),
    TheirMsg = #message{content = private_box:encrypt(TheirBody, [OtherId])},
    ?assertEqual([], msg_blob_refs(TheirMsg)),

    case KeysStarted of true -> gen_server:stop(keys); false -> ok end,
    case ConfigStarted of true -> gen_server:stop(config); false -> ok end.

%%% The durable want set ----------------------------------------------
%%
%% These need ssb_store, and an ssb_home of their own so the store and the
%% blob dir do not outlive the test.  The other tests here deliberately
%% run WITHOUT a store, which exercises the degraded path: no persistence,
%% but a working fetcher.

want_store_setup() ->
    catch gen_server:stop(config),
    Home = "/tmp/bfw_" ++ integer_to_list(erlang:system_time(microsecond)),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = ssb_store:start_link(),
    {ok, _} = blobs:start_link(),
    Home.

want_store_cleanup(Home) ->
    [catch gen_server:stop(N) || N <- [?SERVER, blobs, ssb_store, config]],
    os:cmd("rm -rf " ++ Home),
    application:unset_env(ssb, ssb_home),
    ok.

durable_wants_test_() ->
    {setup, fun want_store_setup/0, fun want_store_cleanup/1,
     fun(_) ->
             [?_test(wants_survive_a_restart()),
              ?_test(an_obtained_want_is_dropped())]
     end}.

%% The reason for persisting at all: recovering the set after a restart
%% must not require the startup scan, which folds every feed and decodes
%% every message.
wants_survive_a_restart() ->
    {ok, F1} = blob_fetcher:start_link(),
    Missing = make_blob_id(unique_payload(~"persisted want")),
    blob_fetcher:want(Missing),
    ?assertEqual([Missing], blob_fetcher:wanted()),
    ?assertEqual([[1]], ssb_store:q("SELECT count(*) FROM blob_want"
                                    " WHERE id=?1", [Missing])),
    ok = gen_server:stop(F1),
    {ok, F2} = blob_fetcher:start_link(),
    ?assertEqual([Missing], blob_fetcher:wanted()),
    ok = gen_server:stop(F2).

%% Durability introduced a way to want something forever: a blob that
%% arrives by another route (blobs.add) used to be forgotten by the next
%% restart clearing the set.  The rebroadcast drops it instead.
an_obtained_want_is_dropped() ->
    {ok, F} = blob_fetcher:start_link(),
    Payload = unique_payload(~"obtained elsewhere"),
    Id = make_blob_id(Payload),
    Absent = make_blob_id(unique_payload(~"still missing")),
    blob_fetcher:want(Id),
    blob_fetcher:want(Absent),
    ?assert(lists:member(Id, blob_fetcher:wanted())),
    %% it shows up without a fetch
    Id = blobs:store(Payload),
    F ! rebroadcast,
    _ = sys:get_state(F),                    %% let the message be handled
    Wanted = blob_fetcher:wanted(),
    ?assertNot(lists:member(Id, Wanted)),
    %% and only the one we now hold — a want with no answer yet stays
    ?assert(lists:member(Absent, Wanted)),
    ?assertEqual([[0]], ssb_store:q("SELECT count(*) FROM blob_want"
                                    " WHERE id=?1", [Id])),
    ?assertEqual([[1]], ssb_store:q("SELECT count(*) FROM blob_want"
                                    " WHERE id=?1", [Absent])),
    ok = gen_server:stop(F).

-endif.
