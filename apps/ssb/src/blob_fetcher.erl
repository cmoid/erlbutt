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
%% Wants are held in memory only; they are rediscovered from replicated
%% messages after a restart.
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
    {ok, #state{}}.

handle_call(wanted, _From, #state{wants = Wants} = State) ->
    {reply, maps:keys(Wants), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({want, BlobId}, #state{wants = Wants} = State) ->
    case maps:is_key(BlobId, Wants) orelse has_local(BlobId) of
        true ->
            {noreply, State};
        false ->
            broadcast_wants([BlobId], connected_peers()),
            {noreply, State#state{wants = Wants#{BlobId => true}}}
    end;

handle_cast({peer_connected, PeerPid}, #state{wants = Wants} = State) ->
    case maps:keys(Wants) of
        []  -> ok;
        Ids -> send_wants(PeerPid, Ids)
    end,
    {noreply, State};

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

handle_info(rebroadcast, #state{wants = Wants} = State) ->
    case maps:keys(Wants) of
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

broadcast_wants(Ids, Peers) ->
    lists:foreach(fun(Peer) -> send_wants(Peer, Ids) end, Peers).

%% The {?SERVER, PeerPid} sink makes blob_wants tag forwarded haves with
%% the connection they arrived on, so we know whom to fetch from.
send_wants(PeerPid, Ids) ->
    ssb_peer:request_blob_wants(PeerPid, Ids, {?SERVER, PeerPid}).

do_fetch(Parent, BlobId, PeerPid) ->
    Result = try ssb_peer:fetch_blob(PeerPid, BlobId) of
                 {ok, Data} -> blobs:store_verified(BlobId, Data);
                 Other      -> {error, Other}
             catch _:Reason -> {error, Reason}
             end,
    Parent ! {fetched, BlobId, Result}.

%% Collect every well-formed blob reference appearing anywhere in decoded
%% JSON message content.
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
%% Unique payloads keep each run independent.
unique_payload(Tag) ->
    <<Tag/binary, " ", (integer_to_binary(erlang:unique_integer([positive])))/binary,
      " ", (integer_to_binary(os:system_time()))/binary>>.

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

-endif.
