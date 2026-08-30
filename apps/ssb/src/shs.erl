%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(shs).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-compile({no_auto_import,[size/1]}).
-import(utils, [concat/1,
                combine/2,
                size/1]).

-export([open_box/3,
         create_long_pair/0,
         client_shake_hands/2,
         client_shake_hands/3,
         client_shake_hands_tunnel/3,
         server_shake_hands/3,
         server_shake_hands/4,
         server_shake_hands_tunnel/4]).

check_hello(BinData, NetId) ->
    case size(BinData) of
        64 ->
            <<Hmac:32/binary, Eph_pk:32/binary>> = BinData,
            Valid = enacl:auth_verify(Hmac, Eph_pk, NetId),
            <<Nonce:24/binary, _End:8/binary>> = Hmac,
            {Valid, Eph_pk, Nonce};
        _ ->
            {false, nobody, none}
    end.

gen_hello(NetId) ->
    #{public := Eph_pk,
      secret := Eph_sk} = enacl:box_keypair(),
    SizEph_pk = size(Eph_pk),
    NaclAuth = enacl:auth(Eph_pk, NetId),
    SizNaclAuth = size(NaclAuth),
    <<Nonce:24/binary, _End:8/binary>> = NaclAuth,
    {Eph_sk, <<NaclAuth:SizNaclAuth/binary,
                Eph_pk:SizEph_pk/binary>>, Nonce}.

mult(Key1, Key2) ->
    enacl:curve25519_scalarmult(Key1, Key2).

sk_to_curve25519(Key) ->
    enacl:crypto_sign_ed25519_secret_to_curve25519(Key).

pk_to_curve25519(Key) ->
    enacl:crypto_sign_ed25519_public_to_curve25519(Key).

open_box(Data, Nonce, Key) ->
    Resp = enacl:secretbox_open(Data,Nonce,Key),
    case Resp of
        {error, failed_verification} ->
            %% need to handle this above somewhere, for now let it crash!
            ~"bad";
        {ok, Msg} ->
            Msg
    end.

create_box(Data, Key) ->
    enacl:secretbox(Data,?SHS_NONCE,crypto:hash(sha256, Key)).

create_long_pair() ->
    KeyPair = enacl:crypto_sign_ed25519_keypair(),
    {maps:get(public, KeyPair),
     maps:get(secret, KeyPair)}.

client_shake_hands(Socket, RemotePubKey) ->
    socket_client(Socket, RemotePubKey, config:network_id(), long_pk(), long_sk()).

%% NetId as binary: retry-loop path in ssb_peer, uses default node keys.
client_shake_hands(Socket, RemotePubKey, NetId) when is_binary(NetId) ->
    socket_client(Socket, RemotePubKey, NetId, long_pk(), long_sk());

%% Key-pair tuple: invite path, uses ephemeral invite keys with primary NetId.
client_shake_hands(Socket, RemotePubKey, {OurPubKey, OurPrivKey}) ->
    socket_client(Socket, RemotePubKey, config:network_id(), OurPubKey, OurPrivKey).

%% Run the client handshake over a gen_tcp socket.
socket_client(Socket, RemotePubKey, NetId, OurPubKey, OurPrivKey) ->
    Send = fun(D) -> gen_tcp:send(Socket, D) end,
    Recv = fun(N) -> gen_tcp:recv(Socket, N, 5000) end,
    {ok, {DecBoxKey, DecNonce, EncBoxKey, EncNonce}} =
        do_client_shake_hands(Send, Recv, RemotePubKey, NetId, OurPubKey, OurPrivKey),
    {ok, {Socket, DecBoxKey, DecNonce, EncBoxKey, EncNonce}}.

%% Run the client handshake over an arbitrary transport (e.g. a room tunnel).
%% Send(Data) writes one handshake message; Recv(N) returns {ok, Bin} with the
%% next message.  Returns the four box-stream keys/nonces (no socket).
client_shake_hands_tunnel(Send, Recv, RemotePubKey) ->
    do_client_shake_hands(Send, Recv, RemotePubKey, config:network_id(),
                          long_pk(), long_sk()).

do_client_shake_hands(Send, Recv, RemotePubKey, NetId, OurPubKey, OurPrivKey) ->
    {Eph_sk, Hmac, DecNonce} = gen_hello(NetId),
    Send(Hmac),
    ServerHmac = recv_or_fail(Recv, 64, hello_rejected),
    {ServEph_pk, EncNonce} = client_check_hello(ServerHmac, NetId),
    Shared_ab = mult(Eph_sk, ServEph_pk),
    Shared_aB = mult(Eph_sk, pk_to_curve25519(RemotePubKey)),
    ShaSab = crypto:hash(sha256, Shared_ab),
    DetSigA = enacl:sign_detached(concat([NetId, RemotePubKey, ShaSab]), OurPrivKey),
    Msg = concat([DetSigA, OurPubKey]),
    Box = create_box(Msg, concat([NetId, Shared_ab, Shared_aB])),
    Send(Box),
    Shared_Ab = mult(sk_to_curve25519(OurPrivKey), ServEph_pk),
    ServData = recv_or_fail(Recv, 80, auth_rejected),
    DetSigB = open_box(ServData, ?SHS_NONCE,
                       crypto:hash(sha256, concat([NetId, Shared_ab, Shared_aB, Shared_Ab]))),
    M = concat([NetId, DetSigA, OurPubKey, ShaSab]),
    true = enacl:sign_verify_detached(DetSigB, M, RemotePubKey),
    SharedKey = crypto:hash(sha256,
                            crypto:hash(sha256,
                                        concat([NetId, Shared_ab, Shared_aB, Shared_Ab]))),
    DecBoxKey = crypto:hash(sha256, concat([SharedKey, OurPubKey])),
    EncBoxKey = crypto:hash(sha256, concat([SharedKey, RemotePubKey])),
    {ok, {DecBoxKey, DecNonce, EncBoxKey, EncNonce}}.

%% Where the client gives up says what went wrong, and the two points mean
%% very different things:
%%
%%   hello_rejected — the peer hung up on our hello.  It never told us why,
%%     so this is the ambiguous one: a different network id, or a port that
%%     is not speaking SHS at all.  Trying our other network ids is worth
%%     something here.
%%
%%   auth_rejected  — the peer read our hello and answered it, then refused
%%     our auth box.  It ACCEPTED this network id; what it rejected is the
%%     identity we dialed.  Retrying with other network ids cannot help.
%%
%% Collapsing both into a bare badmatch is what made every outbound failure
%% read as "unknown network id" no matter the cause.
recv_or_fail(Recv, N, Tag) ->
    case Recv(N) of
        {ok, Data}      -> Data;
        {error, Reason} -> error({Tag, Reason})
    end.

%% The peer answered, but its hello does not verify under the network id we
%% sent.  A well-behaved peer echoes back the id it accepted, so unlike a
%% dropped connection this is a mismatch we have actually observed.
client_check_hello(ServerHmac, NetId) ->
    case check_hello(ServerHmac, NetId) of
        {true, Eph_pk, Nonce} -> {Eph_pk, Nonce};
        _                     -> error(network_id_mismatch)
    end.

server_shake_hands(Data, Socket, Transport) ->
    server_shake_hands(Data, Socket, Transport, config:network_ids()).

server_shake_hands(Data, Socket, Transport, NetIds) ->
    WinNetId = find_network_id(Data, NetIds),
    Send = fun(D) -> Transport:send(Socket, D) end,
    Recv = fun(N) -> Transport:recv(Socket, N, 5000) end,
    do_server_shake_hands(Data, Send, Recv, WinNetId).

%% Run the server handshake over an arbitrary transport (e.g. a room tunnel).
%% Data is the already-received client hello; Send/Recv as in the client.
server_shake_hands_tunnel(Data, Send, Recv, NetIds) ->
    WinNetId = find_network_id(Data, NetIds),
    do_server_shake_hands(Data, Send, Recv, WinNetId).

find_network_id(_Data, []) ->
    error(no_matching_network_id);
find_network_id(Data, [NetId | Rest]) ->
    case check_hello(Data, NetId) of
        {true, _, _} -> NetId;
        _            -> find_network_id(Data, Rest)
    end.

do_server_shake_hands(Data, Send, Recv, NetId) ->
    {true, ClEph_pk, EncNonce} = check_hello(Data, NetId),
    {Eph_sk, ServerHmac, DecNonce} = gen_hello(NetId),
    Send(ServerHmac),
    Shared_ab = mult(Eph_sk, ClEph_pk),
    Shared_aB = mult(sk_to_curve25519(long_sk()), ClEph_pk),
    {ok, ServData} = Recv(112),
    ShaSab = crypto:hash(sha256, Shared_ab),
    MsgPlain = open_box(ServData, ?SHS_NONCE,
                        crypto:hash(sha256, concat([NetId, Shared_ab, Shared_aB]))),
    <<DetSigA:64/binary, ClLong_pk:32/binary>> = MsgPlain,
    true = enacl:sign_verify_detached(DetSigA,
                                      concat([NetId, long_pk(), ShaSab]),
                                      ClLong_pk),
    Shared_Ab = mult(Eph_sk, pk_to_curve25519(ClLong_pk)),
    DetSigB = enacl:sign_detached(concat([NetId, DetSigA, ClLong_pk, ShaSab]), long_sk()),
    Box = create_box(DetSigB, concat([NetId, Shared_ab, Shared_aB, Shared_Ab])),
    Send(Box),
    SharedKey = crypto:hash(sha256,
                            crypto:hash(sha256,
                                        concat([NetId, Shared_ab, Shared_aB, Shared_Ab]))),
    DecBoxKey = crypto:hash(sha256, combine(SharedKey, long_pk())),
    EncBoxKey = crypto:hash(sha256, combine(SharedKey, ClLong_pk)),
    {ok, {DecBoxKey, DecNonce, EncBoxKey, EncNonce, ClLong_pk, NetId}}.

long_sk() ->
    base64:decode(keys:priv_key()).

long_pk() ->
    base64:decode(keys:pub_key()).



-ifdef(TEST).

simple_test() ->
    {ok, Pid} = config:start_link("test/ssb.cfg"),
    NetId = config:network_id(),
    {Eph_sk, Hmac, _Nonce} = gen_hello(NetId),
    ?assert(size(Eph_sk) == 32),
    ?assert(size(Hmac) == 64),
    gen_server:stop(Pid).

round_trip_test() ->
    {ok, Pid} = config:start_link("test/ssb.cfg"),
    NetId = config:network_id(),
    {_Eph_sk, Hmac, _} = gen_hello(NetId),
    {true, _Eph_pk, _} = check_hello(Hmac, NetId),
    gen_server:stop(Pid).

%% Full client+server handshake over an in-memory transport: the two Send/Recv
%% closures are wired together with message passing (no socket).  Proves the
%% transport abstraction carries the handshake and that both sides derive the
%% same box-stream keys/nonces, then round-trips a boxed message each way.
inmem_handshake_test_() ->
    {setup, fun setup_node/0, fun cleanup_node/1,
     fun(_) -> ?_test(run_inmem_handshake()) end}.

setup_node() ->
    catch gen_server:stop(keys),
    catch gen_server:stop(config),
    Home = filename:join("/tmp", "shs_inmem_"
                         ++ integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("test/ssb.cfg"),
    {ok, _} = keys:start_link(),
    Home.

cleanup_node(Home) ->
    catch gen_server:stop(keys),
    catch gen_server:stop(config),
    os:cmd("rm -rf " ++ Home),
    application:unset_env(ssb, ssb_home),
    ok.

run_inmem_handshake() ->
    NetId    = config:network_id(),
    ServerPk = base64:decode(keys:pub_key()),     %% server identity = node keys
    {ClientPk, ClientSk} = create_long_pair(),    %% fresh client identity
    Parent = self(),
    Server = spawn(fun() ->
        SSend = fun(D) -> Parent ! {s2c, D}, ok end,
        SRecv = fun(_N) -> receive {c2s, D} -> {ok, D} end end,
        {ok, Hello} = SRecv(64),
        Parent ! {server_done, do_server_shake_hands(Hello, SSend, SRecv, NetId)}
    end),
    CSend = fun(D) -> Server ! {c2s, D}, ok end,
    CRecv = fun(_N) -> receive {s2c, D} -> {ok, D} end end,
    {ok, {CDec, CDecN, CEnc, CEncN}} =
        do_client_shake_hands(CSend, CRecv, ServerPk, NetId, ClientPk, ClientSk),
    {ok, {SDec, SDecN, SEnc, SEncN, ClLongPk, _NetId}} =
        receive {server_done, R} -> R after 5000 -> error(server_timeout) end,
    %% Each side's encrypt key/nonce matches the other's decrypt key/nonce.
    ?assertEqual(CEnc,  SDec),
    ?assertEqual(CDec,  SEnc),
    ?assertEqual(CEncN, SDecN),
    ?assertEqual(CDecN, SEncN),
    ?assertEqual(ClientPk, ClLongPk),
    %% A boxed message survives in both directions using the derived keys.
    {B1, _} = boxstream:box(~"ping", CEncN, CEnc),
    ?assertMatch({complete, ~"ping", _, _}, boxstream:unbox(SDec, SDecN, B1)),
    {B2, _} = boxstream:box(~"pong", SEncN, SEnc),
    ?assertMatch({complete, ~"pong", _, _}, boxstream:unbox(CDec, CDecN, B2)).

%% Where a failing client handshake gives up is the whole diagnosis, and the
%% outbound log now depends on telling these three apart.  They used to be
%% one indistinguishable badmatch, which is why every outbound failure was
%% reported as "unknown network id".
handshake_failure_classification_test_() ->
    {setup, fun setup_node/0, fun cleanup_node/1,
     fun(_) ->
             [?_test(hello_drop_is_hello_rejected()),
              ?_test(auth_drop_is_auth_rejected()),
              ?_test(wrong_net_id_is_a_mismatch())]
     end}.

%% Hung up without answering: could be another network, could be a port that
%% is not SSB.  Ambiguous, and the caller is told only that much.
hello_drop_is_hello_rejected() ->
    ServerPk = base64:decode(keys:pub_key()),
    Send = fun(_) -> ok end,
    Recv = fun(_) -> {error, closed} end,
    ?assertError({hello_rejected, closed},
                 client_shake_hands_tunnel(Send, Recv, ServerPk)).

%% Answered our hello — so it accepted the network id — and then refused the
%% auth box.  This is the stale-key case: the identity is wrong, the network
%% is fine, and no other network id could have helped.
auth_drop_is_auth_rejected() ->
    ServerPk = base64:decode(keys:pub_key()),
    {_Sk, ServerHello, _N} = gen_hello(config:network_id()),
    Send = fun(_) -> ok end,
    Recv = fun(64) -> {ok, ServerHello};
              (80) -> {error, closed}
           end,
    ?assertError({auth_rejected, closed},
                 client_shake_hands_tunnel(Send, Recv, ServerPk)).

%% Answered under a network id that is not the one we sent — the only case
%% where the client has actually observed a mismatch rather than guessed one.
wrong_net_id_is_a_mismatch() ->
    ServerPk = base64:decode(keys:pub_key()),
    {_Sk, OtherHello, _N} = gen_hello(crypto:strong_rand_bytes(32)),
    Send = fun(_) -> ok end,
    Recv = fun(_) -> {ok, OtherHello} end,
    ?assertError(network_id_mismatch,
                 client_shake_hands_tunnel(Send, Recv, ServerPk)).

-endif.
