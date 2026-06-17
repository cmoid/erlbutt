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
    {ok, ServerHmac} = Recv(64),
    {true, ServEph_pk, EncNonce} = check_hello(ServerHmac, NetId),
    Shared_ab = mult(Eph_sk, ServEph_pk),
    Shared_aB = mult(Eph_sk, pk_to_curve25519(RemotePubKey)),
    ShaSab = crypto:hash(sha256, Shared_ab),
    DetSigA = enacl:sign_detached(concat([NetId, RemotePubKey, ShaSab]), OurPrivKey),
    Msg = concat([DetSigA, OurPubKey]),
    Box = create_box(Msg, concat([NetId, Shared_ab, Shared_aB])),
    Send(Box),
    Shared_Ab = mult(sk_to_curve25519(OurPrivKey), ServEph_pk),
    {ok, ServData} = Recv(80),
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

-endif.
