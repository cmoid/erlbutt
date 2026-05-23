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
         server_shake_hands/3,
         server_shake_hands/4]).

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
    do_client_shake_hands(Socket, RemotePubKey, config:network_id(), long_pk(), long_sk()).

%% NetId as binary: retry-loop path in ssb_peer, uses default node keys.
client_shake_hands(Socket, RemotePubKey, NetId) when is_binary(NetId) ->
    do_client_shake_hands(Socket, RemotePubKey, NetId, long_pk(), long_sk());

%% Key-pair tuple: invite path, uses ephemeral invite keys with primary NetId.
client_shake_hands(Socket, RemotePubKey, {OurPubKey, OurPrivKey}) ->
    do_client_shake_hands(Socket, RemotePubKey, config:network_id(), OurPubKey, OurPrivKey).

do_client_shake_hands(Socket, RemotePubKey, NetId, OurPubKey, OurPrivKey) ->
    {Eph_sk, Hmac, DecNonce} = gen_hello(NetId),
    gen_tcp:send(Socket, Hmac),
    {ok, ServerHmac} = gen_tcp:recv(Socket, 64, 5000),
    {true, ServEph_pk, EncNonce} = check_hello(ServerHmac, NetId),
    Shared_ab = mult(Eph_sk, ServEph_pk),
    Shared_aB = mult(Eph_sk, pk_to_curve25519(RemotePubKey)),
    ShaSab = crypto:hash(sha256, Shared_ab),
    DetSigA = enacl:sign_detached(concat([NetId, RemotePubKey, ShaSab]), OurPrivKey),
    Msg = concat([DetSigA, OurPubKey]),
    Box = create_box(Msg, concat([NetId, Shared_ab, Shared_aB])),
    gen_tcp:send(Socket, Box),
    Shared_Ab = mult(sk_to_curve25519(OurPrivKey), ServEph_pk),
    {ok, ServData} = gen_tcp:recv(Socket, 80, 5000),
    DetSigB = open_box(ServData, ?SHS_NONCE,
                       crypto:hash(sha256, concat([NetId, Shared_ab, Shared_aB, Shared_Ab]))),
    M = concat([NetId, DetSigA, OurPubKey, ShaSab]),
    true = enacl:sign_verify_detached(DetSigB, M, RemotePubKey),
    SharedKey = crypto:hash(sha256,
                            crypto:hash(sha256,
                                        concat([NetId, Shared_ab, Shared_aB, Shared_Ab]))),
    DecBoxKey = crypto:hash(sha256, concat([SharedKey, OurPubKey])),
    EncBoxKey = crypto:hash(sha256, concat([SharedKey, RemotePubKey])),
    {ok, {Socket, DecBoxKey, DecNonce, EncBoxKey, EncNonce}}.

server_shake_hands(Data, Socket, Transport) ->
    server_shake_hands(Data, Socket, Transport, config:network_ids()).

server_shake_hands(Data, Socket, Transport, NetIds) ->
    WinNetId = find_network_id(Data, NetIds),
    do_server_shake_hands(Data, Socket, Transport, WinNetId).

find_network_id(_Data, []) ->
    error(no_matching_network_id);
find_network_id(Data, [NetId | Rest]) ->
    case check_hello(Data, NetId) of
        {true, _, _} -> NetId;
        _            -> find_network_id(Data, Rest)
    end.

do_server_shake_hands(Data, Socket, Transport, NetId) ->
    {true, ClEph_pk, EncNonce} = check_hello(Data, NetId),
    {Eph_sk, ServerHmac, DecNonce} = gen_hello(NetId),
    Transport:send(Socket, ServerHmac),
    Shared_ab = mult(Eph_sk, ClEph_pk),
    Shared_aB = mult(sk_to_curve25519(long_sk()), ClEph_pk),
    {ok, ServData} = Transport:recv(Socket, 112, 5000),
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
    Transport:send(Socket, Box),
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

-endif.
