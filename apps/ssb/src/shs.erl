%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2018 Dionne Associates, LLC.
-module(shs).

-include("ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-import(utils, [concat/1,
                combine/2]).
-export([open_box/3,
         create_long_pair/0,
         client_shake_hands/2,
         server_shake_hands/3]).

check_hello(BinData) ->
    case size(BinData) of
        64 ->
            <<Hmac:32/binary,Eph_pk:32/binary>> = BinData,
            Valid = enacl:auth_verify(Hmac,Eph_pk,?NETWORK_ID),
            <<Nonce:24/binary,_End:8/binary>> = Hmac,
            {Valid, Eph_pk, Nonce};
        _ ->
            {false, nobody, none}
    end.

gen_hello() ->

    #{public := Eph_pk,
      secret := Eph_sk} = enacl:box_keypair(),
    SizEph_pk = size(Eph_pk),

    NaclAuth = enacl:auth(Eph_pk, ?NETWORK_ID),
    SizNaclAuth = size(NaclAuth),

    <<Nonce:24/binary,_End:8/binary>> = NaclAuth,

    {Eph_sk, <<NaclAuth:SizNaclAuth/binary,
                Eph_pk:SizEph_pk/binary>>, Nonce}.

mult(Key1, Key2) ->
    enacl:curve25519_scalarmult(Key1, Key2).

sk_to_curve25519(Key) ->
    enacl:crypto_sign_ed25519_secret_to_curve25519(Key).

pk_to_curve25519(Key) ->
    enacl:crypto_sign_ed25519_public_to_curve25519(Key).

open_box(Data, Nonce, Key) ->
    {ok, Msg} =
        enacl:secretbox_open(Data,Nonce,Key),
    Msg.

create_box(Data, Key) ->
    enacl:secretbox(Data,?SHS_NONCE,crypto:hash(sha256, Key)).

create_long_pair() ->
    KeyPair = enacl:crypto_sign_ed25519_keypair(),
    {maps:get(public, KeyPair),
     maps:get(secret, KeyPair)}.

client_shake_hands(Socket, RemotePubKey) ->

    {Pub_pk, Priv_sk} = create_long_pair(),
    {Eph_sk, Hmac, DecNonce} = gen_hello(),

    % say hello to server
    gen_tcp:send(Socket, Hmac),

    % receive response from server
    {ok, ServerHmac} = gen_tcp:recv(Socket, 64, 5000),

    {true, ServEph_pk, EncNonce} = check_hello(ServerHmac),

    Shared_ab = mult(Eph_sk, ServEph_pk),
    Shared_aB = mult(Eph_sk,
                         pk_to_curve25519(RemotePubKey)),

    ShaSab = crypto:hash(sha256,Shared_ab),

    DetSigA = enacl:sign_detached(concat([?NETWORK_ID,
                                          RemotePubKey,
                                          ShaSab]),
                                  Priv_sk),

    Msg = concat([DetSigA, Pub_pk]),

    Box = create_box(Msg,
                         concat([?NETWORK_ID,
                                 Shared_ab,
                                 Shared_aB])),

    % client authenticates
    gen_tcp:send(Socket, Box),

    Shared_Ab = mult(sk_to_curve25519(Priv_sk),
                         ServEph_pk),
    {ok, ServData} = gen_tcp:recv(Socket, 80, 5000),

    DetSigB =
        open_box(ServData,
                     ?SHS_NONCE,
                     crypto:hash(sha256,
                                 concat([?NETWORK_ID,
                                         Shared_ab,
                                         Shared_aB,
                                         Shared_Ab]))),

    M = concat([?NETWORK_ID,
                DetSigA,
                Pub_pk,
                ShaSab]),

    true = enacl:sign_verify_detached(DetSigB,
                                      M,
                                      RemotePubKey),

    SharedKey = crypto:hash(sha256,
                            crypto:hash(sha256,
                                        concat([?NETWORK_ID,
                                                Shared_ab,
                                                Shared_aB,
                                                Shared_Ab]))),
    DecBoxKey = crypto:hash(sha256,
                          concat([SharedKey, Pub_pk])),
    EncBoxKey = crypto:hash(sha256,
                          concat([SharedKey, RemotePubKey])),

    {ok, {Socket, DecBoxKey, DecNonce, EncBoxKey, EncNonce}}.

server_shake_hands(Data, Socket, Transport) ->

    % check hellow from client
    {true, ClEph_pk, EncNonce} = check_hello(Data),

    {Eph_sk, ServerHmac, DecNonce} = gen_hello(),
    % server says hello
    Transport:send(Socket,ServerHmac),

    Shared_ab = mult(Eph_sk, ClEph_pk),
    Shared_aB = mult(sk_to_curve25519(long_sk()),
                                 ClEph_pk),

    % receive client autheticate
    {ok, ServData} = Transport:recv(Socket, 112, 5000),

    ShaSab = crypto:hash(sha256,Shared_ab),
    MsgPlain =
        open_box(ServData, ?SHS_NONCE,
                     crypto:hash(sha256,
                                 concat([?NETWORK_ID,
                                         Shared_ab,
                                         Shared_aB]))),
    <<DetSigA:64/binary,ClLong_pk:32/binary>> = MsgPlain,
    true =
        enacl:sign_verify_detached(DetSigA,
                                   concat([?NETWORK_ID,
                                           long_pk(),
                                           ShaSab]),
                                   ClLong_pk),
    Shared_Ab =
        mult(Eph_sk,pk_to_curve25519(ClLong_pk)),
    DetSigB =
        enacl:sign_detached(concat([?NETWORK_ID,
                                    DetSigA,
                                    ClLong_pk,
                                    ShaSab]),
                            long_sk()),
    Box =
        create_box(DetSigB,
                       concat([?NETWORK_ID,
                               Shared_ab,
                               Shared_aB,
                               Shared_Ab])),

    % server accepts
    Transport:send(Socket,Box),

    SharedKey = crypto:hash(sha256,
                            crypto:hash(sha256, concat([?NETWORK_ID,
                                                        Shared_ab,
                                                        Shared_aB,
                                                        Shared_Ab]))),
    DecBoxKey = crypto:hash(sha256, combine(SharedKey,
                                            long_pk())),
    EncBoxKey = crypto:hash(sha256, combine(SharedKey,
                                            ClLong_pk)),

    {ok, {DecBoxKey, DecNonce, EncBoxKey, EncNonce}}.

long_sk() ->
    base64:decode(keys:priv_key()).

long_pk() ->
    base64:decode(keys:pub_key()).



-ifdef(TEST).

simple_test() ->
    {Eph_sk, Hmac, _Nonce} = gen_hello(),
    ?assert(size(Eph_sk) == 32),
    ?assert(size(Hmac) == 64).

round_trip_test() ->
    {_Eph_sk, Hmac, _Nonce} = gen_hello(),
    {true, _Eph_pk, _Nonce} = check_hello(Hmac).

-endif.
