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
         server_shake_hands/3]).

check_hello(BinData) ->
    case size(BinData) of
        64 ->
            <<Hmac:32/binary,Eph_pk:32/binary>> = BinData,
            Valid = enacl:auth_verify(Hmac, Eph_pk, config:network_id()),
            <<Nonce:24/binary,_End:8/binary>> = Hmac,
            {Valid, Eph_pk, Nonce};
        _ ->
            {false, nobody, none}
    end.

gen_hello() ->

    #{public := Eph_pk,
      secret := Eph_sk} = enacl:box_keypair(),
    SizEph_pk = size(Eph_pk),

    NaclAuth = enacl:auth(Eph_pk, config:network_id()),
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

    DetSigA = enacl:sign_detached(concat([config:network_id(),
                                          RemotePubKey,
                                          ShaSab]),
                                  Priv_sk),

    Msg = concat([DetSigA, Pub_pk]),

    Box = create_box(Msg,
                         concat([config:network_id(),
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
                                 concat([config:network_id(),
                                         Shared_ab,
                                         Shared_aB,
                                         Shared_Ab]))),

    M = concat([config:network_id(),
                DetSigA,
                Pub_pk,
                ShaSab]),

    true = enacl:sign_verify_detached(DetSigB,
                                      M,
                                      RemotePubKey),

    SharedKey = crypto:hash(sha256,
                            crypto:hash(sha256,
                                        concat([config:network_id(),
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
                                 concat([config:network_id(),
                                         Shared_ab,
                                         Shared_aB]))),
    <<DetSigA:64/binary,ClLong_pk:32/binary>> = MsgPlain,
    true =
        enacl:sign_verify_detached(DetSigA,
                                   concat([config:network_id(),
                                           long_pk(),
                                           ShaSab]),
                                   ClLong_pk),
    Shared_Ab =
        mult(Eph_sk,pk_to_curve25519(ClLong_pk)),
    DetSigB =
        enacl:sign_detached(concat([config:network_id(),
                                    DetSigA,
                                    ClLong_pk,
                                    ShaSab]),
                            long_sk()),
    Box =
        create_box(DetSigB,
                       concat([config:network_id(),
                               Shared_ab,
                               Shared_aB,
                               Shared_Ab])),

    % server accepts
    Transport:send(Socket,Box),

    SharedKey = crypto:hash(sha256,
                            crypto:hash(sha256, concat([config:network_id(),
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
    {ok, Pid} = config:start_link("test/ssb.cfg"),
    {Eph_sk, Hmac, _Nonce} = gen_hello(),
    ?assert(size(Eph_sk) == 32),
    ?assert(size(Hmac) == 64),
    gen_server:stop(Pid).

round_trip_test() ->
    {ok, Pid} = config:start_link("test/ssb.cfg"),
    {_Eph_sk, Hmac, _} = gen_hello(),
    {true, _Eph_pk, _} = check_hello(Hmac),
    gen_server:stop(Pid).

-endif.
