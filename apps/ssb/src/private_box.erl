%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2025 Charles Moid
%%
%% Implements the SSB private-box encryption scheme.
%%
%% Wire format (before base64 + ".box" suffix):
%%
%%   nonce      (24 bytes, random)
%%   header_pk  (32 bytes, ephemeral curve25519 public key)
%%   headers    (49 bytes × N recipients, max 7)
%%     each = secretbox( num_recipients(1) || body_key(32) )
%%             under key = HMAC-SHA512256( scalarmult(header_sk, recip_pk),
%%                                        nonce || header_pk )
%%   body       secretbox( plaintext, nonce, body_key )
%%
%% The ".box" suffix lets peers detect private messages without parsing JSON.
-module(private_box).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([encrypt/2,
         decrypt/1,
         is_private/1]).

-define(MAX_RECIPIENTS, 7).
-define(NONCE_BYTES,    24).
-define(HEADER_PK_BYTES, 32).
-define(BODY_KEY_BYTES,  32).
-define(HEADER_ENC,      49).   %% 33 + 16 (secretbox MAC)

%%%===================================================================
%%% Public API
%%%===================================================================

%% Returns <<"base64ciphertext.box">> addressed to up to 7 recipient feed IDs
%% (strings like <<"@pubkey.ed25519">>).
-spec encrypt(binary(), [binary()]) -> binary().
encrypt(Content, RecipientIds) when length(RecipientIds) =< ?MAX_RECIPIENTS ->
    Nonce    = enacl:randombytes(?NONCE_BYTES),
    BodyKey  = enacl:randombytes(?BODY_KEY_BYTES),
    #{public := HeaderPk, secret := HeaderSk} = enacl:box_keypair(),
    NumRecips = length(RecipientIds),
    EncHeaders = [encrypt_header(NumRecips, BodyKey, HeaderSk, HeaderPk, Nonce, Id)
                  || Id <- RecipientIds],
    EncBody = enacl:secretbox(Content, Nonce, BodyKey),
    Ciphertext = iolist_to_binary([Nonce, HeaderPk | EncHeaders] ++ [EncBody]),
    <<(base64:encode(Ciphertext))/binary, ".box">>.

%% Returns {ok, Plaintext} if addressed to us, not_for_me otherwise.
-spec decrypt(binary()) -> {ok, binary()} | not_for_me.
decrypt(Boxed) ->
    case is_private(Boxed) of
        false -> not_for_me;
        true  ->
            B64Size = byte_size(Boxed) - 4,  %% strip ".box"
            <<B64:B64Size/binary, ".box">> = Boxed,
            try
                Data = base64:decode(B64),
                try_decrypt(Data)
            catch
                _:_ -> not_for_me
            end
    end.

%% True when the content binary ends with ".box".
-spec is_private(binary()) -> boolean().
is_private(Content) when is_binary(Content) ->
    binary:longest_common_suffix([Content, <<".box">>]) == 4;
is_private(_) ->
    false.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% enacl:auth (HMAC-SHA512256) is used here as a KDF, not for authentication.
encrypt_header(NumRecips, BodyKey, HeaderSk, HeaderPk, Nonce, RecipId) ->
    RecipCurvePk  = id_to_curve25519_pk(RecipId),
    SharedSecret  = enacl:curve25519_scalarmult(HeaderSk, RecipCurvePk),
    RecipKey      = enacl:auth(<<Nonce/binary, HeaderPk/binary>>, SharedSecret),
    enacl:secretbox(<<NumRecips:8, BodyKey/binary>>, Nonce, RecipKey).

try_decrypt(Data) ->
    <<Nonce:?NONCE_BYTES/binary,
      HeaderPk:?HEADER_PK_BYTES/binary,
      HeadersAndBody/binary>> = Data,
    MyCurvePrivKey = my_curve25519_sk(),
    SharedSecret   = enacl:curve25519_scalarmult(MyCurvePrivKey, HeaderPk),
    MyKey          = enacl:auth(<<Nonce/binary, HeaderPk/binary>>, SharedSecret),
    try_headers(HeadersAndBody, Nonce, MyKey, 0).

%% We don't know which slot holds our header, so try each in turn.
%% NumRecips in the plaintext tells us where the body begins (after all headers).
try_headers(HeadersAndBody, Nonce, MyKey, SlotIdx) when SlotIdx < ?MAX_RECIPIENTS ->
    Offset = SlotIdx * ?HEADER_ENC,
    case HeadersAndBody of
        <<_:Offset/binary, Header:?HEADER_ENC/binary, _/binary>> ->
            case enacl:secretbox_open(Header, Nonce, MyKey) of
                {ok, <<NumRecips:8, BodyKey:?BODY_KEY_BYTES/binary>>} ->
                    BodyStart = NumRecips * ?HEADER_ENC,
                    <<_:BodyStart/binary, EncBody/binary>> = HeadersAndBody,
                    case enacl:secretbox_open(EncBody, Nonce, BodyKey) of
                        {ok, Body} -> {ok, Body};
                        _          -> not_for_me
                    end;
                {error, _} ->
                    try_headers(HeadersAndBody, Nonce, MyKey, SlotIdx + 1)
            end;
        _ ->
            not_for_me
    end;
try_headers(_, _, _, _) ->
    not_for_me.

%% Keys on disk are ed25519; DH requires curve25519, so convert here.
my_curve25519_sk() ->
    Ed25519Sk = base64:decode(keys:priv_key()),
    enacl:crypto_sign_ed25519_secret_to_curve25519(Ed25519Sk).

%% Convert a feed ID like <<"@base64pub.ed25519">> to a curve25519 pk.
id_to_curve25519_pk(<<"@", Rest/binary>>) ->
    PubB64 = hd(string:replace(Rest, ".ed25519", "")),
    Ed25519Pk = base64:decode(PubB64),
    enacl:crypto_sign_ed25519_public_to_curve25519(Ed25519Pk).

-ifdef(TEST).

round_trip_test() ->
    {ok, CPid} = config:start_link("test/ssb.cfg"),
    {ok, KPid} = keys:start_link(),
    Me = keys:pub_key_disp(),
    Plaintext = ~"hello private world",
    Boxed = private_box:encrypt(Plaintext, [Me]),
    ?assert(is_private(Boxed)),
    {ok, Decrypted} = private_box:decrypt(Boxed),
    ?assert(Decrypted == Plaintext),
    gen_server:stop(KPid),
    gen_server:stop(CPid).

not_for_me_test() ->
    {ok, CPid} = config:start_link("test/ssb.cfg"),
    {ok, KPid} = keys:start_link(),
    %% Encrypt to a random ed25519 identity that is not ours.
    #{public := OtherEd25519Pk} = enacl:crypto_sign_ed25519_keypair(),
    OtherId = <<"@", (base64:encode(OtherEd25519Pk))/binary, ".ed25519">>,
    Boxed = encrypt(~"secret", [OtherId]),
    ?assert(decrypt(Boxed) == not_for_me),
    gen_server:stop(KPid),
    gen_server:stop(CPid).

-endif.
