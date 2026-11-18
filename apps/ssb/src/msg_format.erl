%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
-module(msg_format).
-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% The structural rules ssb-validate enforces on a classic (ed25519)
%% message, beyond its signature.
%%
%% Until this module erlbutt checked a peer's message for two things only:
%% a signature that verifies, and a `previous` that links our tail.
%% Everything else — content that is null or an array, a content type of
%% two characters or of two hundred, a `hash` that is not "sha256", a
%% timestamp that is a string — was stored and re-served.  Every JS peer
%% rejects such a message, and with it every later message in that feed,
%% so a node that keeps one is a node that disagrees with the network
%% about what the feed contains.
%%
%% Two kinds of rule live elsewhere, in message:validate/2, because they
%% decide whether the signature can be checked at all: the encoding of
%% the author and signature (exact suffix, canonical base64, exact
%% length), and the order of the value's keys.  The order has to be
%% there: store/2 re-encodes in canonical order, so a message signed over
%% any other order (or with an extra key) could not be stored faithfully
%% even if we wanted it.
%%
%% What is left is checkable from the decoded #message{} alone, and is
%% enforced by ssb_feed:store_msg_checked/2 under {require_valid_format,
%% Bool}: warn-and-count by default, as signatures were, until a real
%% corpus shows these rules reject nothing genuine.
-export([check/1,
         decode_sigil/4]).

%% A content type's length is measured in UTF-16 code units, because that
%% is what JavaScript's String.length counts.
-define(MIN_TYPE_LEN, 3).
-define(MAX_TYPE_LEN, 52).

-spec check(#message{}) -> ok | {error, binary()}.
check(#message{} = Msg) ->
    check([fun sequence/1, fun previous/1, fun timestamp/1, fun hash/1,
           fun content/1, fun max_length/1], Msg).

check([], _Msg) ->
    ok;
check([Rule | Rules], Msg) ->
    case Rule(Msg) of
        ok    -> check(Rules, Msg);
        Error -> Error
    end.

sequence(#message{sequence = Seq}) when is_integer(Seq), Seq >= 1 -> ok;
sequence(_) -> {error, ~"sequence must be a positive integer"}.

%% Whether `previous` names the RIGHT message is the chain check's job;
%% this only asks whether it could name one at all.  An archive genesis
%% carries its real predecessor at seq > 1, so null is required only at 1.
previous(#message{sequence = 1, previous = null}) -> ok;
previous(#message{sequence = 1}) ->
    {error, ~"previous must be null on the first message"};
previous(#message{previous = Prev}) ->
    case decode_sigil(Prev, ~"%", ~".sha256", 32) of
        {ok, _} -> ok;
        error   -> {error, ~"previous must be a message id"}
    end.

timestamp(#message{timestamp = Ts}) when is_number(Ts) -> ok;
timestamp(_) -> {error, ~"timestamp must be a number"}.

hash(#message{hash = ~"sha256"}) -> ok;
hash(_) -> {error, ~"hash must be \"sha256\""}.

content(#message{content = null}) ->
    {error, ~"content must not be null"};
content(#message{content = C}) when is_list(C) ->
    {error, ~"content must not be an array"};
content(#message{content = {Props}}) when is_list(Props) ->
    content_type(proplists:get_value(~"type", Props));
content(#message{content = C}) when is_binary(C) ->
    boxed(C);
content(_) ->
    {error, ~"content must be a string or an object"}.

content_type(Type) when is_binary(Type) ->
    case utf16_length(Type) of
        L when L < ?MIN_TYPE_LEN -> {error, ~"content type is too short"};
        L when L > ?MAX_TYPE_LEN -> {error, ~"content type is too long"};
        _                        -> ok
    end;
content_type(_) ->
    {error, ~"content type must be a string"}.

%% String content is a private message: canonical base64 then `.box`,
%% optionally followed by a version number (`.box2`).
boxed(C) ->
    case binary:matches(C, ~".box") of
        [] ->
            {error, ~"content string must be a .box"};
        Matches ->
            {Pos, Len} = lists:last(Matches),
            Body = binary:part(C, 0, Pos),
            Tail = binary:part(C, Pos + Len, byte_size(C) - Pos - Len),
            case Body =/= <<>> andalso all_digits(Tail)
                     andalso canonical_base64(Body) =/= error of
                true  -> ok;
                false -> {error, ~"content string must be canonical base64 .box"}
            end
    end.

max_length(#message{} = Msg) ->
    case message:value_length(Msg) of
        L when L > ?MAX_MSG_LENGTH -> {error, ~"message is too large"};
        _                          -> ok
    end.

%% Decode a sigil-wrapped base64 reference: an author (`@…​.ed25519`, 32
%% bytes), a signature (`…​.sig.ed25519`, 64), a message id
%% (`%…​.sha256`, 32).  Strict in every particular, because each leniency
%% is a second spelling of the same bytes, and a second spelling is a
%% second message id:
%%
%%   - the suffix must be exactly at the end (not merely somewhere in it)
%%   - the base64 must be canonical: what we decode re-encodes to itself
%%   - the decoded length must be exact.  libsodium's verify reads the
%%     first 64 bytes of a signature and ignores the rest, so without this
%%     a signature with junk appended verifies — and, the signature being
%%     part of what the id hashes, yields a second id for a genuine
%%     message.  Served first, that copy would wedge the feed: the
%%     author's next message names the real id as `previous`.
-spec decode_sigil(term(), binary(), binary(), pos_integer()) ->
          {ok, binary()} | error.
decode_sigil(Ref, Prefix, Suffix, Size) when is_binary(Ref) ->
    PL = byte_size(Prefix),
    SL = byte_size(Suffix),
    case Ref of
        <<Prefix:PL/binary, Rest/binary>> when byte_size(Rest) > SL ->
            BL = byte_size(Rest) - SL,
            case Rest of
                <<Body:BL/binary, Suffix:SL/binary>> ->
                    case canonical_base64(Body) of
                        Bytes when byte_size(Bytes) =:= Size -> {ok, Bytes};
                        _                                    -> error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end;
decode_sigil(_, _, _, _) ->
    error.

canonical_base64(Body) ->
    try base64:decode(Body) of
        Bytes ->
            case base64:encode(Bytes) of
                Body -> Bytes;
                _    -> error
            end
    catch _:_ ->
        error
    end.

all_digits(Bin) ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Bin)).

utf16_length(Bin) ->
    try unicode:characters_to_list(Bin, utf8) of
        CPs when is_list(CPs) ->
            lists:sum([case CP > 16#FFFF of true -> 2; false -> 1 end
                       || CP <- CPs]);
        _ ->
            byte_size(Bin)
    catch _:_ ->
        byte_size(Bin)
    end.

-ifdef(TEST).

%% Every case here is built and signed afresh with a throwaway key, so the
%% signature always verifies and each assertion is about the one rule the
%% case breaks.  The rules follow ssb-validate; the cases are our own.

keypair() ->
    #{public := Pub, secret := Priv} = enacl:sign_keypair(),
    {iolist_to_binary(["@", base64:encode(Pub), ".ed25519"]), Priv}.

%% Sign an arbitrary value proplist exactly as given — any key order, any
%% field values — the way a careless or hostile client might.
sign(Props, Priv) ->
    Enc = iolist_to_binary(message:ssb_encoder({Props}, fun message:ssb_encoder/3,
                                               [pretty, use_nil])),
    Sig = enacl:sign_detached(Enc, Priv),
    Props ++ [{~"signature", iolist_to_binary([base64:encode(Sig), ".sig.ed25519"])}].

value(Author, Overrides) ->
    Base = [{~"previous",  null},
            {~"author",    Author},
            {~"sequence",  1},
            {~"timestamp", 1700000000000},
            {~"hash",      ~"sha256"},
            {~"content",   {[{~"type", ~"post"}, {~"text", ~"hi"}]}}],
    [{K, proplists:get_value(K, Overrides, V)} || {K, V} <- Base].

%% Build, sign, decode and verify: the #message{} as ingest would see it.
signed(Overrides) ->
    {Author, Priv} = keypair(),
    message:from_value(sign(value(Author, Overrides), Priv), true).

verdict(Overrides) ->
    Msg = signed(Overrides),
    ?assertEqual(true, Msg#message.validated),
    check(Msg).

msg_id() ->
    iolist_to_binary(["%", base64:encode(crypto:strong_rand_bytes(32)), ".sha256"]).

box() ->
    iolist_to_binary([base64:encode(crypto:strong_rand_bytes(100)), ".box"]).

well_formed_test_() ->
    Type = fun(T) -> {~"content", {[{~"type", T}]}} end,
    [?_assertEqual(ok, verdict([])),
     ?_assertEqual(ok, verdict([Type(~"abc")])),
     ?_assertEqual(ok, verdict([Type(binary:copy(~"x", 52))])),
     %% 26 astral characters are 52 UTF-16 units: at the limit, not over
     ?_assertEqual(ok, verdict([Type(binary:copy(~"\x{1F600}", 26))])),
     %% three BMP characters that are nine UTF-8 bytes are still three
     ?_assertEqual(ok, verdict([Type(~"\x{65E5}\x{672C}\x{8A9E}")])),
     ?_assertEqual(ok, verdict([{~"content", box()}])),
     ?_assertEqual(ok, verdict([{~"content", <<(box())/binary, "2">>}])),
     %% any number is a timestamp: ssb-validate does not judge plausibility
     ?_assertEqual(ok, verdict([{~"timestamp", 42}])),
     ?_assertEqual(ok, verdict([{~"timestamp", 1700000000000.5}])),
     ?_assertEqual(ok, verdict([{~"timestamp", 99999999999999999999999999999}])),
     ?_assertEqual(ok, verdict([{~"sequence", 2}, {~"previous", msg_id()}]))].

malformed_test_() ->
    Type = fun(T) -> {~"content", {[{~"type", T}]}} end,
    E = fun(Overrides) -> element(1, verdict(Overrides)) end,
    [%% content
     ?_assertEqual(error, E([{~"content", null}])),
     ?_assertEqual(error, E([{~"content", [~"post"]}])),
     ?_assertEqual(error, E([{~"content", 12}])),
     ?_assertEqual(error, E([{~"content", true}])),
     ?_assertEqual(error, E([{~"content", ~"hello"}])),
     ?_assertEqual(error, E([{~"content", ~"not base64!.box"}])),
     ?_assertEqual(error, E([{~"content", <<(box())/binary, "x">>}])),
     ?_assertEqual(error, E([{~"content", {[]}}])),
     ?_assertEqual(error, E([Type(7)])),
     ?_assertEqual(error, E([Type(null)])),
     ?_assertEqual(error, E([Type({[{~"nested", ~"obj"}]})])),
     ?_assertEqual(error, E([Type(~"ab")])),
     ?_assertEqual(error, E([Type(~"")])),
     ?_assertEqual(error, E([Type(binary:copy(~"x", 53))])),
     ?_assertEqual(error, E([Type(binary:copy(~"\x{1F600}", 27))])),
     %% the other fields
     ?_assertEqual(error, E([{~"hash", ~"sha512"}])),
     ?_assertEqual(error, E([{~"hash", null}])),
     ?_assertEqual(error, E([{~"timestamp", ~"1700000000000"}])),
     ?_assertEqual(error, E([{~"timestamp", null}])),
     ?_assertEqual(error, E([{~"sequence", ~"1"}])),
     ?_assertEqual(error, E([{~"sequence", 0}])),
     ?_assertEqual(error, E([{~"sequence", 1.5}])),
     ?_assertEqual(error, E([{~"previous", msg_id()}])),
     ?_assertEqual(error, E([{~"sequence", 2}])),
     ?_assertEqual(error, E([{~"sequence", 2}, {~"previous", ~"%junk.sha256"}])),
     %% over the 8192-byte limit, measured as message:value_length/1 does
     ?_assertEqual(error, E([{~"content", {[{~"type", ~"post"},
                                            {~"text", binary:copy(~"x", 8200)}]}}]))].

decode_sigil_test_() ->
    Bytes = crypto:strong_rand_bytes(32),
    Good  = iolist_to_binary(["@", base64:encode(Bytes), ".ed25519"]),
    [?_assertEqual({ok, Bytes}, decode_sigil(Good, ~"@", ~".ed25519", 32)),
     ?_assertEqual(error, decode_sigil(Good, ~"@", ~".ed25519", 64)),
     ?_assertEqual(error, decode_sigil(<<Good/binary, "x">>, ~"@", ~".ed25519", 32)),
     ?_assertEqual(error, decode_sigil(binary:part(Good, 1, byte_size(Good) - 1),
                                       ~"@", ~".ed25519", 32)),
     ?_assertEqual(error, decode_sigil(~"@.ed25519", ~"@", ~".ed25519", 32)),
     ?_assertEqual(error, decode_sigil(null, ~"@", ~".ed25519", 32)),
     ?_assertEqual(error, decode_sigil(42, ~"@", ~".ed25519", 32)),
     %% missing padding is a second spelling of the same bytes
     ?_assertEqual(error, decode_sigil(binary:replace(Good, ~"=", ~""),
                                       ~"@", ~".ed25519", 32))].

-endif.
