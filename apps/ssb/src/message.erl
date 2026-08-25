%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(message).
-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([decode/2,
         decode_value/2,
         encode/1,
         encode_value/1,
         encode_value_decrypted/2,
         encode_decrypted/2,
         ssb_encoder/3,
         is_null_ref/1,
         new_msg/4]).


%% Every spelling of "no reference" that reaches us: `null` from JSON,
%% `nil` from the decoder's use_nil mode, `undefined` from an unset record
%% field.  A genesis message's `previous` is the usual subject.
%%
%% It lives here, beside the decoding that produces those spellings, rather
%% than in any one of its callers — ssb_feed compares refs with it, and
%% feed_floor asks it of an archive genesis before taking a floor.  Two
%% copies of the list would drift the moment a fourth spelling appears.
is_null_ref(null)      -> true;
is_null_ref(nil)       -> true;
is_null_ref(undefined) -> true;
is_null_ref(_)         -> false.

new_msg(Previous, Sequence, Content, {PubKey, PrivKey}) ->
    Timestamp = current_time(),
    Hash = ~"sha256",
    NewMsg = #message{previous = Previous,
                      author = PubKey,
                      sequence = Sequence,
                      timestamp = Timestamp,
                      hash = Hash,
                      content = Content,
                      received = Timestamp},
    EncNewMsg = ssb_encoder({canonical_sign_props(NewMsg)},
                            fun ssb_encoder/3,
                            [pretty, use_nil]),
    Sig = enacl:sign_detached(EncNewMsg,
                              base64:decode(PrivKey)),
    EncSig = ?l2b(utils:base_64(Sig) ++ ".sig.ed25519"),
    %% Now add the sig to original msg
    add_sig(NewMsg, EncSig).

encode(#message{id = Key, received = Received, swapped = Swapped} = Msg) ->
    MsgProps = msg_to_proplist(Msg),
    EncMsg = build_props(MsgProps, Swapped),

    iolist_to_binary(ssb_encoder({[{~"key", Key},
                                   {~"value", {EncMsg}},
                                   {~"timestamp", Received}]}, fun ssb_encoder/3, [use_nil])).

%% Just the signed message value ({previous, author, sequence, content,
%% signature, ...}), without the {key, value, timestamp} envelope — the
%% shape ssb-db `get` returns.
encode_value(#message{swapped = Swapped} = Msg) ->
    EncMsg = build_props(msg_to_proplist(Msg), Swapped),
    iolist_to_binary(ssb_encoder({EncMsg}, fun ssb_encoder/3, [use_nil])).

%% Like encode_value/1 for a decrypted private message: the boxed content
%% string is replaced by the given decrypted content object (EJSON) and a
%% `private: true` marker is added, matching ssb-db get({private:true}).
encode_value_decrypted(#message{} = Msg, ContentObj) ->
    iolist_to_binary(ssb_encoder({value_decrypted(Msg, ContentObj)},
                                 fun ssb_encoder/3, [use_nil])).

%% Like encode/1 (the {key, value, timestamp} envelope) but with the
%% boxed content replaced by the decrypted object and a private marker —
%% the shape a feed rollup returns for a private message.
encode_decrypted(#message{id = Key, received = Received} = Msg, ContentObj) ->
    iolist_to_binary(
      ssb_encoder({[{~"key", Key},
                    {~"value", {value_decrypted(Msg, ContentObj)}},
                    {~"timestamp", Received}]}, fun ssb_encoder/3, [use_nil])).

value_decrypted(#message{swapped = Swapped} = Msg, ContentObj) ->
    Base = build_props(msg_to_proplist(Msg), Swapped),
    lists:keyreplace(~"content", 1, Base, {~"content", ContentObj})
        ++ [{~"private", true}].

%% Decode a value-only JSON binary (as sent by EBT / createHistoryStream keys:false).
%% The message ID is computed by hashing the canonical (pretty) JSON, matching
%% the same form used when the message was originally signed and stored.
decode_value(ValueJson, CheckValid) ->
    {ValueProps} = utils:nat_decode(ValueJson),
    IsSwapped = is_swapped(ValueProps),
    IsValid   = validate(CheckValid, ValueProps),
    %% Re-encode in canonical (pretty-printed) form before hashing.
    %% EBT peers send compact JSON, but SSB IDs are SHA256 of the
    %% pretty-printed canonical form (JS JSON.stringify(v, null, 2)).
    CanonJson = iolist_to_binary(ssb_encoder({ValueProps}, fun ssb_encoder/3, [pretty, use_nil])),
    Id        = compute_id(CanonJson),
    #message{id        = Id,
             previous  = ?pgv(~"previous",  ValueProps),
             author    = ?pgv(~"author",    ValueProps),
             sequence  = ?pgv(~"sequence",  ValueProps),
             timestamp = ?pgv(~"timestamp", ValueProps),
             hash      = ?pgv(~"hash",      ValueProps),
             content   = ?pgv(~"content",   ValueProps),
             signature = ?pgv(~"signature", ValueProps),
             received  = integer_to_binary(current_time()),
             validated = IsValid,
             swapped   = IsSwapped}.

decode(Msg, CheckValid) ->
    {DecDataProps} = utils:nat_decode(Msg),
    Key = ?pgv(~"key", DecDataProps),
    {ValueProps} = ?pgv(~"value", DecDataProps),
    IsSwapped = is_swapped(ValueProps),
    IsValid = validate(CheckValid, ValueProps),
    #message{id = Key,
             previous = ?pgv(~"previous", ValueProps),
             author = ?pgv(~"author", ValueProps),
             sequence = ?pgv(~"sequence", ValueProps),
             timestamp = ?pgv(~"timestamp", ValueProps),
             hash = ?pgv(~"hash", ValueProps),
             content = ?pgv(~"content", ValueProps),
             signature = ?pgv(~"signature", ValueProps),
             received = ?pgv(~"timestamp", DecDataProps),
             validated = IsValid,
             swapped = IsSwapped}.

is_swapped(PropList) ->
    SecondElement = lists:nth(2, PropList),
    case SecondElement of
        {Key, _Value} -> Key == ~"sequence";
        _ -> false
    end.

build_props(Props, Swapped) ->
    [{~"previous", ?pgv(~"previous", Props)}] ++
        check_swapped(Props, Swapped) ++
        [{~"timestamp", ?pgv(~"timestamp", Props)},
         {~"hash", ?pgv(~"hash", Props)},
         {~"content", ?pgv(~"content", Props)},
         {~"signature", ?pgv(~"signature", Props)}].

check_swapped(Props, Swapped) ->
    Seq = {~"sequence", ?pgv(~"sequence", Props)},
    Auth = {~"author", ?pgv(~"author", Props)},
    if Swapped ->
            [Seq, Auth];
       true ->
            [Auth, Seq]
    end.

%% `not_checked` rather than `false` when validation was not requested.
%%
%% These are completely different facts — "this signature is wrong" and
%% "nobody looked" — and collapsing them into `false` is why the
%% validated field sat unread for so long: no consumer could act on it
%% without also rejecting every message it had decoded cheaply.
validate(false, _MsgProps) ->
    not_checked;
validate(true, MsgProps) ->
    try
        Author = ?pgv(~"author", MsgProps),

        %% remove signature from message and encode as json
        DelSigProps = proplists:delete(~"signature", MsgProps),
        EncMsg = ssb_encoder({DelSigProps}, fun ssb_encoder/3, [pretty, use_nil]),

        %% extract and decode the keys for the signature and the author
        Sig = ?pgv(~"signature", MsgProps),
        <<"@",KeySuf/binary>> = Author,
        %% Assertive match, like the author line above it: a signature that
        %% is not a binary is not a signature, and drops into the catch
        %% below as an unvalidatable message.
        <<SigBody/binary>> = Sig,
        AuthorPk = base64:decode(utils:strip_suffix(KeySuf, ~".ed25519")),
        SigDec = base64:decode(utils:strip_suffix(SigBody, ~".sig.ed25519")),

        %% verify
        enacl:sign_verify_detached(SigDec, EncMsg, AuthorPk)
    catch
        error:Reason ->
            ?LOG_INFO("Unable to validate due to: ~p ~n",
                  [Reason]),
            false
    end.

add_sig(NewMsg, EncSig) ->
    CanonProps = canonical_sign_props(NewMsg) ++ [{~"signature", EncSig}],
    MsgId = compute_id(ssb_encoder({CanonProps}, fun ssb_encoder/3, [pretty, use_nil])),
    NewMsg#message{id = MsgId,
                   signature = EncSig}.

msg_to_proplist(Msg) ->
    Fields = record_info(fields, message),
    FieldsBinary = lists:map(fun(A) when is_atom(A) ->
                                     atom_to_binary(A, utf8) end,
                             Fields),
    MsgList = tuple_to_list(Msg),
    MsgTail = case MsgList of
                  [_|Tail] -> Tail;
                  [] -> []
              end,
    lists:zip(FieldsBinary, MsgTail).

%% Only the 6 fields that SSB signs over — no id, received, validated, swapped.
canonical_sign_props(#message{previous  = Prev,  author    = Auth,
                               sequence  = Seq,   timestamp = TS,
                               hash      = Hash,  content   = Content}) ->
    [{~"previous",  Prev},
     {~"author",    Auth},
     {~"sequence",  Seq},
     {~"timestamp", TS},
     {~"hash",      Hash},
     {~"content",   Content}].

compute_id(CanonJson) ->
    ?l2b("%" ++
        utils:base_64(crypto:hash(sha256, ssb_hash_bytes(CanonJson)))
        ++
        ".sha256").

%% SSB message IDs are the SHA256 of the canonical JSON hashed the way
%% ssb-keys does it: Buffer.from(jsonString, 'binary'), i.e. every UTF-16
%% code unit reduced to its low byte (latin1) — NOT the UTF-8 encoding.
%% For ASCII the two are identical, so existing ids are unchanged; for
%% non-ASCII (e.g. the U+202F narrow no-break space macOS puts in
%% screenshot filenames) they differ, and we must match ssb-keys so that
%% cross-client references (votes, backlinks, replies) to a non-ASCII
%% message resolve to the same id.  Signing stays UTF-8 (ssb-keys signs
%% over Buffer.from(json) with no encoding); only the id hash is latin1.
ssb_hash_bytes(CanonJson) ->
    << <<B>> || CP <- unicode:characters_to_list(CanonJson, utf8),
                B  <- utf16_low_bytes(CP) >>.

%% ssb-keys' 'binary' encoding of one codepoint: a BMP code unit contributes
%% its low byte; an astral codepoint becomes a UTF-16 surrogate pair, each
%% surrogate contributing its low byte.
utf16_low_bytes(CP) when CP =< 16#FFFF ->
    [CP band 16#FF];
utf16_low_bytes(CP) ->
    C  = CP - 16#10000,
    Hi = 16#D800 bor (C bsr 10),
    Lo = 16#DC00 bor (C band 16#3FF),
    [Hi band 16#FF, Lo band 16#FF].

current_time() ->
    erlang:system_time(millisecond).

ssb_encoder(Val, Encoder, Options) ->
    ssb_encoder1(Val, Encoder, Options, 0).

ssb_encoder1([], _Encoder, _Options, _Ind) ->
    [~"[]"];

ssb_encoder1([_|_] = V, Encoder, Options, Ind) when is_list(V) ->
    Pretty = lists:member(pretty, Options),
    Array = lists:map(fun(Elem) ->
                              if Pretty ->
                                      [~"\n", string:copies("  ", Ind + 1),
                                 ssb_encoder1(Elem, Encoder, Options, Ind + 1),
                                 ~","];
                                 true ->
                                      [ssb_encoder1(Elem, Encoder, Options, Ind + 1),
                                       ~","]
                              end
                      end, V),
    ArrayNoLast = lists:droplast(Array),
    FixElem = lists:droplast(lists:last(Array)),
    if Pretty ->
            [~"[", ArrayNoLast ++ [FixElem], ~"\n", string:copies("  ", Ind), ~"]"];
       true ->
            [~"[", ArrayNoLast ++ [FixElem], ~"]"]
    end;

ssb_encoder1({[]}, _Encoder, _Options, _Ind) ->
    [~"{}"];

    ssb_encoder1({KeyValList}, Encoder, Options, Ind) ->
        Pretty = lists:member(pretty, Options),
        case KeyValList of
            [] ->
                [~"{}"];
            _ ->
                Obj = lists:map(fun({_, _} = Val) -> ssb_encoder1(Val, Encoder, Options, Ind + 1) end,
                                KeyValList),
                LastElem = lists:last(Obj),
                ObjNoLast = lists:droplast(Obj),
                FixElem = lists:droplast(LastElem),
                if Pretty ->
                        [~"{", ObjNoLast ++ [FixElem], ~"\n", string:copies("  ", Ind), ~"}"];
                   true ->
                        [~"{", ObjNoLast ++ [FixElem], ~"}"]
                end
        end;

ssb_encoder1({Key, Val}, Encoder, Options, Ind) ->
    Pretty = lists:member(pretty, Options),
    if Pretty ->
            [~"\n", string:copies("  ", Ind), ssb_encoder1(Key, Encoder, Options, Ind), <<": ">>, ssb_encoder1(Val, Encoder, Options, Ind), <<",">>];
       true ->
            [ssb_encoder1(Key, Encoder, Options, Ind), ~":", ssb_encoder1(Val, Encoder, Options, Ind), <<",">>]
    end;

%% Strings are encoded here rather than by OTP's json module, which escapes
%% control characters with UPPERCASE hex (\u001A) where JavaScript's
%% JSON.stringify uses lowercase (\u001a).  Both are valid JSON and both
%% decode to the same string — but SSB signs and hashes the bytes of the
%% canonical encoding, so one byte of case difference makes a genuine
%% message fail its own signature check and hash to the wrong id.
%%
%% Exactly nine codepoints in the BMP differ: the control characters whose
%% escape contains a hex letter — U+000B, U+000E, U+000F, and U+001A
%% through U+001F.  Everything else agrees, non-ASCII is not escaped by
%% either, and astral characters pass through as UTF-8 on both sides.
ssb_encoder1(Bin, Encoder, _Options, _Ind) when is_binary(Bin) ->
    encode_string(Bin, Encoder);

ssb_encoder1(Other, Encoder, Options, _Ind) ->
    GoodAtom = is_atom(Other) andalso ((Other == null)
                                       orelse
                                       (Other == true)
                                       orelse
                                       (Other == false)),
    UseNil = lists:member(use_nil, Options) andalso (Other == nil),
    if UseNil ->
            json:encode_value(null, Encoder);
       true ->
            if is_atom(Other) andalso (not GoodAtom) ->
                    encode_string(atom_to_binary(Other), Encoder);
               true ->
                    json:encode_value(Other, Encoder)
            end
    end.

%% Invalid UTF-8 is handed back to json:encode_value so it raises exactly
%% the error it always did.  That symmetry is load-bearing: encode and
%% decode reject the same bytes, which is what makes a stored message that
%% cannot be re-encoded unreachable rather than silently corrupt.
encode_string(Bin, Encoder) ->
    case unicode:characters_to_binary(Bin, utf8, utf8) of
        Valid when is_binary(Valid) -> [$", escape(Valid, <<>>), $"];
        _NotUtf8                    -> json:encode_value(Bin, Encoder)
    end.

%% Byte-wise is correct here precisely because non-ASCII is never escaped:
%% every byte of a multi-byte UTF-8 sequence is >= 16#80 and falls to the
%% last clause untouched.
escape(<<>>, Acc) ->
    Acc;
escape(<<$", Rest/binary>>, Acc)  -> escape(Rest, <<Acc/binary, "\\\"">>);
escape(<<$\\, Rest/binary>>, Acc) -> escape(Rest, <<Acc/binary, "\\\\">>);
escape(<<8,  Rest/binary>>, Acc)  -> escape(Rest, <<Acc/binary, "\\b">>);
escape(<<9,  Rest/binary>>, Acc)  -> escape(Rest, <<Acc/binary, "\\t">>);
escape(<<10, Rest/binary>>, Acc)  -> escape(Rest, <<Acc/binary, "\\n">>);
escape(<<12, Rest/binary>>, Acc)  -> escape(Rest, <<Acc/binary, "\\f">>);
escape(<<13, Rest/binary>>, Acc)  -> escape(Rest, <<Acc/binary, "\\r">>);
escape(<<C,  Rest/binary>>, Acc) when C < 16#20 ->
    escape(Rest, <<Acc/binary, "\\u00", (hex(C bsr 4)), (hex(C band 15))>>);
escape(<<C,  Rest/binary>>, Acc) ->
    escape(Rest, <<Acc/binary, C>>).

hex(N) when N < 10 -> $0 + N;
hex(N)             -> $a + N - 10.


-ifdef(TEST).

roundtrip_test() ->
    {ok, Cwd} = file:get_cwd(),
    Ints = lists:seq(1,235),
    Fun = fun(N) ->
                  F = Cwd ++ "/testdata/" ++ io_lib:format("~5..0w",[N]) ++ ".full",
                  {ok, FilBin} = file:read_file(F),
                  encode(decode(FilBin, true)) == FilBin
          end,

    Results = lists:map(Fun, Ints),
    ?assert(lists:all(fun(B) when is_boolean(B) ->
                    B end, Results)).

ssb_test() ->
    O1 = {[{~"key1",~"val1"},{~"key2", [{[{~"skey1", ~"sval1"}]},{[{~"skey12", ~"sval2"}]}]}]},
    BO1 = iolist_to_binary(ssb_encoder(O1, fun ssb_encoder/3, [use_nil])),
    ?assert(O1 == utils:nat_decode(BO1)).

bad_msg_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "bad.full",
    {ok, FilBin} = file:read_file(F),
    ?assert(FilBin == encode(decode(FilBin, true))).

%% Control characters escape with LOWERCASE hex, as JSON.stringify does.
%%
%% OTP's json module uppercases them, and for the nine codepoints whose
%% escape contains a hex letter that is a one-byte difference in the
%% canonical form SSB signs and hashes over.  Every other control
%% character escapes to digits only, or to a short form, and agrees.
control_char_escaping_test() ->
    Enc = fun(CP) ->
                  iolist_to_binary(
                    ssb_encoder(unicode:characters_to_binary([CP], utf8),
                                fun ssb_encoder/3, [use_nil]))
          end,
    %% the nine that used to differ
    [?assertEqual(Expect, Enc(CP))
     || {CP, Expect} <- [{16#0B, ~"\"\\u000b\""}, {16#0E, ~"\"\\u000e\""},
                         {16#0F, ~"\"\\u000f\""}, {16#1A, ~"\"\\u001a\""},
                         {16#1B, ~"\"\\u001b\""}, {16#1C, ~"\"\\u001c\""},
                         {16#1D, ~"\"\\u001d\""}, {16#1E, ~"\"\\u001e\""},
                         {16#1F, ~"\"\\u001f\""}]],
    %% short forms and the digit-only escapes are unchanged
    [?assertEqual(Expect, Enc(CP))
     || {CP, Expect} <- [{16#08, ~"\"\\b\""}, {16#09, ~"\"\\t\""},
                         {16#0A, ~"\"\\n\""}, {16#0C, ~"\"\\f\""},
                         {16#0D, ~"\"\\r\""}, {16#00, ~"\"\\u0000\""},
                         {16#01, ~"\"\\u0001\""}, {16#19, ~"\"\\u0019\""}]],
    %% quote and backslash, and non-ASCII left alone (never \u-escaped)
    ?assertEqual(~"\"\\\"\"", Enc($")),
    ?assertEqual(~"\"\\\\\"", Enc($\\)),
    ?assertEqual(~"\"\x{202F}\"", Enc(16#202F)),
    ?assertEqual(~"\"\x{1F600}\"", Enc(16#1F600)).

%% Three real messages from feed @2h32wN… that erlbutt rejected in
%% production: genuine messages whose `vote.expression` (278, 287) or post
%% text (320) contains U+001A.  erlbutt re-encoded it as \u001A, so the
%% signature it checked was one byte from the one the author signed, and
%% the id it computed was not the id every other client had.
%%
%% Rejection wedged the feed: 278 failed, the tail stopped at 277, and the
%% peer re-offered it every few seconds for as long as the node ran.  Both
%% halves are asserted here — a fix that verified the signature but still
%% hashed the wrong id would leave the message unreachable by its own name.
control_char_real_messages_test() ->
    {ok, Cwd} = file:get_cwd(),
    Cases = [{278, ~"%/nDGVqr3W8CpA8OMxjapL5I5N1HV5KO61QoQq5s3vUY=.sha256"},
             {287, ~"%EcCR+i/sjab0OK0+yGJWp6q1hxs7UpaujJ0ELbIQXBE=.sha256"},
             {320, ~"%nX5A/A6DuSRPRlx0Q5yjWnZ68YHnaH6p4lWqhW6m6CY=.sha256"}],
    [begin
         F = Cwd ++ "/testdata/ctrl_" ++ integer_to_list(Seq) ++ ".value",
         {ok, Json} = file:read_file(F),
         Msg = decode_value(Json, true),
         ?assertEqual(Seq, Msg#message.sequence),
         ?assertEqual(true, Msg#message.validated),
         ?assertEqual(Id, Msg#message.id)
     end || {Seq, Id} <- Cases].

%% A real post (feed @ASFlv8..., seq 11) authored in a JS client whose
%% screenshot-filename mention contains U+202F (NARROW NO-BREAK SPACE, the
%% char macOS puts before "PM").  ssb-keys hashes the message id as latin1,
%% so U+202F contributes only its low byte (0x2F); erlbutt formerly hashed
%% UTF-8 and computed the wrong id (%qlb9...), which broke the vote/backlink
%% that referenced this post from other clients.  The canonical id is the
%% latin1 hash: %EWthyy...
non_ascii_message_id_test() ->
    NNBSP = <<16#202F/utf8>>,
    Name  = <<"Screenshot 2026-07-12 at 4.39.17", NNBSP/binary, "PM.png">>,
    Blob  = <<"&T0i/vYmhT1PNlqQIhj9DRsRbBmzfU+Onp7RaWhC31vg=.sha256">>,
    Value = iolist_to_binary(
        ["{\"previous\":\"%7Y5SyM7lLMciLPjjarhxlmR2SXNpN2JLXuU8RbRZsAk=.sha256\",",
         "\"author\":\"@ASFlv8MHXcuHeRMruDnUPZwMkFTx+t1fYvoP7xWkXRo=.ed25519\",",
         "\"sequence\":11,\"timestamp\":1784020809354,\"hash\":\"sha256\",",
         "\"content\":{\"type\":\"post\",",
         "\"root\":\"%KKkY5A/UZxg1o+ANFVJLaK9jmYTci2H42pvlVYmGNv0=.sha256\",",
         "\"branch\":\"%eN//hfJI0HwX/3p4dgTXPyGSBOeJpcrG6NUcZUpF9lo=.sha256\",",
         "\"reply\":{",
         "\"%KKkY5A/UZxg1o+ANFVJLaK9jmYTci2H42pvlVYmGNv0=.sha256\":",
         "\"@ASFlv8MHXcuHeRMruDnUPZwMkFTx+t1fYvoP7xWkXRo=.ed25519\",",
         "\"%eN//hfJI0HwX/3p4dgTXPyGSBOeJpcrG6NUcZUpF9lo=.sha256\":",
         "\"@PerULA9DgxWbudMHzEv9RuUmasdGwTUcRJSVGZAKt+Q=.ed25519\"},",
         "\"channel\":null,\"recps\":null,",
         "\"text\":\"once we can do attachments :)\\n\\n![", Name, "](", Blob, ")\\n\",",
         "\"mentions\":[{\"link\":\"", Blob, "\",\"name\":\"", Name,
         "\",\"type\":\"image/png\",\"size\":438087}]},",
         "\"signature\":\"jkEQX5BlyEJsEOjb9mudFghtRfmrIqmayrgkLpImV7ov1YnRQcrkEARhLRSRwcG1Lmx1X",
         "+p1k7/cqY3T2CV2Cg==.sig.ed25519\"}"]),
    #message{id = Id} = decode_value(Value, false),
    ?assertEqual(<<"%EWthyyv7gnqzXKx0PFjwuEaui1StcwVoQpo9fR1T8ak=.sha256">>, Id).


-endif.
