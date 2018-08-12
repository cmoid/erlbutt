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
         encode/1,
         ssb_encoder/3,
         new_msg/4]).


new_msg(Previous, Sequence, Content, {PubKey, PrivKey}) ->
    Timestamp = integer_to_binary(current_time()),
    Hash = ~"sha256",
    NewMsg = #message{previous = Previous,
                      author = PubKey,
                      sequence = Sequence,
                      timestamp = Timestamp,
                      hash = Hash,
                      content = Content,
                      received = Timestamp},
    EncNewMsg = ssb_encoder({msg_to_proplist(NewMsg)},
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

validate(false, _MsgProps) ->
    false;
validate(true, MsgProps) ->
    try
        Author = ?pgv(~"author", MsgProps),

        %% remove signature from message and encode as json
        DelSigProps = proplists:delete(~"signature", MsgProps),
        EncMsg = ssb_encoder({DelSigProps}, fun ssb_encoder/3, [pretty, use_nil]),

        %% extract and decode the keys for the signature and the author
        Sig = ?pgv(~"signature", MsgProps),
        <<"@",KeySuf/binary>> = Author,
        AuthorPk = base64:decode(hd(string:replace(KeySuf,".ed25519",""))),
        SigDec = base64:decode(hd(string:replace(Sig,".sig.ed25519",""))),

        %% verify
        enacl:sign_verify_detached(SigDec, EncMsg, AuthorPk)
    catch
        error:Reason ->
            ?LOG_INFO("Unable to validate due to: ~p ~n",
                  [Reason]),
            false
    end.

add_sig(NewMsg, EncSig) ->
    NewMsgList = msg_to_proplist(NewMsg) ++
        [{~"signature", EncSig}],
    %% added sig to msg before computing id
    MsgId = compute_id(ssb_encoder({NewMsgList}, fun ssb_encoder/3,
                            [pretty,
                             use_nil])),
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

compute_id(Msg) ->
    ?l2b("%" ++
        utils:base_64(crypto:hash(sha256, Msg))
        ++
        ".sha256").

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
    LastElem = lists:last(Array),
    ArrayNoLast = lists:reverse(tl(lists:reverse(Array))),
    FixElem = lists:reverse(tl(lists:reverse(LastElem))),
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
                ObjNoLast = lists:reverse(tl(lists:reverse(Obj))),
                FixElem = lists:reverse(tl(lists:reverse(LastElem))),
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
                    json:encode_value(atom_to_binary(Other), Encoder);
               true ->
                    json:encode_value(Other, Encoder)
            end
    end.


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


-endif.
