%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(message).
-include("ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([decode/2,
         encode/1,
         ssb_encoder/3,
         nat_decode/1,
         is_follow/1,
         is_about/1,
         is_reply/1,
         is_branch/1,
         new_msg/4]).

is_follow(#message{content = Val}) when is_binary(Val) ->
    nope;

is_follow(#message{content = {ContentProps}}) ->
    Type = ?pgv(<<"type">>, ContentProps),
    case Type of
        <<"contact">> ->
            check_contact(?pgv(<<"contact">>,ContentProps),
                          ?pgv(<<"following">>, ContentProps));
        _Else ->
            nope
    end.

is_about(#message{content = Content}) when is_binary(Content) ->
    false;

is_about(#message{content = {ContentProps}}) ->
    Type = ?pgv(<<"type">>, ContentProps),
    case Type of
        undefined ->
            false;
        Type ->
            Type == <<"about">>
    end.

is_branch(#message{content = Content}) when is_binary(Content) ->
    false;

is_branch(#message{content = {Content}}) ->
    Root = ?pgv(<<"root">>, Content),
    Branch = ?pgv(<<"branch">>, Content),
    build_branch(Root, Branch).

build_branch(undefined, _Branch) ->
    false;
build_branch(_Root, undefined) ->
    false;
build_branch(false, _Branch) ->
    false;
build_branch(_Root, false) ->
    false;
build_branch(Root, Branch) when is_list(Branch) ->
    {Root, Branch};
build_branch(Root, Branch) ->
    {Root, [Branch]}.

is_reply(#message{content = Content}) when is_binary(Content) ->
    false;

is_reply(#message{content = {Content}}) ->
    build_reply(?pgv(<<"reply">>, Content)).

build_reply(undefined) ->
    false;

build_reply(Reply) ->
    Reply.

check_contact(undefined, _Following) ->
    nope;
check_contact(<<>>, _Following) ->
    nope;
check_contact(Contact, true) ->
    {Contact, true};
check_contact(Contact, false) ->
    {Contact, false};
check_contact(_Contact, _Following) ->
    nope.

new_msg(Previous, Sequence, Content, {PubKey, PrivKey}) ->
    Timestamp = integer_to_binary(current_time()),
    Hash = <<"sha256">>,
    NewMsg = #message{previous = Previous,
                      author = PubKey,
                      sequence = Sequence,
                      timestamp = Timestamp,
                      hash = Hash,
                      content = Content},
    EncNewMsg = jiffy:encode({msg_to_proplist(NewMsg)},
                             [pretty, use_nil]),
    Sig = enacl:sign_detached(EncNewMsg,
                              base64:decode(PrivKey)),
    EncSig = ?l2b(utils:base_64(Sig) ++ ".sig.ed25519"),
    %% Now add the sig to original msg
    add_sig(NewMsg, EncSig).

encode(#message{id = Key, received = Received, swapped = Swapped} = Msg) ->
    MsgProps = msg_to_proplist(Msg),
    EncMsg = build_props(MsgProps, Swapped),

    iolist_to_binary(jiffy:encode({[{<<"key">>, Key},
                   {<<"value">>, {EncMsg}},
                   {<<"timestamp">>, Received}]}, [use_nil, force_utf8])).

decode(Msg, CheckValid) ->
    {DecDataProps} = jiffy:decode(Msg),
    Key = ?pgv(<<"key">>, DecDataProps),
    {Value} = ?pgv(<<"value">>, DecDataProps),
    IsSwapped = is_swapped(Value),

    IsValid = validate(CheckValid, Value),
    #message{id = Key,
             previous = ?pgv(<<"previous">>, Value),
             author = ?pgv(<<"author">>, Value),
             sequence = ?pgv(<<"sequence">>, Value),
             timestamp = ?pgv(<<"timestamp">>, Value),
             hash = ?pgv(<<"hash">>, Value),
             content = ?pgv(<<"content">>, Value),
             signature = ?pgv(<<"signature">>, Value),
             received = ?pgv(<<"timestamp">>, DecDataProps),
             validated = IsValid,
             swapped = IsSwapped}.

is_swapped(PropList) ->
    element(1, lists:nth(2, PropList)) == <<"sequence">>.

build_props(Props, Swapped) ->
    [{<<"previous">>, ?pgv(<<"previous">>, Props)}] ++
        check_swapped(Props, Swapped) ++
        [{<<"timestamp">>, ?pgv(<<"timestamp">>, Props)},
         {<<"hash">>, ?pgv(<<"hash">>, Props)},
         {<<"content">>, ?pgv(<<"content">>, Props)},
         {<<"signature">>, ?pgv(<<"signature">>, Props)}].

check_swapped(Props, Swapped) ->
    Seq = {<<"sequence">>, ?pgv(<<"sequence">>, Props)},
    Auth = {<<"author">>, ?pgv(<<"author">>, Props)},
    if Swapped ->
            [Seq, Auth];
       true ->
            [Auth, Seq]
    end.

%% Internal functions
nat_decode(Msg) ->
    {Json, _, _} = json:decode(Msg,[], #{object_finish =>
                              fun(Acc,OldAcc) ->
                                      {{lists:reverse(Acc)}, OldAcc} end}),
    Json.

validate(false, _MsgProps) ->
    false;
validate(true, MsgProps) ->
    try
        Author = ?pgv(<<"author">>, MsgProps),

        %% remove signature from message and encode as json
        DelSigProps = proplists:delete(<<"signature">>, MsgProps),
        EncMsg = jiffy:encode({DelSigProps}, [pretty, force_utf8]),

        %% extract and decode the keys for the signature and the author
        Sig = ?pgv(<<"signature">>, MsgProps),
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
        [{<<"signature">>, EncSig}],
    %% added sig to msg before computing id
    MsgId = compute_id(jiffy:encode({NewMsgList},
                            [pretty,
                             use_nil])),
    NewMsg#message{id = MsgId,
                   signature = EncSig}.

msg_to_proplist(Msg) ->
    lists:zip(lists:map(fun(A) ->
                                atom_to_binary(A, utf8) end,
                        record_info(fields, message)),
              tl(tuple_to_list(Msg))).

compute_id(Msg) ->
    ?l2b("%" ++
        utils:base_64(crypto:hash(sha256, Msg))
        ++
        ".sha256").

current_time() ->
    erlang:system_time(millisecond).

ssb_encoder([_|_] = V, Encoder, Options) ->
    json:encode_list(V, fun(Elem, _Enc) ->
                                ssb_encoder(Elem, Encoder, Options) end);

ssb_encoder({KeyValList}, Encoder, Options) ->
    Obj = lists:map(fun({_, _} = Val) -> ssb_encoder(Val, Encoder, Options) end,
                    KeyValList),
    LastElem = lists:last(Obj),
    ObjNoLast = lists:reverse(tl(lists:reverse(Obj))),
    FixElem = lists:reverse(tl(lists:reverse(LastElem))),
    [<<"{">>, ObjNoLast ++ [FixElem], <<"}">>];

ssb_encoder({Key, Val}, Encoder, Options) ->
    Pretty = lists:member(pretty, Options),
    if Pretty ->
            [<<"\n  ">>, ssb_encoder(Key, Encoder, Options), <<": ">>, ssb_encoder(Val, Encoder, Options), <<",">>];
       true ->
            [ssb_encoder(Key, Encoder, Options), <<":">>, ssb_encoder(Val, Encoder, Options), <<",">>]
    end;

ssb_encoder(Other, Encoder, Options) ->
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
    ?assert(lists:all(fun(B) ->
                    B end, Results)).



bad_msg_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "bad.full",
    {ok, FilBin} = file:read_file(F),
    ?assert(FilBin == encode(decode(FilBin, true))).

is_about_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "about.full",
    {ok, FilBin} = file:read_file(F),

    ?assert(message:is_about(decode(FilBin, true))).

is_reply_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "reply.full",
    {ok, FilBin} = file:read_file(F),

    ?assert(message:is_reply(decode(FilBin, true)) /= false).

is_multi_reply_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "mult-reply.full",
    {ok, FilBin} = file:read_file(F),
    Msg = decode(FilBin, true),
    {ListPairs} = message:is_reply(Msg),
    ?assert(length(ListPairs) == 2).


is_not_reply_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "not_reply.full",
    {ok, FilBin} = file:read_file(F),

    ?assert(not message:is_reply(decode(FilBin, true))).

is_single_branch_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "single-branch.full",
    {ok, FilBin} = file:read_file(F),
    {_Root, Branches} = message:is_branch(decode(FilBin, true)),
    ?assert(is_list(Branches) andalso length(Branches) == 1).

is_multi_branch_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "multi-branch.full",
    {ok, FilBin} = file:read_file(F),
    {_Root, Branches} = message:is_branch(decode(FilBin, true)),
    ?assert(is_list(Branches) andalso length(Branches) == 2).

follow_true_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "follow_true.full",
    {ok, FilBin} = file:read_file(F),
    {_FeedId, Bool} = message:is_follow(decode(FilBin, true)),

    ?assert(Bool).

follow_false_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "follow_false.full",
    {ok, FilBin} = file:read_file(F),
    {_FeedId, Bool} = message:is_follow(decode(FilBin, true)),

    ?assert(not Bool).

-endif.
