%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2018 Dionne Associates, LLC.
-module(message).
-include("ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([build_msg/1,
        encn_store/3]).

encn_store(Previous, Sequence, Msg) ->
    Author = keys:pub_key(),
    Timestamp = integer_to_binary(current_time()),
    Hash = <<"sha256">>,
    NewMsg = #message{previous = Previous,
                      author = Author,
                      sequence = Sequence,
                      timestamp = Timestamp,
                      hash = Hash,
                      content = Msg},
    EncNewMsg = jiffy:encode({lists:zip(lists:map(fun(A) ->
                                                         atom_to_binary(A, utf8) end,
                                                 record_info(fields, message)),
                                       tl(tuple_to_list(NewMsg)))},
                             [pretty, use_nil]),
    Sig = enacl:sign_detached(EncNewMsg,
                              long_sk()),
    EncSig = ?l2b(base_64(Sig) ++ ".sig.ed25519"),
    %% Now add the sig to original msg
    NewMsgSig = add_sig(NewMsg, EncSig),
    NewMsgSig.

% Build a message record, checking that it's valid based on
% the signature
build_msg(DecDataProps) ->
    Key = ?pgv(<<"key">>, DecDataProps),
    {Value} = ?pgv(<<"value">>, DecDataProps),

    validate(Key, Value),

    #message{id = Key,
             previous = ?pgv(<<"previous">>, Value),
             author = ?pgv(<<"author">>, Value),
             sequence = ?pgv(<<"sequence">>, Value),
             timestamp = ?pgv(<<"timestamp">>, Value),
             hash = ?pgv(<<"hash">>, Value),
             content = ?pgv(<<"content">>, Value),
             signature = ?pgv(<<"signature">>, Value)}.

%% Internal functions

validate(Key, MsgProps) ->
    Author = ?pgv(<<"author">>, MsgProps),

    %% remove signature from message and encode as json
    DelSigProps = proplists:delete(<<"signature">>, MsgProps),
    EncMsg = jiffy:encode(js_order(DelSigProps), [pretty, force_utf8]),

    %% extract and decode the keys for the signature and the author
    Sig = ?pgv(<<"signature">>, MsgProps),
    <<"@",KeySuf/binary>> = Author,
    AuthorPk = base64:decode(hd(string:replace(KeySuf,".ed25519",""))),
    SigDec = base64:decode(hd(string:replace(Sig,".sig.ed25519",""))),

    %% verify
    Result = enacl:sign_verify_detached(SigDec, EncMsg, AuthorPk),

    case Result of
        true ->
            ?info("This is a valid msg ~p ~n",[Key]);
        false ->
            %% assuming we only want to store valid messages, currently
            %% we have 11 out of 1.2M messages that are invalid. The culprit
            %% is most likely UTF-8 issues from JS land. Will revisit later
            %% and if need be store a validity boolean in the message mnesia
            %% records.
            ?info("This failed to validate ~p ~n",[{Key, DelSigProps, EncMsg}])
    end.

% scuttlebutt protocol requires json keys be in a specific order for
% message signing, this order is based on the javascript implementation
% sometimes author and sequence are swapped, kind of gross :)
js_order(DelSigProps) ->
    Swap = element(1, lists:nth(2, DelSigProps)) == <<"sequence">>,
    if Swap ->
            {[{<<"previous">>, ?pgv(<<"previous">>, DelSigProps)},
              {<<"sequence">>, ?pgv(<<"sequence">>, DelSigProps)},
              {<<"author">>, ?pgv(<<"author">>, DelSigProps)},
              {<<"timestamp">>, ?pgv(<<"timestamp">>, DelSigProps)},
              {<<"hash">>, ?pgv(<<"hash">>, DelSigProps)},
              {<<"content">>, ?pgv(<<"content">>, DelSigProps)}]};
       true ->
            {[{<<"previous">>, ?pgv(<<"previous">>, DelSigProps)},
              {<<"author">>, ?pgv(<<"author">>, DelSigProps)},
              {<<"sequence">>, ?pgv(<<"sequence">>, DelSigProps)},
              {<<"timestamp">>, ?pgv(<<"timestamp">>, DelSigProps)},
              {<<"hash">>, ?pgv(<<"hash">>, DelSigProps)},
              {<<"content">>, ?pgv(<<"content">>, DelSigProps)}]}
    end.

add_sig(NewMsg, EncSig) ->
    ?info("The new mess is: ~p ~n",[NewMsg]),

    NewMsgList = lists:zip(lists:map(fun(A) ->
                                             atom_to_binary(A, utf8) end,
                                     record_info(fields, message)),
                           tl(tuple_to_list(NewMsg))) ++
        [{<<"signature">>, EncSig}],
    MsgId = compute_id(jiffy:encode({NewMsgList},
                            [pretty,
                             use_nil])),
    NewMsg#message{id = MsgId,
                   signature = EncSig}.

compute_id(Msg) ->
    "%" ++
        base_64(crypto:hash(sha256, Msg))
        ++
        ".sha256".

long_sk() ->
    base64:decode(keys:priv_key()).

base_64(Binary) ->
    base64:encode_to_string(Binary).

current_time() ->
    erlang:system_time(millisecond).
