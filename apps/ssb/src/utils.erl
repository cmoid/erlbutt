%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(utils).
-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type input() :: tuple() | binary().

-export([create_key_pair/0,
         base_64/1,
         display_pub/1,
         decode_id/1,
         encode_rec/1,
         concat/1,
         incr/1,
         combine/2,
         send_data/4,
         load_term/1,
         find_or_create_feed_pid/1,
         check_id/1,
         update_refs/1,
         log/1,
         ping_req/0,
         whoami_req/0,
         error_msg/2,
         nat_decode/1,
         size/1]).

-doc "Calculate the size of an input, suggested by elp".
-spec size(input()) -> non_neg_integer().
size(Input) when is_tuple(Input) ->
    erlang:tuple_size(Input);
size(Input) when is_binary(Input) ->
    erlang:byte_size(Input).

nat_decode(Msg) ->
    {Json, _, _} = json:decode(Msg,[], #{object_finish =>
                              fun(Acc,OldAcc) ->
                                      {{lists:reverse(Acc)}, OldAcc} end}),
    Json.


create_key_pair() ->
    {Pub, Priv} = shs:create_long_pair(),
    {?l2b(base_64(Pub)), ?l2b(base_64(Priv))}.

base_64(Binary) ->
    base64:encode_to_string(Binary).

display_pub(PubKey) ->
    ?l2b("@" ++ ?b2l(PubKey) ++ ".ed25519").

decode_id(<<"@",RemId/binary>>) ->
    RawId = hd(string:replace(RemId,".ed25519","")),
    decode_id1(RawId);

decode_id(<<"%",RemId/binary>>) ->
    RawId = hd(string:replace(RemId,".sha256","")),
    decode_id1(RawId);

decode_id(<<"&",RemId/binary>>) ->
    RawId = hd(string:replace(RemId,".sha256","")),
    decode_id1(RawId).

decode_id1(RawId) ->
    integer_to_binary(binary:decode_unsigned(base64:decode(RawId)),16).

concat(ListOfBins) ->
    iolist_to_binary(ListOfBins).

incr(Nonce) ->
    binary:encode_unsigned(binary:decode_unsigned(Nonce) + 1).

combine(nil, Bin) ->
    Bin;

combine(Bin1, Bin2) ->
    Bin1Len = utils:size(Bin1),
    <<Bin1:Bin1Len/binary, Bin2/binary>>.

send_data(Data, Socket, Nonce, SecretBoxKey) ->

    {EncBox, NewNonce} =
        boxstream:box(Data, Nonce, SecretBoxKey),

    gen_tcp:send(Socket, EncBox),

    NewNonce.

%% Specific to the SSB feed format, each message is prefixed with a 4 byte length
%% field and followed by the same length field. This enables on to check that a message
%% was correctly read.
load_term(IoDev) ->
    case file:read(IoDev, 4) of
        {ok, <<TermLenInt:32/integer>>} ->
            case file:read(IoDev, TermLenInt) of
                {ok, TermData} ->
                    check_data(IoDev, TermData, TermLenInt);
                {error, Reason} ->
                    {error, Reason}
            end;
        eof ->
            {error, eof};
        {error, Reason} ->
            {error, Reason}
    end.

check_data(IoDev, Data, Len) ->
    case file:read(IoDev, 4) of
        {ok, TermLen} ->
            <<TermLenInt:32/integer>> = TermLen,
            %% the length of the term is also stored at the end of the term
            %% and can be used to check
            if TermLenInt == Len ->
                    {ok, Data};
               true ->
                    {error, data_size_no_match}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

find_or_create_feed_pid(Id) ->
    %% ugh, using process dictionary for global state :(
    case check_id(Id) of
        bad ->
            bad;
        ok ->

            Val = get(Id),
            case Val of
                undefined ->
                    {ok, Pid} = ssb_feed:start_link(Id),
                    put(Id, Pid),
                    Pid;
                Pid when is_pid(Pid) ->
                    Alive = is_process_alive(Pid),
                    if Alive ->
                            Pid;
                       true ->
                            erase(Id),
                            find_or_create_feed_pid(Id)
                    end;
                _Else ->
                    erase(Id),
                    find_or_create_feed_pid(Id)
            end
    end.

update_refs(#message{id = Id, author = AuthId} = Msg) ->
    Branches = social_msg:is_branch(Msg),
    case Branches of
        false ->
            none;
        {Root, BranchList} ->
            Target = {~"tar", [Id, AuthId]},
            lists:map(fun(Bi) ->
                              %% this is really ugly, branch most often is either a binary
                              %% or a list of such, but occasionally it's an
                              %% object like {<<"0">>:<<"%msgid....">>}
                              %% this is the price one pays for immutable feeds, garbage
                              %% lasts forever
                              Ai = if is_binary(Bi) ->
                                           mess_auth:get(Bi);
                                      true ->
                                           not_found
                                   end,
                              case Ai of
                                  not_found ->
                                      nop;
                                  _Else ->
                                      Record = {[{~"root", Root},
                                                 {~"src", [Bi, Ai]},
                                                 Target]},
                                      Pid = find_or_create_feed_pid(Ai),
                                      ssb_feed:store_ref(Pid, encode_rec(Record))
                              end
                      end, BranchList)
    end.

encode_rec(Record) ->
    iolist_to_binary(message:ssb_encoder(Record, fun message:ssb_encoder/3, [])).

check_id(<<"@",Id/binary>>) ->
        try
            case binary:matches(Id,[~".ed25519"]) of
                [] ->
                    bad;
                _Else ->
                    RawId = hd(string:replace(Id,".ed25519","")),
                    _DecodedId = integer_to_binary(binary:decode_unsigned(base64:decode(?b2l(RawId))),16),
                    ok
            end
        catch
            error:_Reason ->
                bad
        end;
check_id(_Else) ->
    bad.

ping_req() ->
    Flags = rpc_processor:create_flags(1,0,2),
    Body = encode_rec({[{~"name",[~"gossip",~"ping"]},
                          {~"args",[{[{~"timeout", 300000}]}]},
                          {~"type",~"duplex"}]}),
    Header = rpc_processor:create_header(Flags, utils:size(Body), 1),
    utils:combine(Header, Body).

whoami_req() ->
    Flags = rpc_processor:create_flags(1,0,2),
    Body = encode_rec({[{~"name",[?whoami]},
                          {~"args",[]},
                          {~"type",~"sync"}]}),
    Header = rpc_processor:create_header(Flags, utils:size(Body), 1),
    utils:combine(Header, Body).

error_msg(Name, Mess) ->
    encode_rec({[{~"name", Name},
                 {~"message", Mess},
                 {~"stack", ~"_"}
                 ]}).




log({Socket, Data}) ->
    ?LOG_DEBUG("received a tcp packet of size: ~p ~n on socket ~p ~n",
           [utils:size(Data), Socket]);

log(Info) ->
    ?LOG_DEBUG("received random info message ~p ~n",
           [Info]).

-ifdef(TEST).

combine_test() ->
    ~"foo" = combine(nil, ~"foo"),
    ~"foobar" = combine(~"foo",~"bar").

create_pid_test() ->
    erase(),
    config:start_link("test/ssb.cfg"),
    keys:start_link(),
    {Pub, _Priv} = create_key_pair(),
    FeedId1 = display_pub(Pub),
    find_or_create_feed_pid(FeedId1),
    {Pub2, _Priv2} = create_key_pair(),
    FeedId2 = display_pub(Pub2),
    find_or_create_feed_pid(FeedId2),
    ?assert(length(get()) == 2).

create_pid_kill_test() ->
    erase(),
    config:start_link("test/ssb.cfg"),
    keys:start_link(),
    {Pub, _Priv} = create_key_pair(),
    FeedId1 = display_pub(Pub),
    find_or_create_feed_pid(FeedId1),
    {Pub2, _Priv2} = create_key_pair(),
    FeedId2 = display_pub(Pub2),
    find_or_create_feed_pid(FeedId2),

    ?assert(length(get()) == 2),
    erase(FeedId2),
    ?assert(length(get()) == 1),
    find_or_create_feed_pid(FeedId2),
    ?assert(length(get()) == 2).

simple_blob_decode_test() ->
    Coded = <<"&ybENuaMAdmfjmwR852FNDsj3biaMl5P4HF/jJj7OtQQ=.sha256">>,
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "b10db9a3007667e39b047ce7614d0ec8f76e268c9793f81c5fe3263eceb504",
    {ok, Blob} = file:read_file(F),
    CompCoded = list_to_binary("&" ++
                         utils:base_64(crypto:hash(sha256, Blob))
                     ++
                         ".sha256"),
    ?assert(CompCoded == Coded).








-endif.
