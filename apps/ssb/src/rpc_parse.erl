%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2018 Dionne Associates, LLC.
-module(rpc_parse).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-import(rpc_processor, [create_flags/3,
                        create_header/3]).

-endif.

-include("ssb.hrl").

%% API
-export([parse/1]).

%%%===================================================================
%%% API
%%%===================================================================

parse(Data) ->
    EnoughForHeader = size(Data) >= 9,
    case EnoughForHeader of
        true ->
            <<Header:9/binary,
              Rest/binary>> = Data,
            %% may also have enough data for the body
            parseBody(Header, Rest, Data);
        _Else ->
            {partial, nil, Data}
    end.

parseBody(Header, Rest, OrigData) ->
    BodySize = body_size(Header),
    HaveBodySize = BodySize =< size(Rest),
    case HaveBodySize of
        true ->
            <<Body:BodySize/binary,
              RestRest/binary>> = Rest,
            {complete, {Header, Body}, RestRest};
        _ ->
            {partial, nil, OrigData}
    end.

body_size(Header) ->
    <<_Flags:1/binary,
      BodySize:4/big-unsigned-integer-unit:8,
      _ReqNo:4/big-signed-integer-unit:8>> = Header,
    BodySize.

-ifdef(TEST).
simple_test() ->
    Flags = create_flags(1,0,2),
    Header = create_header(Flags, 0, 1),
    ?assert(parse(utils:combine(Header, <<>>)) ==
                {complete,{<<10,0,0,0,0,0,0,0,1>>,<<>>},<<>>}).

no_body_test() ->
    Flags = create_flags(1,0,2),
    Header = create_header(Flags, 6, 1),
    ?assert(parse(utils:combine(Header, <<"true">>)) ==
                {partial,nil,utils:combine(Header, <<"true">>)}).

no_header_test() ->
    Header = <<10,0,0,0,0,0,0,0>>,
    ?assert(parse(Header) ==
                {partial,nil,Header}).

full_test() ->
    Flags = create_flags(1,0,2),
    Header = create_header(Flags, 4, 1),
    ?assert(parse(utils:combine(Header, <<"true">>)) ==
                {complete,{<<10,0,0,0,4,0,0,0,1>>,<<"true">>},<<>>}).

not_enough_test() ->
    Flags = create_flags(1,0,2),
    Header = create_header(Flags, 12, 1),
    ?assert(parse(utils:combine(Header, <<"trueorfalse">>)) ==
                {partial,nil,utils:combine(Header, <<"trueorfalse">>)}).

over_test() ->
    Flags = create_flags(1,0,2),
    Header = create_header(Flags, 4, 1),
    ?assert(parse(utils:combine(Header, <<"trueorfalse">>)) ==
                {complete,{<<10,0,0,0,4,0,0,0,1>>,<<"true">>},<<"orfalse">>}).

-endif.
