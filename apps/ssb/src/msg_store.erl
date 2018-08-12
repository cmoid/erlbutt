%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2018 Dionne Associates, LLC.
-module(msg_store).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ssb.hrl").

-export([persist/1,
         fetch_msg/1,
         fetch_msgs_author/1,
         fetch_latest_msg/1,
         fetch_history_feed/2]).

persist(Msg) ->
    F = fun() ->
                mnesia:write(Msg)
        end,
    Result = mnesia:sync_transaction(F),
    case Result of
        {aborted, Reason} ->
            ?info("We aborted with ~p ~n",[Reason]);
        {atomic, Answer} ->
            Answer
    end.

fetch_msg(Id) ->
    F = fun() ->
                mnesia:read(message, Id)
        end,
    Result = mnesia:transaction(F),

    case Result of
        {aborted, {no_exists, message}} ->
            not_exists;
        {atomic, [Msg]} ->
            Msg;
        _Else ->
            should_be_unique
    end.

fetch_msgs_author(Id) ->
    F = fun() ->
                mnesia:index_read(message, Id, #message.author)
        end,
    Result = mnesia:transaction(F),

    case Result of
        {abort, {no_exists, message}} ->
            not_exists;
        {atomic, Rest} ->
            Rest;
        _Else ->
            %% ?? Makes no sense
            should_be_unique
    end.

fetch_history_feed(Id, Seq) ->
    F = fun() ->
                mnesia:select(message,[{#message{author=Id, sequence='$1', _='_'},
                                        [{'>', '$1', Seq}], ['$_']}])
        end,
    Result = mnesia:transaction(F),

    case Result of
        {abort, {no_exists, message}} ->
            not_exists;
        {atomic, Rest} ->
            Rest;
        _Else ->
            %% ?? Makes no sense
            should_be_unique
    end.

%% not very performant, will need a better way to fetch tips of tangles
fetch_latest_msg(Id) ->
    Msgs = fetch_msgs_author(Id),
    case Msgs of
        not_exists ->
            nil;
        should_be_unique ->
            nil;
        _Else ->
            find_max(Msgs)
    end.

% this is pretty gross when you think about it
% should get the last message in the feed, regardless of sequence
% number as those could be a lie
find_max([]) ->
    nil;
find_max(Msgs) ->
    lists:foldl(fun(Rec, Max) ->
                        if Rec#message.sequence > Max#message.sequence
                           ->
                                Rec;
                           true ->
                                Max
                        end
                end,
                #message{sequence = 0}, Msgs).
