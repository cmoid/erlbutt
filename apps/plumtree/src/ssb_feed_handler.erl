%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% plumtree_handler implementation for SSB feed replication.
%%
%% MsgId   = SSB message key, e.g. <<"%abc...=.sha256">>
%% Payload = JSON binary (the "value" object, same format as EBT sends)
-module(ssb_feed_handler).

-behaviour(plumtree_handler).

-include_lib("ssb/include/ssb.hrl").

-export([deliver/2, retrieve/1]).

%% Decode and store the incoming message in the appropriate feed.
deliver(MsgId, Payload) ->
    try
        Msg = message:decode_value(Payload, true),
        case utils:find_or_create_feed_pid(Msg#message.author) of
            bad ->
                ?SSB_INFO("plumtree: bad author in message ~s~n", [MsgId]);
            Pid ->
                ssb_feed:store_msg(Pid, Msg)
        end
    catch
        _:Reason ->
            ?SSB_INFO("plumtree: failed to deliver ~s: ~p~n", [MsgId, Reason])
    end.

%% Look up a stored message payload to answer a GRAFT request.
retrieve(MsgId) ->
    case mess_auth:get(MsgId) of
        not_found ->
            not_found;
        Author ->
            Pid     = utils:find_or_create_feed_pid(Author),
            Msg     = ssb_feed:fetch_msg(Pid, MsgId),
            Encoded = message:encode(Msg),
            {PropList} = utils:nat_decode(Encoded),
            Value   = proplists:get_value(~"value", PropList),
            Payload = iolist_to_binary(
                message:ssb_encoder(Value, fun message:ssb_encoder/3,
                                    [pretty, use_nil])),
            {ok, Payload}
    end.
