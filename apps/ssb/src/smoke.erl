%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(smoke).
-include_lib("ssb/include/ssb.hrl").

-export([secret_handshake/1,
        remote_long_pk/0,
        smoke/0,
        largest_feed/0,
        whoami/1]).

-import(proplists, [get_value/2,
                    get_value/3]).

-import(utils, [concat3/3,
                concat4/4,
                incr/1]).

secret_handshake(Host) ->
    {ok, NewSbotClient} = ssb_client:start_link(Host, remote_long_pk()),
    ?LOG_DEBUG("connection made ~n",[]),
    NewSbotClient.

remote_long_pk() ->
    base64:decode(keys:pub_key()).


smoke() ->
    NewClient = secret_handshake("localhost"),
    ?LOG_DEBUG("The new client pid ~p ~n", [NewClient]),
    ssb_client:send(NewClient, utils:ping_req()).

whoami(Peer) ->
    NewClient = secret_handshake(Peer),
    ?LOG_DEBUG("The new client pid ~p ~n", [NewClient]),
    ssb_client:send(NewClient, utils:whoami_req()).

largest_feed() ->
    Auths = mess_auth:all_auths(),
    NoAuths = length(Auths),
    check_feeds(Auths, 1, NoAuths, {<<"feeds">>,0}).

check_feeds(_Auths, N, NoAuths, Max) when N == NoAuths ->
    Max;
check_feeds(Auths, N, NoAuths, Max) ->
    {_Feed, Seq} = Max,
    Id = lists:nth(N, Auths),
    Pid = utils:find_or_create_feed_pid(Id),
    Mess = ssb_feed:fetch_last_msg(Pid),
    case Mess of
        done ->
            check_feeds(Auths, N + 1, NoAuths, Max);
        _Else ->
            #message{sequence = NewSeq} = Mess,
            NewMax = case Seq > NewSeq of
                         true ->
                             Max;
                         false ->
                             {Id, NewSeq}
                     end,
            check_feeds(Auths, N + 1, NoAuths, NewMax)
    end.
