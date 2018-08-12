-module(smoke).
-include_lib("ssb/include/ssb.hrl").

-export([secret_handshake/1,
        remote_long_pk/0,
        ping/0,
        smoke/0,
        whoami/1]).

-import(proplists, [get_value/2,
                    get_value/3]).

-import(utils, [concat3/3,
                concat4/4,
                incr/1]).

secret_handshake(Host) ->
    {ok, NewSbotClient} = sbot_client:start_link(Host, remote_long_pk()),
    ?debug("connection made ~n",[]),
    NewSbotClient.

remote_long_pk() ->
    base64:decode(keys:pub_key()).

ping() ->
    Flags = rpc_processor:create_flags(1,0,2),
    Body = jiffy:encode({[{<<"name">>,[<<"gossip">>,<<"ping">>]},
                          {<<"args">>,[{[{<<"timeout">>, 300000}]}]},
                          {<<"type">>,<<"duplex">>}]}),
    Header = rpc_processor:create_header(Flags, size(Body), 1),
    utils:combine(Header, Body).

whoami_req() ->
    Flags = rpc_processor:create_flags(1,0,2),
    Body = jiffy:encode({[{<<"name">>,[?whoami]},
                          {<<"args">>,[]},
                          {<<"type">>,<<"async">>}]}),
    Header = rpc_processor:create_header(Flags, size(Body), 1),
    utils:combine(Header, Body).

smoke() ->
    NewClient = secret_handshake("localhost"),
    ?debug("The new client pid ~p ~n", [NewClient]),
    sbot_client:send(NewClient, ping()).

whoami(Peer) ->
    NewClient = secret_handshake(Peer),
    ?debug("The new client pid ~p ~n", [NewClient]),
    sbot_client:send(NewClient, whoami_req()).
