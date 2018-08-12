-module(packet).
-include_lib("ssb/include/ssb.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([secret_handshake/1,
        remote_long_pk/0,
        ping/0]).

-import(proplists, [get_value/2,
                    get_value/3]).

-import(utils, [concat3/3,
                concat4/4,
                incr/1]).

secret_handshake(Opts) ->
    Host = get_value(hostname, Opts, "localhost"),
    {ok, NewSbotClient} = sbot_client:start_link(Host, remote_long_pk()),
    sbot_client:send(NewSbotClient, ping()),
    ct:pal("OK, trivial message sent ~n",[]).

remote_long_pk() ->
    base64:decode(keys:pub_key()).

ping() ->
    Flags = rpc_processor:create_flags(1,0,2),
    Body = jiffy:encode({[{<<"name">>,[<<"gossip">>,<<"ping">>]},
                          {<<"args">>,[{[{<<"timeout">>, 300000}]}]},
                          {<<"type">>,<<"duplex">>}]}),
    Header = rpc_processor:create_header(Flags, size(Body), 1),
    utils:combine(Header, Body).
