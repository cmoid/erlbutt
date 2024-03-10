#!/usr/bin/env escript
%%! -noshell -noinput
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et

-define(INFO(Fmt,Args), io:format(Fmt,Args)).

main([Command]) ->
    code:add_path("./_build/default/lib/jiffy/ebin/"),
    code:add_path("./_build/default/lib/ssb/ebin/"),
    code:add_path("./_build/default/lib/enacl/ebin/"),
    code:add_path("./_build/default/lib/ranch/ebin/"),
    code:add_path("./_build/default/lib/bitcask/ebin/"),
    logger:set_primary_config(level, error),
    keys:start_link(),
    config:start_link(),

   %% invoke the command passed as argument
    F = fun() ->
                NewClient = secret_handshake("10.0.0.159"),
                ?INFO("~p~n",[NewClient]),
                ?INFO("~s~n",[Command]),
                Req = case Command of
                             "whoami" ->
                                 whoami_req();
                             "menu" ->
                                 menu_req();
                             "ping" ->
                                 ping_req()
                         end,
                Resp = ssb_peer:send(NewClient, Req),
                ?INFO("~s~n",[Resp])
        end,
    F();

main(Args) ->
    ?INFO("unknown args: ~p\n", [Args]),
    erlang:halt(1).

secret_handshake(Host) ->
    {ok, NewSbotClient} = ssb_peer:start_link(Host, base64:decode(<<"ceHuIsvTt71cD5IsoXBlqwda8S+W9l5JKb5b89MbTo8=">>)),
    NewSbotClient.

%%base64:decode(<<"LrNsx/3v3rBPk1zFkDp3V8mdsNQrcup8iu4FdymtFm0=">>)

%%base64:decode(<<"bombBa/UwB792ilEh7wXooqBSluIvzrJbrWzZAFhnxw=">>)

%%base64:decode(<<"aBkmLQLxnsJleW1LyyCrS3DA6a/Wfz57vIK321vRumc=">>)

%%base64:decode(<<"ceHuIsvTt71cD5IsoXBlqwda8S+W9l5JKb5b89MbTo8=">>)


remote_long_pk() ->
    base64:decode(keys:pub_key()).

whoami_req() ->
    Flags = rpc_processor:create_flags(0,0,2),
    Body = jiffy:encode({[{<<"name">>,[<<"whoami">>]},
                          {<<"args">>,[]},
                          {<<"type">>,<<"sync">>}]}),
    Header = rpc_processor:create_header(Flags, size(Body), 1),
    utils:combine(Header, Body).

menu_req() ->
    Flags = rpc_processor:create_flags(0,0,2),
    Body = jiffy:encode({[{<<"name">>,[<<"manifest">>]},
                          {<<"args">>,[]},
                          {<<"type">>,<<"sync">>}]}),
    Header = rpc_processor:create_header(Flags, size(Body), 1),
    utils:combine(Header, Body).

ping_req() ->
    Flags = rpc_processor:create_flags(1,0,2),
    Body = jiffy:encode({[{<<"name">>,[<<"ping">>]},
                          {<<"args">>,[{[{<<"timeout">>, 30}]}]},
                          {<<"type">>,<<"duplex">>}]}),
    Header = rpc_processor:create_header(Flags, size(Body), 1),
    utils:combine(Header, Body).
