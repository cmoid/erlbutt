#!/usr/bin/env escript
%%! -noshell -noinput
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et

-define(INFO(Fmt,Args), io:format(Fmt,Args)).

main([Host, Command]) ->
    code:add_path("./_build/default/lib/ssb/ebin/"),
    code:add_path("./_build/default/lib/enacl/ebin/"),
    code:add_path("./_build/default/lib/ranch/ebin/"),
    logger:set_primary_config(level, error),
    config:start_link(),
    keys:start_link(),
    ?INFO("current working directory is ~p ~n",[file:get_cwd()]),

   %% invoke the command passed as argument
    F = fun() ->
                NewClient = secret_handshake(Host),
                ?INFO("~p~n",[NewClient]),
                ?INFO("~s~n",[Command]),
                Req = case Command of
                             "whoami" ->
                                 utils:whoami_req();
                             "menu" ->
                                 menu_req();
                             "ping" ->
                                 utils:ping_req()
                         end,
                Resp = ssb_peer:send(NewClient, Req),
                ?INFO("~s~n",[Resp])
        end,
    F();

main(Args) ->
    ?INFO("unknown args: ~p\n", [Args]),
    erlang:halt(1).

secret_handshake(Host) ->
    {ok, NewSbotClient} = ssb_peer:start_link(Host, remote_long_pk()),
    NewSbotClient.

%%base64:decode(<<"ceHuIsvTt71cD5IsoXBlqwda8S+W9l5JKb5b89MbTo8=">>)

%%base64:decode(<<"LrNsx/3v3rBPk1zFkDp3V8mdsNQrcup8iu4FdymtFm0=">>)

%%base64:decode(<<"bombBa/UwB792ilEh7wXooqBSluIvzrJbrWzZAFhnxw=">>)

%%base64:decode(<<"aBkmLQLxnsJleW1LyyCrS3DA6a/Wfz57vIK321vRumc=">>)

%%base64:decode(<<"ceHuIsvTt71cD5IsoXBlqwda8S+W9l5JKb5b89MbTo8=">>)


remote_long_pk() ->
    base64:decode(keys:pub_key()).

menu_req() ->
    Flags = rpc_processor:create_flags(0,0,2),
    Body = iolist_to_binary(json:encode(maps:from_list([{<<"name">>,[<<"manifest">>]},
                          {<<"args">>,[]},
                          {<<"type">>,<<"sync">>}]))),
    Header = rpc_processor:create_header(Flags, size(Body), 1),
    utils:combine(Header, Body).
