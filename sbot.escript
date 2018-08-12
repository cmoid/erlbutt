#!/usr/bin/env escript
%%! -noshell -noinput
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et

-define(INFO(Fmt,Args), io:format(Fmt,Args)).

main([Command | CommandArgs]) ->
    code:add_path("./_build/default/lib/jiffy/ebin/"),
    code:add_path("./_build/default/lib/ssb/ebin/"),
    code:add_path("./_build/default/lib/enacl/ebin/"),
    code:add_path("./_build/default/lib/ranch/ebin/"),
    code:add_path("./_build/default/lib/bitcask/ebin/"),
    logger:set_primary_config(level, debug),
    keys:start_link(),
    config:start_link(),

    Args = parse_arguments(CommandArgs),
    %% invoke the command passed as argument
    F = fun(_Args) ->
                NewClient = secret_handshake("10.0.0.251"),
                ?INFO("~p~n",[NewClient]),
                ?INFO("~s~n",[Command]),
                Req = case Command of
                             "whoami" ->
                                 utils:whoami_req();
                             "ping" ->
                                 utils:ping_req()
                         end,
                Resp = ssb_client:send(NewClient, Req),
                ?INFO("~s~n",[Resp])
        end,
    F(Args);

main(Args) ->
    ?INFO("unknown args: ~p\n", [Args]),
    erlang:halt(1).

parse_arguments(Args) ->
    parse_arguments(Args, []).

parse_arguments([], Acc) -> Acc;

parse_arguments(Else, _Acc) ->
    ?INFO("Args not supported yet ~p \n",
          [Else]).




secret_handshake(Host) ->
    {ok, NewSbotClient} = ssb_client:start_link(Host, remote_long_pk()),
    NewSbotClient.

%%base64:decode(<<"LrNsx/3v3rBPk1zFkDp3V8mdsNQrcup8iu4FdymtFm0=">>)

%%base64:decode(<<"bombBa/UwB792ilEh7wXooqBSluIvzrJbrWzZAFhnxw=">>)


remote_long_pk() ->
    base64:decode(keys:pub_key()).
