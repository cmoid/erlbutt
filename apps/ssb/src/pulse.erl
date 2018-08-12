%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2018 Dionne Associates, LLC.
-module(pulse).

-include("ssb.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([whoami/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket,
                clients = ets:new(sbot_clients, [set, named_table]),
               timer}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    Resp = gen_udp:open(8008,
                         [binary,
                          {broadcast, true},
                          {active,true}]),
    case Resp of
        {ok, Socket} ->
            {ok, TRef} = timer:send_after(1000, advertise),
            {ok, #state{socket = Socket,
                        timer = TRef}};
        {error, Reason} ->
            ?debug("Why can't I broadcast a heartbeat? ~p ~n",
                   [Reason]),
            {ok, #state{}}
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% casts

handle_cast(_Msg, State) ->
    {noreply, State}.

%% info

handle_info({udp, _ClientSocket, Ip, _Port, Data},
            #state{socket =_Socket}=State) ->
    new_sbot_client(Ip, Data),
    {noreply, State};

handle_info(advertise, #state{socket=Socket}=State) ->
    gen_udp:send(Socket,
                 {255,255,255,255},
                 8008,
                 whoami()),
    {noreply, State#state{timer=timer:send_after(1000, advertise)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

whoami()->
    "net:" ++
        inet:ntoa(local_ip_v4()) ++
        ":8008~shs:" ++
        keys:pub_key().

local_ip_v4() ->
    {ok, Addrs} = inet:getifaddrs(),
    %% Deal with taking hd of empty list
    Ips = [
         Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
         size(Addr) == 4, Addr =/= {127,0,0,1}
    ],
    case Ips of
        [] ->
            {127,0,0,1};
        _else ->
            hd(Ips)
    end.

new_sbot_client(Ip, Data) ->
    case local_ip_v4() == Ip of
        false ->
            case ets:member(sbot_clients, Ip) of
                true ->
                    [{_Ip, NSClient}] = ets:lookup(sbot_clients, Ip),
                    sbot_client:send(NSClient, ping());
                _Else ->
                    {ok, NewSbotClient} = sbot_client:start_link(Ip, extract_key(Data)),
                    ?debug("Connected to client ~p ~n",[extract_key(Data)]),
                    ets:insert(sbot_clients, {Ip, NewSbotClient}),
                    sbot_client:send(NewSbotClient, ping())
            end;
        _else ->
            %%?debug("This is looking like self ~p ~n",[local_ip_v4()]),
            ok
    end.

ping() ->
    Flags = rpc_processor:create_flags(1,0,2),
    Body = jiffy:encode({[{<<"name">>,[<<"gossip">>,<<"ping">>]},
                          {<<"args">>,[{[{<<"timeout">>, 700000}]}]},
                          {<<"type">>,<<"duplex">>}]}),
    Header = rpc_processor:create_header(Flags, size(Body), 1),
    utils:combine(Header, Body).

blob_wants() ->
    Flags = rpc_processor:create_flags(1,0,2),
    Body = jiffy:encode({[{<<"name">>,[<<"blobs">>,<<"createWants">>]},
                          {<<"args">>,[]},
                          {<<"type">>,<<"source">>}]}),
    Header = rpc_processor:create_header(Flags, size(Body), 1),
    utils:combine(Header, Body).

extract_key(Data) ->
    %% may be more that one connection string here, look for semicolon
    %% and truncate
    {Pos, Len} = binary:match(Data, <<":8008~shs:">>),
    CheckSemicolon = binary:match(Data, <<";">>),
    End = case CheckSemicolon of
              nomatch ->
                  size(Data);
              {Pos1, _} ->
                  Pos1
          end,
    base64:decode(binary_to_list(binary:part(Data,
                                             (Pos + Len),
                                             End - (Pos + Len)))).
whoami_req() ->
    Flags = rpc_processor:create_flags(0,0,2),
    Body = jiffy:encode({[{<<"name">>,[?whoami]},
                          {<<"args">>,[]},
                          {<<"type">>,<<"async">>}]}),
    Header = rpc_processor:create_header(Flags, size(Body), 1),
    utils:combine(Header, Body).
