%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
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
                peers = ets:new(ssb_peers, [set, named_table]),
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
            {ok, TRef} = timer:send_after(3000, advertise),
            {ok, #state{socket = Socket,
                        timer = TRef}};
        {error, Reason} ->
            ?LOG_ERROR("Why can't I broadcast a heartbeat? ~p ~n",
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

handle_info({udp, _PeerSocket, Ip, _Port, Data},
            #state{socket =_Socket}=State) ->
    %% don't talk to yourself, unless you want complete agreement
    IsSelf = local_ip_v4() == Ip,
    new_ssb_peer(Ip, Data, IsSelf),
    {noreply, State};

handle_info(advertise, #state{socket=Socket}=State) ->
    gen_udp:send(Socket,
                 {255,255,255,255},
                 8008,
                 whoami()),
    {noreply, State#state{timer=timer:send_after(5000, advertise)}};

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

new_ssb_peer(_Ip, _Data, true) ->
    %%?LOG_DEBUG("No talking to self ~n",[]),
    ok;

new_ssb_peer(Ip, Data, _) ->
    PeerExists = ets:member(ssb_peers, Ip),
    reuse_or_create(Ip, Data, PeerExists).

reuse_or_create(Ip, _Data, true) ->
    [{_Ip, NSPeer}] = ets:lookup(ssb_peers, Ip),
    ssb_peer:send(NSPeer, ping());

reuse_or_create(Ip, Data, false) ->
    PubKey = extract_key(Data),
    if PubKey == nokey ->
            ?LOG_ERROR("No public key in data ~p ~n",[Data]);
       true ->
            ?LOG_DEBUG("Trying to connect to ~p ~n",[Ip]),
            Result =
                ssb_peer:start_link(Ip, PubKey),
            case Result of
                {ok, NewSbotPeer} ->
                    ?LOG_INFO("Started new link with ~p ~n",[{Ip, PubKey}]),
                    ets:insert(ssb_peers, {Ip, NewSbotPeer}),
                    ssb_peer:send(NewSbotPeer, ping());
                Else ->
                    ?LOG_ERROR("Issue connecting to peer ~p ~n",[Else])
            end
    end.

ping() ->
    Flags = rpc_processor:create_flags(1,0,2),
    Body = iolist_to_binary(message:ssb_encoder({[{<<"name">>,[<<"gossip">>,<<"ping">>]},
                          {<<"args">>,[{[{<<"timeout">>, 700000}]}]},
                          {<<"type">>,<<"duplex">>}]},
                                               fun message:ssb_encoder/3, [])),
    Header = rpc_processor:create_header(Flags, size(Body), 1),
    utils:combine(Header, Body).

extract_key(Data) ->
    %% may be more that one connection string here, look for semicolon
    %% and truncate
    CheckMatch = binary:match(Data, <<":8008~shs:">>),
    if CheckMatch == nomatch ->
            nokey;
       true ->
            {Pos, Len} = CheckMatch,
            CheckSemicolon = binary:match(Data, <<";">>),
            End = case CheckSemicolon of
                      nomatch ->
                          size(Data);
                      {Pos1, _} ->
                          Pos1
                  end,
            base64:decode(binary_to_list(binary:part(Data,
                                                     (Pos + Len),
                                                     End - (Pos + Len))))
    end.
