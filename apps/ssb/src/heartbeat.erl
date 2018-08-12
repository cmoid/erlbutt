%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(heartbeat).

-include_lib("ssb/include/ssb.hrl").

-behaviour(gen_server).

-compile({no_auto_import,[size/1]}).
-import(utils, [size/1]).

%% API
-export([start_link/0]).

-export([whoami/0,
         peers/0]).

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

%% identifying string sent out on UDP broadcast
whoami()->
    case inet:ntoa(local_ip_v4()) of
        {error, einval} ->
            ?l2b("net:127.0.0.1:8008~shs:" ++ keys:pub_key());
        IpString ->
            ?l2b("net:" ++
                IpString ++
                ":8008~shs:" ++
                keys:pub_key())
    end.

peers() ->
    gen_server:call(?SERVER, peers).

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
            {ok, TRef} = timer:send_after(5000, beat),
            {ok, #state{socket = Socket,
                        timer = TRef}};
        {error, Reason} ->
            ?LOG_ERROR("Why can't I broadcast a heartbeat? ~p ~n",
                   [Reason]),
            {ok, #state{}}
    end.

handle_call(peers, _From, #state{peers=Peers} = State) ->
    {reply, ets:tab2list(Peers), State};

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
    record_peer(Ip, Data, IsSelf),
    {noreply, State};

handle_info(beat, #state{socket=Socket}=State) ->
    gen_udp:send(Socket,
                 {255,255,255,255},
                 8008,
                 whoami()),
    {noreply, State#state{timer=timer:send_after(5000, beat)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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

record_peer(_Ip, _Data, true) ->
    %%?LOG_DEBUG("No talking to self ~n",[]),
    ok;

record_peer(Ip, Data, _) ->
    PeerExists = ets:member(ssb_peers, Ip),
    case PeerExists of
        false ->
            PubKey = extract_key(Data),
            ?LOG_INFO("Heartbeat received from ~p ~n",[{Ip, utils:display_pub(PubKey)}]),
            ets:insert(ssb_peers, {Ip, PubKey});
        _ ->
            nop
    end.

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
            %%base64:decode(binary_to_list(
            binary:part(Data,
                        (Pos + Len),
                        End - (Pos + Len))
    end.
