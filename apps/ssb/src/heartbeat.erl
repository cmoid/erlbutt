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
    %% UDP broadcasts are untrusted input; never let a malformed one crash
    %% heartbeat (it would cascade to a node restart via the supervisor).
    try record_peer(Ip, Data, IsSelf)
    catch Class:Reason ->
        ?LOG_INFO("heartbeat: ignoring bad broadcast from ~p: ~p~n",
                  [Ip, {Class, Reason}])
    end,
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
            case extract_key(Data) of
                nokey ->
                    %% Broadcast without a parseable shs key — ignore it rather
                    %% than crash (a non-key value would blow up display_pub).
                    ?LOG_INFO("Heartbeat from ~p had no shs key; ignoring ~n", [Ip]);
                PubKey ->
                    ?LOG_INFO("Heartbeat received from ~p ~n",[{Ip, utils:display_pub(PubKey)}]),
                    ets:insert(ssb_peers, {Ip, PubKey}),
                    peer_dialer:trigger()
            end;
        _ ->
            nop
    end.

%% Pull the shs public key out of a broadcast.  A broadcast holds one or more
%% connection strings ("...~shs:<base64key>", separated by ';'); the key is the
%% same in each.  Match on "~shs:" rather than a fixed ":8008~shs:" so peers on
%% any port (and IPv6/ws addresses) are handled.
extract_key(Data) ->
    case binary:match(Data, ~"~shs:") of
        nomatch ->
            nokey;
        {Pos, Len} ->
            KeyStart = Pos + Len,
            Rest = binary:part(Data, KeyStart, size(Data) - KeyStart),
            case binary:match(Rest, ~";") of
                nomatch      -> Rest;
                {SemiPos, _} -> binary:part(Rest, 0, SemiPos)
            end
    end.
