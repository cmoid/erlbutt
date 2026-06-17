%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% One short-lived process per active tunnel.  Pipes muxrpc data frames
%% between two connections that meet at a room:
%%
%%   Caller (B) ──+R_B──▶ Room ──+R_A──▶ Target (A)
%%   Caller (B) ◀──R_B── Room ◀──R_A── Target (A)
%%
%% B opened the stream to the room, so the room receives B's frames at +R_B
%% and replies on -R_B.  The room opens a stream to A, sending at +R_A and
%% receiving A's replies at -R_A.  This process forwards each direction,
%% routing every send through the owning ssb_peer so nonces stay ordered.
%%
%% Note (Phase 2b): the muxrpc end flag is not visible on the sink path, so
%% the bridge tears down on connection DOWN rather than on a graceful end
%% frame.  Clean end-of-stream propagation arrives with Phase 2c.
-module(tunnel_bridge).

-include_lib("ssb/include/ssb.hrl").

-export([start/4, init/4]).

%% PidB / ReqNoB: caller's connection peer and its (positive) request number.
%% PidA: target attendant's connection peer.  Args is the tunnel.connect args.
start(PidB, ReqNoB, PidA, Args) ->
    {ok, proc_lib:spawn(?MODULE, init, [PidB, ReqNoB, PidA, Args])}.

init(PidB, ReqNoB, PidA, Args) ->
    %% Dial the target: open a duplex tunnel.connect on A's connection,
    %% routing A's replies (-ReqNoA) back to us.
    {ok, ReqNoA} = ssb_peer:open_duplex(PidA, [?tunnel, ?connect], Args, self()),
    erlang:monitor(process, PidA),
    erlang:monitor(process, PidB),
    loop(PidB, ReqNoB, PidA, ReqNoA).

loop(PidB, ReqNoB, PidA, ReqNoA) ->
    receive
        %% Frame from the caller (B → A): forward on A's outbound (+ReqNoA).
        {tunnel_data, ReqNoB, Body} ->
            ssb_peer:send_frame(PidA, ReqNoA, Body),
            loop(PidB, ReqNoB, PidA, ReqNoA);
        %% Frame from the target (A → B): forward on B's response (-ReqNoB).
        {tunnel_data, NegReqNoA, Body} when NegReqNoA =:= -ReqNoA ->
            ssb_peer:send_frame(PidB, -ReqNoB, Body),
            loop(PidB, ReqNoB, PidA, ReqNoA);
        {'DOWN', _Ref, process, _Pid, _Reason} ->
            ok;
        _Other ->
            loop(PidB, ReqNoB, PidA, ReqNoA)
    end.
