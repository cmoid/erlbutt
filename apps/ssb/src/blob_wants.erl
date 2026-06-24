%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2025 Charles Moid
%%
%% Handles incoming messages on a peer's blobs.createWants source stream.
%%
%% The SSB blob want/have protocol works as follows:
%%   - Negative values (-1, -2, …) are "want" signals. The magnitude
%%     indicates propagation distance; -1 means direct interest.
%%   - Positive values are "have" signals whose value is the blob size
%%     in bytes.
%%
%% Both peers open independent createWants source streams. The SSB blob
%% protocol is two one-directional sources: each peer reads the other's
%% wants/haves on the response channel of the createWants call IT made
%% (-our_wants_req), and writes its own wants/haves on the response channel
%% of the createWants call the PEER made (-remote_wants_req). So when a peer
%% sends us wants we respond with haves on -remote_wants_req. (Only if the
%% peer never opened its own createWants do we fall back to our_wants_req.)
-module(blob_wants).

-include_lib("ssb/include/ssb.hrl").

-behaviour(rpc_behavior).

-export([handle_data/3, handle_data/4]).

-compile({no_auto_import, [size/1]}).
-import(utils, [size/1]).

handle_data(ReqNo, Body, Conn) ->
    handle_data(ReqNo, Body, Conn, undefined).

handle_data(_ReqNo, Body, #ssb_conn{socket = Socket,
                                    nonce = Nonce,
                                    secret_box = Key,
                                    our_wants_req = OurWantsReq,
                                    remote_wants_req = RemoteWantsReq}, SinkPid) ->
    case utils:nat_decode(Body) of
        {Props} ->
            maybe_forward_haves(Props, SinkPid),
            respond_to_wants(have_reqno(RemoteWantsReq, OurWantsReq),
                             Props, Socket, Nonce, Key);
        _Other ->
            Nonce
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_forward_haves(_Props, undefined) ->
    ok;
maybe_forward_haves(Props, Sink) ->
    lists:foreach(fun({BlobId, Val}) when is_integer(Val), Val > 0 ->
                        ?SSB_DEBUG("forwarding have to: ~p ~p ~p~n", [Sink, BlobId, Val]),
                        forward_have(Sink, BlobId, Val);
                     (_) ->
                          ok
                  end, Props).

%% A {Name, PeerPid} sink (used by blob_fetcher) receives haves tagged with
%% the ssb_peer connection they arrived on, so it knows whom to fetch from.
forward_have({Name, PeerPid}, BlobId, Val) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid       -> Pid ! {have, BlobId, Val, PeerPid}
    end;
forward_have(Pid, BlobId, Val) when is_pid(Pid) ->
    Pid ! {have, BlobId, Val}.

%% Reply on the response channel of the peer's createWants call
%% (-remote_wants_req) per standard SSB; only if the peer never opened its
%% own createWants do we fall back to our own request channel.
have_reqno(undefined, undefined) -> undefined;
have_reqno(undefined, OurWantsReq) -> OurWantsReq;
have_reqno(RemoteWantsReq, _OurWantsReq) -> -RemoteWantsReq.

respond_to_wants(undefined, _Props, _Socket, Nonce, _Key) ->
    Nonce;
respond_to_wants(OurWantsReq, Props, Socket, Nonce, Key) ->
    ?SSB_DEBUG("respond to wants: ~p for req: ~p~n", [Props, OurWantsReq]),
    Haves = [{BlobId, BlobSize}
             || {BlobId, Val} <- Props,
                Val < 0,
                {ok, BlobSize} <- [blobs:size_of(BlobId)]],
    case Haves of
        [] ->
            Nonce;
        _ ->
            ?SSB_DEBUG("blob_wants: ~p have ~p~n", [self(), Haves]),
            HaveMsg = utils:encode_rec({Haves}),
            Flags   = rpc_processor:create_flags(1, 0, 2),
            Header  = rpc_processor:create_header(Flags, size(HaveMsg), OurWantsReq),
            utils:send_data(utils:combine(Header, HaveMsg), Socket, Nonce, Key)
    end.
