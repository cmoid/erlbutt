%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% blobs.ls — the one ssb-blobs method the renderer needs beyond the
%% builtin has/get/createWants.  patchwork's blob/obs/has.js opens
%% ls({old: false}) whenever blobs.has answers false, to be told when
%% the missing blob arrives; without the method the renderer throws
%% inside packet-stream's frame loop.  Each frame is the blob id as a
%% JSON string.  Events come from blobs:insert/1 via the `blobs' view
%% event group.
%%
%% Approximation (documented, not needed by the renderer): old: true
%% (the default) would list the existing store; we return an empty,
%% ended stream instead of walking the blob directory.
%%
%% Registered by silkpurse_app (stateless; no view).
-module(silkpurse_blobs).

-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

-export([manifest/0, handle_rpc/3]).

manifest() ->
    [{[~"blobs", ~"ls"], source, owner}].

handle_rpc([~"blobs", ~"ls"], Args, _Caller) ->
    case wants_old(Args) of
        false ->
            %% live tail only: push each newly stored blob id.  No dedup
            %% ({send, Bin}): re-storing a blob may re-emit its id, which
            %% the client's waitFor callbacks treat idempotently.
            EventFun = fun({blob, Id}) -> {send, <<$", Id/binary, $">>};
                          (_)          -> skip
                       end,
            {live_source, [], blobs, EventFun};
        true ->
            {source, []}
    end.

%% ls args are [] or [{Props}] with old/live/meta; old defaults to true.
wants_old([{Props}]) ->
    proplists:get_value(~"old", Props, true) =/= false;
wants_old(_) ->
    true.
