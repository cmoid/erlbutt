%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid

-include_lib("kernel/include/logger.hrl").

-define(l2b(List), list_to_binary(List)).
-define(b2l(List), binary_to_list(List)).
-define(pgv(K,V), proplists:get_value(K, V)).

-define(SSB_DEBUG(Format, Args), ?LOG_DEBUG("~p " ++ Format, [self() | Args])).
-define(SSB_ERROR(Format, Args), ?LOG_ERROR("~p " ++ Format, [self() | Args])).
-define(SSB_INFO(Format, Args), ?LOG_INFO("~p " ++ Format, [self() | Args])).

-define(KEEP_ALIVE_GRACE, 1500).

%% Shown in place of message text for private content we cannot decrypt.
-define(ENCRYPTED_PLACEHOLDER, <<"[encrypted]">>).

%% This network id is almost the same
-define(DEFAULT_NETWORK_ID, base64:decode("1KHLiKZvAvjbY1ziZEHMXawbCEIM6qwjCDm3VYnaR/s=")).
-define(DEFAULT_ARCHIVE_LENGTH, 10000).
-define(DEFAULT_CACHE_CAPACITY, 1000).
-define(DEFAULT_EBT_STALE_CHECK_MS,    60_000).  %% staleness poll interval
-define(DEFAULT_EBT_STALE_THRESHOLD_S,    120).  %% idle seconds before closing
-define(DEFAULT_EBT_ENTROPY_MS,        30_000).  %% anti-entropy clock interval
%%
%% as this one that is for the current main SSB network
%% 1KHLiKZvAvjbY1ziZEHMXawbCEIM6qwjCDm3VYRan/s=
%% diff is in the last three digits
%%
%% Eventually this network id will be configurable

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(SHS_NONCE, <<0:24/integer-unit:8>>).
-define(BOX_END, <<0:18/integer-unit:8>>).
-define(RPC_END, <<0:9/integer-unit:8>>).


-define(createhistorystream, <<"createHistoryStream">>).
-define(whoami, <<"whoami">>).
-define(ping, <<"ping">>).
-define(gossip, <<"gossip">>).
-define(blobs, <<"blobs">>).
-define(ebt, <<"ebt">>).
-define(tunnel, <<"tunnel">>).
-define(isRoom, <<"isRoom">>).
-define(room, <<"room">>).
-define(metadata, <<"metadata">>).
-define(attendants, <<"attendants">>).
-define(connect, <<"connect">>).
-define(createwants, <<"createWants">>).
-define(blobsget, <<"get">>).
-define(blobshas, <<"has">>).

-record(ssb_conn,
        { socket,
          nonce,
          secret_box,
          our_wants_req
        }).

-record(ssb_rpc,
        { name,
          args,
          type
        }).

-record(rpc_state,
        { calls }).

-record(message,
        { id,
          previous,
          author,
          sequence,
          timestamp,
          hash,
          content,
          signature,
          received,
          validated,
          %% the field order counts when signing messages. In some legacy messages,
          %% sequence occurs before author. The swapped boolean tracks this, for
          %% validation purposes.
          swapped
        }).

-record(sbox_state, {ref,
                socket,
                transport,
                dec_sbox_key,
                enc_sbox_key,
                dec_nonce,
                enc_nonce,
                shook_hands = 0,
                box_rem_bytes = <<>>,
                rpc_rem_bytes = <<>>,
                rpc_proc,
                %% When peer connect to a remote node
                %% We know the remote pub key when we connect
                remote_pk,
                response,
                %% Monotonically increasing counter for outbound RPC request numbers.
                req_counter = 0,
                %% Deferred reply for an in-flight fetch_blob call: {From, Ref}
                pending_fetch = undefined,
                %% Deferred reply for an in-flight has_blob call: {From, Ref}
                pending_has = undefined,
                %% In-flight sync RPC calls: #{Ref => From}
                pending_rpc = #{},
                %% ReqNo of our own outbound createWants source stream
                our_wants_req = undefined,
                %% ReqNo of the remote peer's createWants source stream (if seen)
                remote_wants_req = undefined,
                %% Set to true once an EBT duplex stream is active on this
                %% connection (either we initiated or the peer did) so that
                %% request_ebt/1 does not open a duplicate stream.
                ebt_active = false,
                %% erlang:system_time(second) of the last message processed
                %% while ebt_active = true.  Used to detect stale connections.
                ebt_last_rx = undefined,
                %% Timer ref for the periodic staleness check (undefined if EBT not yet active).
                ebt_stale_ref = undefined,
                %% The req number to use when sending clock updates to the peer.
                %% Positive when we initiated EBT (client), negative when the peer did (server).
                ebt_out_req = undefined,
                %% Timer ref for the periodic anti-entropy clock re-exchange.
                ebt_entropy_ref = undefined}).
