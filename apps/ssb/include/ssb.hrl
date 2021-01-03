%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2018 Dionne Associates, LLC.
-define(l2b(List), list_to_binary(List)).
-define(b2l(List), binary_to_list(List)).
-define(pgv(K,V), proplists:get_value(K, V)).

-define(debug(FormatStr, Args), logger:debug(FormatStr, Args)).
-define(info(FormatStr, Args), logger:info(FormatStr, Args)).
-define(warning(FormatStr, Args), logger:warning(FormatStr, Args)).
-define(notice(FormatStr, Args), logger:notice(FormatStr, Args)).
-define(error(FormatStr, Args), logger:error(FormatStr, Args)).

-define(KEEP_ALIVE_GRACE, 1500).

%% This network id is almost the same
-define(NETWORK_ID, base64:decode("1KHLiKZvAvjbY1ziZEHMXawbCEIM6qwjCDm3VYnaR/s=")).
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

-record(ssb_conn,
        { socket,
          nonce,
          secret_box
        }).

-record(ssb_rpc,
        { name,
          args,
          type
        }).

-record(message,
        { id,
          previous,
          author,
          sequence,
          timestamp,
          hash,
          content,
          signature
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
                rpc_rem_bytes = <<>>}).
