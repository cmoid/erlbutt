%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(config).
-include_lib("ssb/include/ssb.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/1,
         ssb_repo_loc/0,
         feed_loc/0,
         blob_loc/0,
         network_id/0,
         network_ids/0,
         add_network_id/1,
         archive_length/0,
         set_archive_length/1,
         replication_hops/0,
         dialer_enabled/0,
         blob_scan_enabled/0,
         is_room/0,
         room_name/0,
         room_privacy/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, ssb_config).

%% The whole configuration lives in one record, kept private to this
%% module: callers go through the accessor functions below, so the record
%% can change shape without touching (or recompiling) any other module.
-record(config, {ssb_home,
                 repo_loc,
                 feed_loc,
                 blob_loc,
                 net_id,
                 extra_network_ids = [],
                 archive_length = ?DEFAULT_ARCHIVE_LENGTH,
                 replication_hops = ?DEFAULT_REPLICATION_HOPS,
                 dialer = true,
                 blob_scan = false,
                 room = false,
                 room_name = <<"erlbutt room">>,
                 room_privacy = open}).

%%%===================================================================
%%% API
%%%===================================================================

%% Reads never touch the gen_server: the current #config{} is published
%% to a protected ETS table owned by it (same read-mostly pattern as
%% ebt_repl_set / network_id_cache), so hot paths like the SHS handshake
%% don't serialize through this process's mailbox.

ssb_repo_loc() ->
    (get_config())#config.repo_loc.

feed_loc() ->
    (get_config())#config.feed_loc.

blob_loc() ->
    (get_config())#config.blob_loc.

network_id() ->
    (get_config())#config.net_id.

network_ids() ->
    Cfg = get_config(),
    [Cfg#config.net_id | Cfg#config.extra_network_ids].

archive_length() ->
    (get_config())#config.archive_length.

%% Max hops from our own feed for EBT replication (the follow horizon).
replication_hops() ->
    (get_config())#config.replication_hops.

%% Whether peer_dialer should dial automatically at startup.
%% Set {peer_dialer, false}. in ssb.cfg to start with dialing off.
dialer_enabled() ->
    (get_config())#config.dialer.

%% Whether to scan existing on-disk messages for blob references at startup
%% and fetch any we don't already hold.  Off by default (it folds the whole
%% log); enable with {blob_scan, true}. in ssb.cfg.
blob_scan_enabled() ->
    (get_config())#config.blob_scan.

%% Whether this node acts as an SSB room (connection relay).
is_room() ->
    (get_config())#config.room.

room_name() ->
    (get_config())#config.room_name.

%% Room privacy mode: open | community | restricted.
room_privacy() ->
    (get_config())#config.room_privacy.

add_network_id(NetId) when is_binary(NetId) ->
    gen_server:call(?MODULE, {add_network_id, NetId}, infinity).

set_archive_length(undefined) ->
    gen_server:call(?MODULE, {set_archive_length, undefined}, infinity);
set_archive_length(Len) when is_integer(Len), Len > 0 ->
    gen_server:call(?MODULE, {set_archive_length, Len}, infinity).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, ["ssb.cfg"], []).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Config]) ->
    process_flag(trap_exit, true),
    ets:new(?TABLE, [named_table, protected, set, {read_concurrency, true}]),
    SSBHome = application:get_env(ssb, ssb_home, "."),
    %% Room settings default from application env; a cfg-file entry overrides.
    Base = #config{ssb_home = SSBHome,
                   net_id = default_net_id(),
                   room = application:get_env(ssb, room, false),
                   room_name = application:get_env(ssb, room_name, <<"erlbutt room">>),
                   room_privacy = application:get_env(ssb, room_privacy, open),
                   replication_hops = application:get_env(ssb, replication_hops,
                                                          ?DEFAULT_REPLICATION_HOPS)},
    Cfg = case filelib:is_file(Config) of
              true ->
                  load_and_parse(Config, Base#config{repo_loc = default_repo(SSBHome)});
              false ->
                  Base#config{repo_loc = default_repo(SSBHome),
                              feed_loc = default_feed_store(SSBHome),
                              blob_loc = default_blob_store(SSBHome)}
          end,
    {ok, publish(Cfg)}.

handle_call({add_network_id, NetId}, _From, #config{extra_network_ids = Extras}=Cfg) ->
    {reply, ok, publish(Cfg#config{extra_network_ids = Extras ++ [NetId]})};

handle_call({set_archive_length, Len}, _From, Cfg) ->
    {reply, ok, publish(Cfg#config{archive_length = Len})}.

%% casts

handle_cast(_Msg, Cfg) ->
    {noreply, Cfg}.

%% info

handle_info(_Info, Cfg) ->
    {noreply, Cfg}.


terminate(_Reason, _Cfg) ->
    ok.

code_change(_OldVsn, Cfg, _Extra) ->
    {ok, Cfg}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% The ETS copy is the read path; the gen_server state is the write path.
%% The table is owned by this process, so it disappears (and reads start
%% failing loudly) if the config server goes down.
publish(#config{} = Cfg) ->
    ets:insert(?TABLE, {config, Cfg}),
    Cfg.

get_config() ->
    [{config, Cfg}] = ets:lookup(?TABLE, config),
    Cfg.

load_and_parse(CfgFile, #config{} = Cfg) ->
    {ok, CfgTerms} = file:consult(CfgFile),
    lists:foldl(fun(CfgTerm, CfgIn) ->
                        parse(CfgTerm, CfgIn)
                end, Cfg, CfgTerms).

parse({feed_store_location, Loc}, #config{repo_loc = RepLoc} = Cfg) ->
    Store = ?l2b(?b2l(RepLoc) ++ Loc),
    filelib:ensure_dir(Store),
    Cfg#config{feed_loc = Store};

parse({blob_store_location, Loc}, #config{repo_loc = RepLoc} = Cfg) ->
    Store = ?l2b(?b2l(RepLoc) ++ Loc),
    filelib:ensure_dir(Store),
    Cfg#config{blob_loc = Store};

parse({network_id, NetId}, Cfg) ->
    Cfg#config{net_id = base64:decode(NetId)};

parse({extra_network_ids, List}, Cfg) when is_list(List) ->
    Cfg#config{extra_network_ids = [base64:decode(Id) || Id <- List]};

parse({archive_length, Len}, Cfg) when is_integer(Len), Len > 0 ->
    Cfg#config{archive_length = Len};

parse({replication_hops, Hops}, Cfg) when is_integer(Hops), Hops >= 0 ->
    Cfg#config{replication_hops = Hops};

parse({peer_dialer, Bool}, Cfg) when is_boolean(Bool) ->
    Cfg#config{dialer = Bool};

parse({blob_scan, Bool}, Cfg) when is_boolean(Bool) ->
    Cfg#config{blob_scan = Bool};

parse({room, Bool}, Cfg) when is_boolean(Bool) ->
    Cfg#config{room = Bool};

parse({room_name, Name}, Cfg) when is_binary(Name) ->
    Cfg#config{room_name = Name};

parse({room_privacy, Privacy}, Cfg)
  when Privacy =:= open; Privacy =:= community; Privacy =:= restricted ->
    Cfg#config{room_privacy = Privacy};

parse(Any, Cfg) ->
    %% Unrecognised config term: either an unknown key or a known key whose
    %% value failed its guard (e.g. {peer_dialer, "false"} — a string instead
    %% of the atom false, as produced by a mis-quoted ssb.cfg template). Warn
    %% rather than silently drop it, so the setting being ignored is visible.
    ?SSB_ERROR("config: ignoring unrecognised term ~p", [Any]),
    Cfg.

default_repo(SSBHome) ->
    ?l2b(SSBHome ++ "/.ssberl/").

default_feed_store(SSBHome) ->
    DataStore = ?l2b(SSBHome ++ "/.ssberl/feeds/"),
    filelib:ensure_dir(DataStore),
    DataStore.

default_blob_store(SSBHome) ->
    DataStore = ?l2b(SSBHome ++ "/.ssberl/blobs/"),
    filelib:ensure_dir(DataStore),
    DataStore.

default_net_id() ->
    ?DEFAULT_NETWORK_ID.
