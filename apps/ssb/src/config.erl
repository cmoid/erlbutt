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
         archive_floors/0,
         pin_archives/0,
         set_pin_archives/1,
         set_dialer/1,
         require_valid_sigs/0,
         set_require_valid_sigs/1,
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
                 archive_floors = true,
                 pin_archives = false,
                 blob_scan = false,
                 %% Reject peer messages whose signature does not verify.
                 %% Off by default: a node runs in log-and-count mode first
                 %% so the real rate can be measured before anything starts
                 %% being refused (see ssb_feed:store_msg_checked/2).
                 require_valid_sigs = false,
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

%% Max hops from our own feed for EBT replication (the follow horizon).
replication_hops() ->
    (get_config())#config.replication_hops.

%% After how many of our OWN messages to freeze the live log into an
%% archive segment, or `undefined` to never do it automatically.
%%
%% OFF by default, because archiving is not merely a disk saving — it
%% removes early history from what this node can serve.  Nothing folds the
%% frozen segments out to peers (ssb_feed:foldl/3, which both EBT and
%% createHistoryStream use, reads only the live log), so after archiving,
%% a peer holding nothing of the feed is offered the archive genesis
%% first.  An erlbutt peer can adopt a validation floor and carry on from
%% there; a client that cannot floor sees a first message with a real
%% `previous` at a sequence above 1, which ssb-validate rejects outright.
%%
%% Existing followers are unaffected — they are already at the boundary's
%% predecessor, so the genesis chains for them like any other message.
%% It is people who try to start following AFTERWARDS who cannot.
%%
%% So this is a deliberate act, not a default: a node that wants the
%% boundary sets a length, and a node that wants to stay followable by
%% anyone leaves it off.  On a pub, leave it off.
archive_length() ->
    (get_config())#config.archive_length.

%% Whether to fetch and retain the blob behind every archive boundary we
%% learn of, for feeds we replicate.
%%
%% OFF by default, and the default is the important half: an archive blob
%% holds the feed's own frozen history, so wanting it automatically
%% downloads the very messages a reader is meant to CHOOSE to fetch — and
%% downloads them even on a node that adopted a floor precisely to skip
%% them.  Fetching history is a decision, not a side effect of storing a
%% signpost.
%%
%% ON for a pub, and it matters more than it looks.  Archiving moves
%% history OUT of the feed, which every peer replicates, and INTO a blob,
%% which nobody is obliged to keep.  Boundaries propagate — a node that
%% adopted one re-advertises it — so without somebody retaining the blobs,
%% a feed's early history becomes unreachable while a signed commitment
%% goes on pointing at it.  Set {pin_archives, true}. on a node with the
%% disk to be that somebody.
pin_archives() ->
    (get_config())#config.pin_archives.

%% Whether to adopt a validation floor when a peer offers an archive
%% boundary for a feed we hold nothing of.  On by default — skipping
%% history nobody asked us to hold is the point of archiving — but it
%% changes what we replicate, so it is switchable without a rebuild.
%% Set {archive_floors, false}. in ssb.cfg to replicate every feed whole.
archive_floors() ->
    (get_config())#config.archive_floors.

%% Whether peer_dialer should dial automatically at startup.
%% Set {peer_dialer, false}. in ssb.cfg to start with dialing off.
dialer_enabled() ->
    (get_config())#config.dialer.

%% Whether a peer message that fails signature verification is REJECTED
%% (true) or stored with a warning and a count (false, the default).
%% Set {require_valid_sigs, true}. in ssb.cfg once the measured rate on
%% your corpus is known to be zero.
require_valid_sigs() ->
    (get_config())#config.require_valid_sigs.

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

%% Flip signature enforcement at runtime — for measuring on a live node,
%% and for tests, without editing ssb.cfg and restarting.
set_require_valid_sigs(Bool) when is_boolean(Bool) ->
    gen_server:call(?MODULE, {set_require_valid_sigs, Bool}, infinity).

%% Turn automatic dialing on or off, now and after a restart.
%%
%% peer_dialer:enable/0 goes through here rather than only flipping the
%% running server, because a dialer that quietly turns itself off on the
%% next boot is the kind of thing you notice weeks later by wondering why
%% a pub stopped finding peers.
set_dialer(Bool) when is_boolean(Bool) ->
    ok = gen_server:call(?MODULE, {set_dialer, Bool}, infinity),
    %% Best effort: the setting is recorded either way, and a node with no
    %% dialer running (tests, tools) still gets the persisted value.
    catch peer_dialer:apply_enabled(Bool),
    ok.

%% Turn pinning on or off on a running node — a pub deciding to start
%% retaining archive history should not need a restart to do it.
set_pin_archives(Bool) when is_boolean(Bool) ->
    gen_server:call(?MODULE, {set_pin_archives, Bool}, infinity).

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
    {ok, publish(load_overrides(Cfg))}.

%% Settings changed at runtime, layered on top of ssb.cfg.
%%
%% They CANNOT live in ssb.cfg.  That file is a relx template rendered
%% into the release directory, so it ships inside the tarball and the next
%% `tar -xzf` over an install destroys anything written there — every
%% setting reverting at once, on upgrade, silently.  This file lives with
%% the DATA instead, where a redeploy cannot reach it.
%%
%% Read last so it wins: defaults, then ssb.cfg, then this.
load_overrides(#config{repo_loc = RepoLoc} = Cfg) ->
    File = overrides_file(RepoLoc),
    case filelib:is_file(File) of
        true ->
            try load_and_parse(File, Cfg)
            catch Class:Reason ->
                    %% A corrupt overrides file must not stop the node —
                    %% it would take a pub down over a settings change.
                    ?SSB_ERROR("config: ignoring unreadable ~s: ~p:~p",
                               [File, Class, Reason]),
                    Cfg
            end;
        false ->
            Cfg
    end.

overrides_file(RepoLoc) ->
    filename:join(?b2l(RepoLoc), "overrides.cfg").

%% Record one setting so it survives a restart.
%%
%% Rewrites the whole file from the terms it already held, with this key
%% replaced — so hand-edits to other keys are kept, and a key set twice
%% does not accumulate.
persist(Key, Value, #config{repo_loc = RepoLoc}) ->
    File = overrides_file(RepoLoc),
    Existing = case file:consult(File) of
                   {ok, Terms} -> [T || T <- Terms, element(1, T) =/= Key];
                   _           -> []
               end,
    Body = [io_lib:format("~p.~n", [T]) || T <- Existing ++ [{Key, Value}]],
    case file:write_file(File, ["%% Written by erlbutt at runtime.  Layered "
                                "on top of ssb.cfg; survives a redeploy "
                                "because it lives with the data.\n",
                                Body]) of
        ok ->
            ok;
        {error, Reason} ->
            %% Say so: the caller's change took effect in memory and will
            %% be gone after a restart, which is exactly the surprise this
            %% file exists to prevent.
            ?SSB_ERROR("config: could not persist ~p to ~s: ~p — the change "
                       "applies now but will NOT survive a restart",
                       [Key, File, Reason]),
            {error, Reason}
    end.

handle_call({add_network_id, NetId}, _From, #config{extra_network_ids = Extras}=Cfg) ->
    {reply, ok, publish(Cfg#config{extra_network_ids = Extras ++ [NetId]})};

handle_call({set_require_valid_sigs, Bool}, _From, Cfg) ->
    _ = persist(require_valid_sigs, Bool, Cfg),
    {reply, ok, publish(Cfg#config{require_valid_sigs = Bool})};

handle_call({set_pin_archives, Bool}, _From, Cfg) ->
    _ = persist(pin_archives, Bool, Cfg),
    {reply, ok, publish(Cfg#config{pin_archives = Bool})};

handle_call({set_archive_length, Len}, _From, Cfg) ->
    _ = persist(archive_length, Len, Cfg),
    {reply, ok, publish(Cfg#config{archive_length = Len})};

handle_call({set_dialer, Bool}, _From, Cfg) ->
    _ = persist(peer_dialer, Bool, Cfg),
    {reply, ok, publish(Cfg#config{dialer = Bool})}.

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

parse({require_valid_sigs, Bool}, Cfg) when is_boolean(Bool) ->
    Cfg#config{require_valid_sigs = Bool};

parse({replication_hops, Hops}, Cfg) when is_integer(Hops), Hops >= 0 ->
    Cfg#config{replication_hops = Hops};

parse({pin_archives, Bool}, Cfg) when is_boolean(Bool) ->
    Cfg#config{pin_archives = Bool};
parse({archive_floors, Bool}, Cfg) when is_boolean(Bool) ->
    Cfg#config{archive_floors = Bool};
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

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% A setting changed at runtime has to still be there after a restart.
%% That is the whole point: peer_dialer:enable/0 used to flip only the
%% running server, so a pub turned on by hand came back off after the next
%% upgrade, weeks later, with nothing tying the two together.
override_survives_a_restart_test() ->
    Home = fresh_home(),
    try
        {ok, _} = config:start_link("no-such-cfg"),
        ?assert(config:dialer_enabled()),
        ok = gen_server:call(config, {set_dialer, false}, infinity),
        ?assertNot(config:dialer_enabled()),
        gen_server:stop(config),

        {ok, _} = config:start_link("no-such-cfg"),
        ?assertNot(config:dialer_enabled()),
        gen_server:stop(config)
    after
        cleanup_home(Home)
    end.

%% The overrides file is rewritten, not appended to, so setting the same
%% key repeatedly does not accumulate — and other keys are kept.
overrides_do_not_accumulate_test() ->
    Home = fresh_home(),
    try
        {ok, _} = config:start_link("no-such-cfg"),
        ok = gen_server:call(config, {set_dialer, false}, infinity),
        ok = gen_server:call(config, {set_pin_archives, true}, infinity),
        ok = gen_server:call(config, {set_dialer, true}, infinity),
        gen_server:stop(config),

        {ok, Terms} = file:consult(filename:join(Home, ".ssberl/overrides.cfg")),
        ?assertEqual(1, length([T || {peer_dialer, _} = T <- Terms])),
        ?assertEqual([{peer_dialer, true}],
                     [T || {peer_dialer, _} = T <- Terms]),
        ?assertEqual([{pin_archives, true}],
                     [T || {pin_archives, _} = T <- Terms])
    after
        cleanup_home(Home)
    end.

%% A corrupt overrides file must not stop the node: it would take a pub
%% down over a settings change.
unreadable_overrides_are_ignored_test() ->
    Home = fresh_home(),
    try
        File = filename:join(Home, ".ssberl/overrides.cfg"),
        ok = filelib:ensure_dir(File),
        ok = file:write_file(File, ~"this is not erlang terms at all"),
        {ok, _} = config:start_link("no-such-cfg"),
        %% started anyway, on the built-in default
        ?assert(config:dialer_enabled()),
        gen_server:stop(config)
    after
        cleanup_home(Home)
    end.

fresh_home() ->
    Home = filename:join("/tmp", "cfg_" ++
                         integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    Home.

cleanup_home(Home) ->
    catch gen_server:stop(config),
    os:cmd("rm -rf " ++ Home),
    application:unset_env(ssb, ssb_home),
    ok.

-endif.
