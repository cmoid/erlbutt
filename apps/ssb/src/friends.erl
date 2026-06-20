%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
%%
%% Maintained social index: a follow graph and a profile-name cache, kept
%% in named public ETS tables owned by this gen_server.  Entries are
%% loaded lazily from the per-feed contacts/profile files and then kept
%% current incrementally by social_msg:dispatch/1 at message ingest.
%%
%% Invariant: a follow-graph entry exists only when it is complete (built
%% by folding the full contacts file).  update/3 therefore drops contacts
%% for authors that have never been loaded — the message is already on
%% disk and will be folded in when the author is first queried.
%%
%% Lazy loads read the on-disk files directly rather than calling into the
%% owning ssb_feed process: update/3 and update_name/2 are synchronous
%% calls made from inside ssb_feed:store/2, so calling back into a feed
%% from here could deadlock.
-module(friends).

-behaviour(gen_server).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/0,
         direct_follows/1,
         follows/2,
         blocks/1,
         name/1,
         update/3,
         update_block/3,
         update_name/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(GRAPH, ssb_follow_graph).
-define(NAMES, ssb_profile_names).
-define(BLOCKS, ssb_block_graph).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Apply a contact message to the follow graph.  Called from
%% social_msg:dispatch/1 at ingest; synchronous so the graph is already
%% current when ssb_feed:store_msg/2 returns.  A no-op when the server
%% is down or the author has not been loaded yet.
update(Author, Contact, Following) when is_binary(Contact),
                                        is_boolean(Following) ->
    safe_call({update, Author, Contact, Following});
update(_Author, _Contact, _Following) ->
    %% Legacy garbage: contact ids that are booleans or other non-binaries.
    ok.

%% Apply a contact message's `blocking` field to the block graph (same
%% lazy/loaded-only semantics as update/3).
update_block(Author, Contact, Blocking) when is_binary(Contact),
                                             is_boolean(Blocking) ->
    safe_call({update_block, Author, Contact, Blocking});
update_block(_Author, _Contact, _Blocking) ->
    ok.

%% Record Author's latest self-assigned profile name.  Contact and about
%% messages arrive in sequence order, so the last write is the newest.
update_name(Author, Name) when is_binary(Name) ->
    safe_call({update_name, Author, Name});
update_name(_Author, _Name) ->
    ok.

%% Feeds the given feed follows right now.  Reads ETS directly on a hit;
%% folds the contacts file once on a miss.
direct_follows(FeedPid) when is_pid(FeedPid) ->
    direct_follows(ssb_feed:whoami(FeedPid));
direct_follows(FeedId) ->
    case lookup(?GRAPH, FeedId) of
        {ok, Contacts} ->
            following_ids(Contacts);
        miss ->
            case safe_call({load, FeedId}) of
                Contacts when is_map(Contacts) ->
                    following_ids(Contacts);
                _ ->
                    []
            end
    end.

%% Feeds the given feed blocks right now (mirror of direct_follows/1).
blocks(FeedPid) when is_pid(FeedPid) ->
    blocks(ssb_feed:whoami(FeedPid));
blocks(FeedId) ->
    case lookup(?BLOCKS, FeedId) of
        {ok, Blocked} ->
            blocking_ids(Blocked);
        miss ->
            case safe_call({load_blocks, FeedId}) of
                Blocked when is_map(Blocked) ->
                    blocking_ids(Blocked);
                _ ->
                    []
            end
    end.

%% Transitive follows out to HopCount hops, excluding the start feed.
follows(FeedPid, HopCount) when is_pid(FeedPid) ->
    follows(ssb_feed:whoami(FeedPid), HopCount);
follows(FeedId, HopCount) ->
    {AllFollows, _} = follows2(FeedId, HopCount, sets:from_list([FeedId])),
    lists:usort(AllFollows).

%% Latest self-assigned display name for FeedId, or undefined.
name(FeedId) ->
    case lookup(?NAMES, FeedId) of
        {ok, Name} ->
            Name;
        miss ->
            case safe_call({load_name, FeedId}) of
                ok   -> undefined;          %% server not running
                Name -> Name
            end
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?GRAPH, [set, named_table, public]),
    ets:new(?NAMES, [set, named_table, public]),
    ets:new(?BLOCKS, [set, named_table, public]),
    {ok, #{}}.

handle_call({update, Author, Contact, Following}, _From, State) ->
    case ets:lookup(?GRAPH, Author) of
        [{Author, Contacts}] ->
            ets:insert(?GRAPH, {Author, Contacts#{Contact => Following}});
        [] ->
            ok
    end,
    {reply, ok, State};

handle_call({update_block, Author, Contact, Blocking}, _From, State) ->
    case ets:lookup(?BLOCKS, Author) of
        [{Author, Blocked}] ->
            ets:insert(?BLOCKS, {Author, Blocked#{Contact => Blocking}});
        [] ->
            ok
    end,
    {reply, ok, State};

handle_call({update_name, Author, Name}, _From, State) ->
    ets:insert(?NAMES, {Author, Name}),
    {reply, ok, State};

handle_call({load, FeedId}, _From, State) ->
    Contacts =
        case ets:lookup(?GRAPH, FeedId) of
            [{FeedId, Existing}] ->
                Existing;
            [] ->
                Loaded = load_contacts(FeedId),
                ets:insert(?GRAPH, {FeedId, Loaded}),
                Loaded
        end,
    {reply, Contacts, State};

handle_call({load_blocks, FeedId}, _From, State) ->
    Blocked =
        case ets:lookup(?BLOCKS, FeedId) of
            [{FeedId, Existing}] ->
                Existing;
            [] ->
                Loaded = load_blocks(FeedId),
                ets:insert(?BLOCKS, {FeedId, Loaded}),
                Loaded
        end,
    {reply, Blocked, State};

handle_call({load_name, FeedId}, _From, State) ->
    Name =
        case ets:lookup(?NAMES, FeedId) of
            [{FeedId, Existing}] ->
                Existing;
            [] ->
                Loaded = load_profile_name(FeedId),
                ets:insert(?NAMES, {FeedId, Loaded}),
                Loaded
        end,
    {reply, Name, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

safe_call(Req) ->
    try gen_server:call(?MODULE, Req, infinity)
    catch exit:{noproc, _} -> ok
    end.

lookup(Tab, Key) ->
    try ets:lookup(Tab, Key) of
        [{Key, Val}] -> {ok, Val};
        []           -> miss
    catch
        error:badarg -> miss             %% table absent: server not running
    end.

following_ids(Contacts) ->
    [Id || Id := true <- Contacts].

blocking_ids(Blocks) ->
    [Id || Id := true <- Blocks].

follows2(_FeedId, 0, Visited) ->
    {[], Visited};

follows2(FeedId, HopCount, Visited0) ->
    NewDirect = [Id || Id <- direct_follows(FeedId),
                       not sets:is_element(Id, Visited0)],
    {Deeper, Visited1} =
        lists:foldl(
          fun(Id, {Acc, Vis}) ->
                  case sets:is_element(Id, Vis) of
                      true ->
                          {Acc, Vis};
                      false ->
                          Vis2 = sets:add_element(Id, Vis),
                          {Ids, Vis3} = follows2(Id, HopCount - 1, Vis2),
                          {lists:append(Ids, Acc), Vis3}
                  end
          end, {[], Visited0}, NewDirect),
    {lists:append(NewDirect, Deeper), Visited1}.

%% Fold the on-disk contacts file into #{ContactId => Following}.
load_contacts(FeedId) ->
    fold_feed_file(FeedId, ~"contacts",
        fun(Msg, Acc) ->
                case social_msg:is_follow(Msg) of
                    {Id, F} when is_binary(Id) -> Acc#{Id => F};
                    _                          -> Acc
                end
        end, #{}, #{}).

%% Fold the on-disk contacts file into #{ContactId => Blocking}.
load_blocks(FeedId) ->
    fold_feed_file(FeedId, ~"contacts",
        fun(Msg, Acc) ->
                case social_msg:is_block(Msg) of
                    {Id, B} when is_binary(Id) -> Acc#{Id => B};
                    _                          -> Acc
                end
        end, #{}, #{}).

%% Fold the on-disk profile file for the newest self-assigned name.
load_profile_name(FeedId) ->
    fold_feed_file(FeedId, ~"profile",
        fun(#message{content = {Props}}, Acc) ->
                case {?pgv(~"about", Props), ?pgv(~"name", Props)} of
                    {FeedId, N} when is_binary(N) -> N;
                    _                             -> Acc
                end;
           (_, Acc) ->
                Acc
        end, undefined, undefined).

%% Decode each record of a per-feed file and fold Fun over the messages.
%% Returns Acc0 if the file does not exist, Default on a malformed feed id.
fold_feed_file(FeedId, File, Fun, Acc0, Default) ->
    try
        Path = <<(utils:feed_dir(FeedId))/binary, "/", File/binary>>,
        utils:fold_log_file(
          fun(Data, Acc) ->
                  try Fun(message:decode(Data, false), Acc)
                  catch _:_ -> Acc
                  end
          end, Acc0, Path)
    catch _:_ ->
            Default
    end.

-ifdef(TEST).

friends_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [fun direct_follows_empty_test/1,
      fun direct_follows_follow_test/1,
      fun direct_follows_unfollow_test/1,
      fun update_after_load_test/1,
      fun garbage_contact_test/1,
      fun follows_zero_hops_test/1,
      fun follows_one_hop_test/1,
      fun follows_two_hop_test/1,
      fun follows_no_cycle_test/1,
      fun blocks_block_test/1,
      fun blocks_unblock_test/1,
      fun blocks_independent_of_follow_test/1,
      fun name_updates_test/1,
      fun name_lazy_load_test/1,
      fun name_other_about_test/1]}.

setup() ->
    Started = lists:filtermap(
        fun({Name, StartFun}) ->
            case whereis(Name) of
                undefined ->
                    {ok, Pid} = StartFun(),
                    {true, Pid};
                _ ->
                    false
            end
        end,
        [{config,       fun() -> config:start_link("test/ssb.cfg") end},
         {keys,         fun() -> keys:start_link() end},
         {mess_auth,    fun() -> mess_auth:start_link() end},
         {blobs,        fun() -> blobs:start_link() end},
         {ssb_feed_sup, fun() -> ssb_feed_sup:start_link() end},
         {friends,      fun() -> friends:start_link() end}]),
    Started.

teardown(Pids) ->
    lists:foreach(fun(Pid) -> gen_server:stop(Pid) end, Pids).

%% Create a fresh feed backed by a generated key pair.
%% Returns {FeedPid, FeedId, PrivKey}.
make_peer() ->
    #{public := Pub, secret := Priv} = enacl:sign_keypair(),
    Id = <<"@", (base64:encode(Pub))/binary, ".ed25519">>,
    PrivB64 = base64:encode(Priv),
    Pid = utils:find_or_create_feed_pid(Id),
    {Pid, Id, PrivB64}.

%% Store a contact message in FeedPid, signed by {AuthId, AuthPriv}.
store_contact(FeedPid, AuthId, AuthPriv, Prev, Seq, ContactId, Following) ->
    Content = {[{~"type", ~"contact"}, {~"contact", ContactId}, {~"following", Following}]},
    Msg = message:new_msg(Prev, Seq, Content, {AuthId, AuthPriv}),
    _ = ssb_feed:store_msg(FeedPid, Msg),
    ok.

%% Store an about message in FeedPid naming AboutId, signed by {AuthId, AuthPriv}.
store_about(FeedPid, AuthId, AuthPriv, Prev, Seq, AboutId, Name) ->
    Content = {[{~"type", ~"about"}, {~"about", AboutId}, {~"name", Name}]},
    Msg = message:new_msg(Prev, Seq, Content, {AuthId, AuthPriv}),
    _ = ssb_feed:store_msg(FeedPid, Msg),
    ok.

%% Store a blocking contact message in FeedPid, signed by {AuthId, AuthPriv}.
store_block(FeedPid, AuthId, AuthPriv, Prev, Seq, ContactId, Blocking) ->
    Content = {[{~"type", ~"contact"}, {~"contact", ContactId}, {~"blocking", Blocking}]},
    Msg = message:new_msg(Prev, Seq, Content, {AuthId, AuthPriv}),
    _ = ssb_feed:store_msg(FeedPid, Msg),
    ok.

direct_follows_empty_test(_) ->
    fun() ->
        {Pid, _Id, _Priv} = make_peer(),
        ?assertEqual([], friends:direct_follows(Pid))
    end.

direct_follows_follow_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([Id2], friends:direct_follows(Pid))
    end.

blocks_block_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ?assertEqual([], friends:blocks(Pid)),
        ok = store_block(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([Id2], friends:blocks(Pid))
    end.

blocks_unblock_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_block(Pid, Id, Priv, null, 1, Id2, true),
        #message{id = Msg1Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_block(Pid, Id, Priv, Msg1Id, 2, Id2, false),
        ?assertEqual([], friends:blocks(Pid))
    end.

%% follow and block are tracked independently, even within the same feed.
blocks_independent_of_follow_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_P2, Id2, _Pr2} = make_peer(),
        {_P3, Id3, _Pr3} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        #message{id = Msg1Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_block(Pid, Id, Priv, Msg1Id, 2, Id3, true),
        ?assertEqual([Id2], friends:direct_follows(Pid)),
        ?assertEqual([Id3], friends:blocks(Pid))
    end.

direct_follows_unfollow_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        #message{id = Msg1Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_contact(Pid, Id, Priv, Msg1Id, 2, Id2, false),
        ?assertEqual([], friends:direct_follows(Pid))
    end.

%% Contacts stored after the graph entry is loaded must be applied
%% incrementally via update/3 — no rescan happens on the second read.
update_after_load_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        {_Pid3, Id3, _Priv3} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([Id2], friends:direct_follows(Id)),
        #message{id = Msg1Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_contact(Pid, Id, Priv, Msg1Id, 2, Id3, true),
        ?assertEqual(lists:sort([Id2, Id3]),
                     lists:sort(friends:direct_follows(Id))),
        #message{id = Msg2Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_contact(Pid, Id, Priv, Msg2Id, 3, Id2, false),
        ?assertEqual([Id3], friends:direct_follows(Id))
    end.

%% Legacy planetary garbage: contact field holding a boolean is ignored.
garbage_contact_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, true, true),
        ?assertEqual([], friends:direct_follows(Id))
    end.

follows_zero_hops_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, Id2, true),
        ?assertEqual([], friends:follows(Pid, 0))
    end.

follows_one_hop_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_AlicePid, AliceId, _AlicePriv} = make_peer(),
        ok = store_contact(Pid, Id, Priv, null, 1, AliceId, true),
        Result = friends:follows(Pid, 1),
        ?assert(lists:member(AliceId, Result))
    end.

follows_two_hop_test(_) ->
    fun() ->
        {OwnerPid, OwnerId, OwnerPriv} = make_peer(),
        {AlicePid, AliceId, AlicePriv} = make_peer(),
        {_BobPid, BobId, _BobPriv} = make_peer(),
        ok = store_contact(OwnerPid, OwnerId, OwnerPriv, null, 1, AliceId, true),
        ok = store_contact(AlicePid, AliceId, AlicePriv, null, 1, BobId, true),
        Result = friends:follows(OwnerPid, 2),
        ?assert(lists:member(AliceId, Result)),
        ?assert(lists:member(BobId, Result))
    end.

%% Mutual follows must not cause an infinite loop, and the start node
%% must not appear in the result (it is always in the initial visited set).
follows_no_cycle_test(_) ->
    fun() ->
        {OwnerPid, OwnerId, OwnerPriv} = make_peer(),
        {AlicePid, AliceId, AlicePriv} = make_peer(),
        ok = store_contact(OwnerPid, OwnerId, OwnerPriv, null, 1, AliceId, true),
        ok = store_contact(AlicePid, AliceId, AlicePriv, null, 1, OwnerId, true),
        Result = friends:follows(OwnerPid, 5),
        ?assert(lists:member(AliceId, Result)),
        ?assertNot(lists:member(OwnerId, Result))
    end.

%% A cached undefined must be overwritten when a self-about arrives,
%% and a newer self-about wins over an older one.
name_updates_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        ?assertEqual(undefined, friends:name(Id)),
        ok = store_about(Pid, Id, Priv, null, 1, Id, ~"alice"),
        ?assertEqual(~"alice", friends:name(Id)),
        #message{id = Msg1Id} = ssb_feed:fetch_last_msg(Pid),
        ok = store_about(Pid, Id, Priv, Msg1Id, 2, Id, ~"alice the great"),
        ?assertEqual(~"alice the great", friends:name(Id))
    end.

%% Names of feeds ingested before the index existed are recovered by
%% folding the profile file on first lookup.
name_lazy_load_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        ok = store_about(Pid, Id, Priv, null, 1, Id, ~"bob"),
        ets:delete(ssb_profile_names, Id),
        ?assertEqual(~"bob", friends:name(Id))
    end.

%% An about message naming someone else must not set that feed's name.
name_other_about_test(_) ->
    fun() ->
        {Pid, Id, Priv} = make_peer(),
        {_Pid2, Id2, _Priv2} = make_peer(),
        ok = store_about(Pid, Id, Priv, null, 1, Id2, ~"impostor"),
        ?assertEqual(undefined, friends:name(Id2)),
        ?assertEqual(undefined, friends:name(Id))
    end.

-endif.
