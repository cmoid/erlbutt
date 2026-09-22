%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% What a client needs to show, and act on, a feed at the edge of the
%% graph — one this node has deliberately not replicated.
%%
%% `glimpses.get` answers "who is this, and what would it cost to find
%% out".  A feed id is all a client has for someone at hops+1; a glimpse
%% turns it into a person.  When nothing is staged the call is not an
%% error, it is a `no` with a round kicked off behind it — the same
%% two-step archives.fetch uses, and for the same reason: collecting from
%% peers takes as long as peers take, which is not a thing to hold a
%% connection open for.
%%
%% `glimpses.promote` is the user saying yes.  It pins the feed locally
%% (feed_pins) rather than publishing a follow: promotion has to be as
%% cheap to undo as to do, or "advisory, not authoritative" stops being
%% true.  Following is a separate and later decision, made after reading
%% the feed rather than its summary.
%%
%% `glimpses.publish` writes our own.  The payload is assembled here
%% rather than in core because it is made of conventions — `about` and
%% the statement — and those are this layer's business.
%%
%% ONLY WHAT THE AUTHOR WROTE.  The payload carries their profile and
%% their statement, and nothing the node worked out for itself.  An
%% earlier version also featured recent posts and subscribed channels,
%% which was wrong twice over: the lists were bad (on a real feed the
%% channels subscribed to and the channels posted in had no overlap at
%% all, and the subscriptions were years stale), and the mechanism was
%% worse than either list.  A summary assembled out of somebody's
%% behaviour and presented in their voice is not a self-description, and
%% a reader cannot tell which parts the author chose.  If an author wants
%% to feature a post, they can quote it in the statement, which is the
%% one place everything is theirs.
-module(silkpurse_glimpses).

-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([manifest/0, handle_rpc/3]).

%% exported for tests
-export([describe/1, publish/1, promote/1, demote/1, edge/0,
         assemble/2]).

%% The payload shape, versioned from the first line.  Readers must ignore
%% what they do not know, so this is a floor and not a schema.
-define(PAYLOAD_VERSION, 1).

manifest() ->
    [{[~"glimpses", ~"get"],     async, owner},
     {[~"glimpses", ~"edge"],    async, owner},
     {[~"glimpses", ~"publish"], async, owner},
     {[~"glimpses", ~"promote"], async, owner},
     {[~"glimpses", ~"demote"],  async, owner}].

handle_rpc([~"glimpses", ~"get"], Args, _Caller) ->
    with_feed(Args, fun describe/1, ~"glimpses.get needs a feed id");
handle_rpc([~"glimpses", ~"edge"], _Args, _Caller) ->
    {reply, edge()};
handle_rpc([~"glimpses", ~"publish"], Args, _Caller) ->
    {reply, publish(opts(Args))};
handle_rpc([~"glimpses", ~"promote"], Args, _Caller) ->
    with_feed(Args, fun promote/1, ~"glimpses.promote needs a feed id");
handle_rpc([~"glimpses", ~"demote"], Args, _Caller) ->
    with_feed(Args, fun demote/1, ~"glimpses.demote needs a feed id").

with_feed(Args, Fun, Complaint) ->
    case feed_arg(Args) of
        {ok, FeedId} -> {reply, Fun(FeedId)};
        error        -> {error, Complaint}
    end.

%% Accepts either a bare id or {feedId: ...}, matching silkpurse_archives.
feed_arg([FeedId]) when is_binary(FeedId) ->
    {ok, FeedId};
feed_arg([{Opts}]) when is_list(Opts) ->
    case ?pgv(~"feedId", Opts) of
        FeedId when is_binary(FeedId) -> {ok, FeedId};
        _                             -> error
    end;
feed_arg(_) ->
    error.

opts([{Opts}]) when is_list(Opts) -> Opts;
opts(_)                           -> [].

%%%===================================================================
%%% get
%%%===================================================================

describe(FeedId) ->
    Replicated = ebt:replicate_feed(FeedId),
    Base = [{~"feed", FeedId},
            {~"replicated", Replicated},
            {~"pinned", feed_pins:is_pinned(FeedId)}],
    case ssb_glimpses:for_feed(FeedId) of
        none ->
            %% Not a failure: most feeds have never published one.  Ask
            %% the network in the background so that a second look, a few
            %% seconds later, can say something different.
            %%
            %% But only for a feed we do not carry.  A client asks this
            %% for every profile a reader opens, and for a feed already
            %% in the replication set there is nothing to discover — its
            %% glimpse, if it ever publishes one, arrives through the
            %% ordinary path.  Asking anyway spent a discovery round on
            %% every visit to an ordinary profile.
            Asking = not Replicated,
            [glimpse_discovery:peek() || Asking],
            {Base ++ [{~"found", false}, {~"asking", Asking}]};
        {ok, G} ->
            {Base ++ [{~"found", true}, {~"asking", false}] ++ descriptor(G)}
    end.

descriptor(#{seq := Seq, blob := Blob, size := Size, updated := Updated,
             source := Source}) ->
    {Held, Payload} = payload(Blob),
    [{~"seq", Seq},
     {~"blob", Blob},
     {~"size", Size},
     {~"updated", Updated},
     %% `feed` means we replicate this author and read it off their own
     %% chain; `edge` means a peer handed it to us for a feed we do not
     %% replicate.  Both are signed by the author — the difference is
     %% whether anything anchors WHICH glimpse we were shown.
     {~"source", atom_to_binary(Source)},
     {~"held", Held},
     {~"payload", Payload}].

%% The blob behind the pointer, when it has arrived.
%%
%% A glimpse message and its payload travel separately: the message is
%% relayed in a discovery round, the blob comes over the ordinary
%% want/have path afterwards.  So "we know this feed has a glimpse" and
%% "we can show it" are different states, and the client renders both.
payload(Blob) ->
    case blobs:fetch(Blob) of
        {ok, Bin} ->
            case decode(Bin) of
                {ok, Obj} -> {true, want_avatar(Obj)};
                error     -> {true, null}
            end;
        _ ->
            blob_fetcher:want(Blob),
            {false, null}
    end.

decode(Bin) ->
    try utils:nat_decode(Bin) of
        {_Props} = Obj -> {ok, Obj};
        _              -> error
    catch _:_ ->
        error
    end.

%% A face is worth more than a description, and the image a glimpse names
%% is a blob like any other — but nothing else will ever ask for it.  The
%% feed is not replicated, so no stored message mentions it and the
%% ordinary want-the-refs path never runs.  Asking here is the only
%% chance it gets.
want_avatar({Props} = Obj) ->
    case ?pgv(~"profile", Props) of
        {PProps} ->
            case ?pgv(~"image", PProps) of
                <<"&", _/binary>> = Img ->
                    case blobs:has(Img) of
                        true  -> ok;
                        false -> blob_fetcher:want(Img)
                    end;
                _ -> ok
            end;
        _ -> ok
    end,
    Obj.

%%%===================================================================
%%% edge
%%%===================================================================

%% Everything staged at the boundary: the browsable version of "feeds you
%% could be reading".  Each entry carries its payload when the blob has
%% landed, so a client can render a card without a call per feed.
edge() ->
    [{[{~"feed", Feed}, {~"pinned", feed_pins:is_pinned(Feed)}]
      ++ descriptor(G)}
     || #{feed := Feed} = G <- ssb_glimpses:edge()].

%%%===================================================================
%%% promote / demote
%%%===================================================================

%% Start replicating a feed the graph did not deliver.
%%
%% The staged glimpse is dropped on the way: it was a stand-in for the
%% feed, the feed itself is now on its way, and keeping both would leave
%% a client rendering a summary next to the thing it summarises.
promote(FeedId) ->
    case feed_pins:pin(FeedId, ~"glimpse") of
        ok ->
            ok = ssb_glimpses:forget(FeedId),
            ?SSB_INFO("glimpses: promoted ~s~n", [FeedId]),
            {[{~"feed", FeedId}, {~"pinned", true}]};
        {error, Reason} ->
            {[{~"feed", FeedId}, {~"pinned", false},
              {~"error", atom_to_binary(Reason)}]}
    end.

%% Stop asking for it.  What already arrived stays; see feed_pins.
demote(FeedId) ->
    ok = feed_pins:unpin(FeedId),
    {[{~"feed", FeedId}, {~"pinned", false}]}.

%%%===================================================================
%%% publish
%%%===================================================================

%% Write our own glimpse: assemble a payload, store it as a blob, and
%% publish a message naming it.
%%
%% Nothing is remembered between publishes.  A glimpse is a message like
%% any other, so the previous one is still there to read; a client that
%% wants to offer "edit your glimpse" reads the current one with
%% glimpses.get and hands back what the author leaves in the box.  Keeping
%% a separate draft here would be a second source of truth for something
%% the feed already holds.
publish(Opts) ->
    Self = keys:pub_key_disp(),
    Statement = statement(?pgv(~"statement", Opts)),
    Bin = encode_json(assemble(Self, Statement)),
    Size = byte_size(Bin),
    Blob = blobs:store(Bin),
    Content = {[{~"type", ~"glimpse"},
                {~"blob", Blob},
                {~"size", Size},
                {~"updated", erlang:system_time(millisecond)}]},
    case utils:find_or_create_feed_pid(Self) of
        bad ->
            {[{~"error", ~"no own feed"}]};
        Pid ->
            ok = ssb_feed:post_content(Pid, Content),
            ?SSB_INFO("glimpses: published ~s (~p bytes)~n", [Blob, Size]),
            {[{~"feed", Self}, {~"blob", Blob}, {~"size", Size}]
             ++ oversize(Size)}
    end.

statement(S) when is_binary(S) -> S;
statement(_)                   -> undefined.

%% Say so when a glimpse is bigger than peers will fetch, rather than
%% refusing to publish it.
%%
%% The size limit is a convention on the READING side — a receiver
%% declines an offer over ?GLIMPSE_MAX_SIZE (glimpse_discovery:usable/1)
%% — and turning it into a rule here would be this node deciding what an
%% author may say about themselves, which is the thing this module just
%% stopped doing.  Nothing is truncated either: those are their words.
%% But publishing a glimpse nobody will fetch is a silent failure, and an
%% author is entitled to know they have written one.
oversize(Size) when Size > ?GLIMPSE_MAX_SIZE ->
    [{~"warning", ~"larger than most peers will fetch"},
     {~"maxSize", ?GLIMPSE_MAX_SIZE}];
oversize(_Size) ->
    [].

%% The payload, as EJSON: what the author has written about themselves,
%% and nothing else.  Exported so a test can look at it without a feed to
%% publish into.
assemble(Self, Statement) ->
    {[{~"version", ?PAYLOAD_VERSION},
      {~"feed", Self},
      {~"published", erlang:system_time(millisecond)},
      {~"profile", {profile(Self)}}]
     ++ [{~"statement", Statement} || Statement =/= undefined]}.

%% Name, description and image as the feed last asserted them.  Only the
%% keys that are actually set: a null name is noise in a summary whose
%% whole job is to be read.
profile(FeedId) ->
    All = ssb_feed_meta:all(FeedId),
    [{K, maps:get(K, All)} || K <- [~"name", ~"description", ~"image"],
                              maps:is_key(K, All)].

encode_json(Term) ->
    iolist_to_binary(message:ssb_encoder(Term, fun message:ssb_encoder/3,
                                         [pretty])).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

glimpses_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
             [?_test(assemble_carries_only_what_was_written()),
              ?_test(assemble_omits_what_was_not_said()),
              ?_test(describe_an_unknown_feed_is_not_an_error()),
              ?_test(describe_does_not_ask_about_a_feed_we_carry()),
              ?_test(promote_pins_and_forgets()),
              ?_test(demote_unpins())]
     end}.

setup() ->
    cleanup(ignore),
    Home = filename:join("/tmp", "spglimpse_"
                         ++ integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = ssb_store:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_glimpses:start_link(),
    {ok, _} = feed_pins:start_link(),
    %% ebt, so that replicate_feed/1 answers honestly.  Without it the
    %% call FAILS OPEN — "we replicate everything" — which is right for
    %% production (an unrelated path keeps working) and would quietly
    %% invert what describe/1 reports here.
    %%
    %% And the two views its recompute reads: that whole function is
    %% wrapped in a try, so a missing one is not a crash, it is an
    %% replication set that silently stays empty and a pin that appears
    %% to do nothing.
    {ok, _} = ssb_social_graph:start_link(),
    {ok, _} = room_store:start_link(),
    {ok, _} = ebt:start_link(),
    Home.

cleanup(Home) ->
    [catch gen_server:stop(N) || N <- [ebt, room_store, ssb_social_graph,
                                       feed_pins, ssb_glimpses, blobs,
                                       ssb_store, keys, config]],
    case Home of
        ignore -> ok;
        _ -> os:cmd("rm -rf " ++ Home),
             application:unset_env(ssb, ssb_home)
    end,
    ok.

-define(EDGE, ~"@edgefeed.ed25519").

%% A glimpse is what the author wrote about themselves.  Nothing the node
%% worked out for itself — no recent posts, no channel list — goes in it,
%% because a summary assembled out of somebody's behaviour and printed in
%% their voice is not a self-description.
assemble_carries_only_what_was_written() ->
    {Props} = assemble(?EDGE, ~"a feed about boats"),
    ?assertEqual([~"feed", ~"profile", ~"published", ~"statement",
                  ~"version"],
                 lists:sort([K || {K, _} <- Props])).

%% No statement means no key, rather than a null one for a client to
%% special-case.
assemble_omits_what_was_not_said() ->
    {Without} = assemble(?EDGE, undefined),
    ?assertEqual(undefined, ?pgv(~"statement", Without)),
    ?assertNot(lists:keymember(~"statement", 1, Without)),
    {With} = assemble(?EDGE, ~"a feed about boats"),
    ?assertEqual(~"a feed about boats", ?pgv(~"statement", With)),
    %% the shape a client relies on is there either way
    ?assertEqual(1, ?pgv(~"version", With)),
    ?assertMatch({_}, ?pgv(~"profile", With)).

%% Most feeds have never published one; a client asking gets an answer,
%% not an error.
describe_an_unknown_feed_is_not_an_error() ->
    {Props} = describe(?EDGE),
    ?assertEqual(false, ?pgv(~"found", Props)),
    ?assertEqual(false, ?pgv(~"pinned", Props)),
    %% Not carried and nothing staged: worth asking the network about.
    ?assertEqual(false, ?pgv(~"replicated", Props)),
    ?assertEqual(true, ?pgv(~"asking", Props)).

%% A client calls this for every profile a reader opens.  For a feed
%% already in the replication set there is nothing to discover, and a
%% discovery round per profile visit was the cost of pretending
%% otherwise.
describe_does_not_ask_about_a_feed_we_carry() ->
    ok = feed_pins:pin(?EDGE, ~"manual"),
    {Props} = describe(?EDGE),
    ?assertEqual(true,  ?pgv(~"replicated", Props)),
    ?assertEqual(false, ?pgv(~"asking", Props)),
    ok = feed_pins:unpin(?EDGE).

%% Promotion pins the feed and drops the stand-in: the feed itself is on
%% its way, and a client should not render a summary beside the thing it
%% summarises.
promote_pins_and_forgets() ->
    ok = ssb_glimpses:stage(
           #message{author = ?EDGE, sequence = 4,
                    content = {[{~"type", ~"glimpse"},
                                {~"blob", ~"&g.sha256"},
                                {~"size", 10}]}}),
    ?assertMatch({ok, #{source := edge}}, ssb_glimpses:for_feed(?EDGE)),
    {Reply} = promote(?EDGE),
    ?assertEqual(true, ?pgv(~"pinned", Reply)),
    ?assert(feed_pins:is_pinned(?EDGE)),
    ?assertEqual(none, ssb_glimpses:for_feed(?EDGE)),
    %% and only a feed id can be promoted
    {Bad} = promote(~"%notafeed.sha256"),
    ?assertEqual(~"not_a_feed", ?pgv(~"error", Bad)).

demote_unpins() ->
    {Reply} = demote(?EDGE),
    ?assertEqual(false, ?pgv(~"pinned", Reply)),
    ?assertNot(feed_pins:is_pinned(?EDGE)).

%%%-------------------------------------------------------------------
%%% The write path, end to end
%%%
%%% Publishing is the half of this feature with no network in it: assemble
%%% what the views hold, store a blob, publish a message naming it, and
%%% the ordinary view pipeline must then index that message so the node
%%% can serve it to a peer.  Worth a real feed rather than a stub, because
%%% every one of those steps is a place the payload could come out empty
%%% and still look like it worked.
%%%-------------------------------------------------------------------

publish_test_() ->
    {setup, fun pub_setup/0, fun pub_cleanup/1,
     fun(_) -> {timeout, 30, [?_test(publishes_and_indexes_a_glimpse())]} end}.

pub_setup() ->
    pub_cleanup(ignore),
    Home = filename:join("/tmp", "gpub_"
                         ++ integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = ssb_store:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = view_manager:start_link(),
    {ok, _} = ssb_feed_meta:start_link(),
    {ok, _} = ssb_glimpses:start_link(),
    {ok, _} = feed_pins:start_link(),
    [ok = wait_ready(M) || M <- [ssb_feed_meta, ssb_glimpses]],
    Home.

pub_cleanup(Home) ->
    [catch gen_server:stop(N)
     || N <- [feed_pins, ssb_glimpses, ssb_feed_meta, view_manager,
              ssb_feed_sup, blobs, mess_auth, ssb_store, keys, config]],
    case Home of
        ignore -> ok;
        _ -> os:cmd("rm -rf " ++ Home),
             application:unset_env(ssb, ssb_home)
    end,
    ok.

%% Registration lands in handle_continue, after start_link/0 returns, and
%% registering a view whose state is not complete resets it — so a test
%% that writes before that arrives has its writes deleted underneath it.
wait_ready(Mod) -> wait_ready(Mod, 250).

wait_ready(Mod, 0) -> error({view_never_ready, Mod});
wait_ready(Mod, N) ->
    case lists:member(Mod, view_manager:views())
        andalso view_manager:caught_up(Mod) of
        true  -> ok;
        false -> timer:sleep(20), wait_ready(Mod, N - 1)
    end.

publishes_and_indexes_a_glimpse() ->
    Self = keys:pub_key_disp(),
    Pid = utils:find_or_create_feed_pid(Self),

    %% a profile to carry, and activity that must NOT be carried
    ok = ssb_feed:post_content(Pid, {[{~"type", ~"about"},
                                      {~"about", Self},
                                      {~"name", ~"moid"},
                                      {~"description", ~"erlang and boats"}]}),
    ok = ssb_feed:post_content(Pid, {[{~"type", ~"channel"},
                                      {~"channel", ~"erlang"},
                                      {~"subscribed", true}]}),
    ok = ssb_feed:post_content(Pid, {[{~"type", ~"post"},
                                      {~"text", ~"first thing"}]}),
    ok = ssb_feed:post_content(Pid, {[{~"type", ~"post"},
                                      {~"text", ~"second thing"}]}),

    {Res} = publish([{~"statement", ~"a feed about slow software"}]),
    ?assertEqual(Self, ?pgv(~"feed", Res)),
    ?assertMatch(<<"&", _/binary>>, ?pgv(~"blob", Res)),
    ?assertNot(lists:keymember(~"warning", 1, Res)),

    %% The glimpse message went through the ordinary publish path, so the
    %% view indexed it and we can now offer it to a peer.
    {Got} = describe(Self),
    ?assertEqual(true, ?pgv(~"found", Got)),
    ?assertEqual(~"feed", ?pgv(~"source", Got)),
    ?assertEqual(true, ?pgv(~"held", Got)),
    ?assertMatch([#{feed := Self}], ssb_glimpses:offers()),

    %% and the payload says what the author would recognise as themselves
    {Payload} = ?pgv(~"payload", Got),
    ?assertEqual(1, ?pgv(~"version", Payload)),
    ?assertEqual(~"a feed about slow software", ?pgv(~"statement", Payload)),
    {Profile} = ?pgv(~"profile", Payload),
    ?assertEqual(~"moid", ?pgv(~"name", Profile)),
    ?assertEqual(~"erlang and boats", ?pgv(~"description", Profile)),
    %% The two posts and the channel subscription above are real and
    %% indexed, and none of them are in here.  That is the point.
    ?assertNot(lists:keymember(~"posts", 1, Payload)),
    ?assertNot(lists:keymember(~"channels", 1, Payload)),

    %% Publishing again supersedes it rather than accumulating: the view
    %% keeps one glimpse per feed, the newest.
    {Again} = publish([]),
    ?assertNotEqual(?pgv(~"blob", Res), ?pgv(~"blob", Again)),
    ?assertEqual(1, length(ssb_glimpses:offers())),
    {Second} = describe(Self),
    {P2} = ?pgv(~"payload", Second),
    ?assertNot(lists:keymember(~"statement", 1, P2)),

    %% A statement longer than peers will fetch is still the author's to
    %% publish: they are told, not stopped, and not edited.
    Long = binary:copy(~"z", ?GLIMPSE_MAX_SIZE + 1),
    {Big} = publish([{~"statement", Long}]),
    ?assertEqual(~"larger than most peers will fetch", ?pgv(~"warning", Big)),
    ?assert(?pgv(~"size", Big) > ?GLIMPSE_MAX_SIZE),
    {Third} = describe(Self),
    {P3} = ?pgv(~"payload", Third),
    ?assertEqual(Long, ?pgv(~"statement", P3)).

-endif.
