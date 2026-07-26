%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
%%
%% Tangle traversal: reading a set of cross-feed references as a
%% *conversation* — parents, ancestors, children, descendants, and the
%% depth-first tree of a thread rooted at a message.
%%
%% This lives in ssb_conv, not in the foundation, because it is one
%% interpretation of the reference graph rather than the graph itself
%% (doc/persistence.md §5).  `root`/`branch` are SSB application
%% conventions; replication never needs them.
%%
%% The edges it walks come from the ssb_links core view.  A reply to M
%% inside the tangle rooted at R is a message that names M as a `branch`
%% AND R as its `root`, so a step of the walk is the intersection of two
%% ssb_links queries and reads no message bodies at all.  It used to come
%% from a per-feed `references` file that ssb_feed wrote on every store —
%% a tangle-shaped index maintained by the foundation, which is precisely
%% the layering ssb_links exists to undo (doc/persistence.md §5).
-module(tangle).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("ssb/include/ssb.hrl").

-export([get_tangle/1,
         parents/2,
         ancestors/2,
         children/2,
         descendants/2,
         get_msgs/1]).

get_tangle(TangleId) ->
    %% retrieve tangle root author
    Auth = mess_auth:get(TangleId),
    Targets = children_of(TangleId, TangleId),
    Fun = fun([M, A]) ->
                  find_paths(M, A, TangleId)
          end,
    {TangleId, Auth, lists:map(Fun, Targets)}.

get_msgs({TangleId, Auth, Nodes}) ->
    [get_msg(TangleId, Auth) | get_msgs1(Nodes, [])].

get_msgs1(Nodes, Msgs) ->
    Fun = fun({Id, Auth, Rest}) ->
                  get_msgs1(Rest, [get_msg(Id, Auth) | Msgs]);
             ({Id, Auth}) ->
                  lists:reverse([get_msg(Id, Auth) | Msgs])
          end,
    TmpRes = lists:flatten(lists:map(Fun, Nodes)),
    ResTerm = lists:foldl(fun(E, Acc) when is_list(Acc) ->
                        case lists:member(E, Acc) of
                            true ->
                                Acc;
                            _Else ->
                                [E | Acc]
                        end;
                       (_, Acc) ->
                            Acc
                    end, [], TmpRes),
    case is_list(ResTerm) of
        true ->
            lists:reverse(ResTerm);
        false ->
            []
    end.

get_msg(Id, Auth) ->
    Feed = utils:find_or_create_feed_pid(Auth),
    msg_text(ssb_feed:fetch_msg(Feed, Id)).

%% Extract displayable text without crashing on the non-post and encrypted
%% messages that legitimately appear in a tangle.  Cleartext content is a
%% decoded JSON object ({Props}); private content is a "...box" binary that
%% we decrypt when addressed to us and otherwise surface as a placeholder.
msg_text(#message{content = {Content}}) ->
    ?pgv(~"text", Content);
msg_text(#message{content = Boxed}) when is_binary(Boxed) ->
    case private_box:decrypt(Boxed) of
        {ok, Plain} -> decrypted_text(Plain);
        not_for_me  -> ?ENCRYPTED_PLACEHOLDER
    end;
msg_text(_) ->
    undefined.

%% Decrypted private content is the original plaintext: a JSON object in
%% real SSB, but possibly a bare binary from erlbutt's own private_box.
decrypted_text(Plain) ->
    try utils:nat_decode(Plain) of
        {Props} -> ?pgv(~"text", Props);
        _       -> Plain
    catch _:_ ->
        Plain
    end.

children(MsgId, TangleId) ->
    {MsgId, children_of(MsgId, TangleId)}.

descendants(MsgId, TangleId) ->
    Targets = children_of(MsgId, TangleId),
    Fun = fun([M, A]) ->
                  find_paths(M, A, TangleId)
          end,
    {MsgId, lists:map(Fun, Targets)}.

parents(MsgId, TangleId) ->
    %% retrieve message author
    Auth = mess_auth:get(MsgId),
    FeedPid = utils:find_or_create_feed_pid(Auth),
    Msg = ssb_feed:fetch_msg(FeedPid, MsgId),
    Branches = ssb_conv_msg:is_branch(Msg),
    case Branches of
        false ->
            none;
        {TangleId, BranchList} ->
            {MsgId, lists:map(fun(P) -> [P, mess_auth:get(P)] end,
                      BranchList)};
        _Else ->
            none
    end.

ancestors(MsgId, TangleId) ->
    %% retrieve message author
    Auth = mess_auth:get(MsgId),
    FeedPid = utils:find_or_create_feed_pid(Auth),
    Msg = ssb_feed:fetch_msg(FeedPid, MsgId),
    Branches = ssb_conv_msg:is_branch(Msg),
    case Branches of
        false ->
            none;
        {TangleId, BranchList} ->
            {MsgId, lists:map(fun(P) -> find_par_paths(P, mess_auth:get(P), TangleId) end,
                      BranchList)};
        _Else ->
            none
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

find_paths(MsgId, AuthId, RootId) ->
    Targets = children_of(MsgId, RootId),
    Fun = fun([M, A]) ->
                  find_paths(M, A, RootId)
          end,
    case Targets of
        [] ->
            {MsgId, AuthId};
        _Else ->
            {MsgId, AuthId, lists:map(Fun, Targets)}
    end.

%% Replies to MsgId within the tangle rooted at TangleId, as
%% [[ReplyId, ReplyAuthor]] — the intersection of "named MsgId as a
%% branch" and "named TangleId as its root".  Both come from ssb_links,
%% so no message is read to answer this.
children_of(MsgId, TangleId) ->
    InTangle = sets:from_list(ssb_links:refs(TangleId, ~"root")),
    [[Id, mess_auth:get(Id)]
     || Id <- ssb_links:refs(MsgId, ~"branch"),
        sets:is_element(Id, InTangle)].

find_par_paths(MsgId, AuthId, RootId) ->
    Pid = utils:find_or_create_feed_pid(AuthId),
    Msg = ssb_feed:fetch_msg(Pid, MsgId),
    Branches = ssb_conv_msg:is_branch(Msg),
    case Branches of
        false ->
            {MsgId, AuthId};
        {RootId, BranchList} ->
            {MsgId, AuthId,
             lists:map(fun(P) ->
                               find_par_paths(P, mess_auth:get(P),
                                              RootId) end,
                       BranchList)};
        _Else ->
            {MsgId, AuthId}
    end.

-ifdef(TEST).

%% A fixture, so the servers these tests start are stopped again.  They
%% used to be plain _test/0 functions calling init/0 and never cleaning
%% up, which left config/keys/mess_auth/ssb_feed_sup running for whatever
%% ran next — invisible until this module moved to ssb_conv and the run
%% order changed, at which point blobs' tests started failing with
%% {already_started}.
tangle_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun basic/0,
      fun tangle1/0,
      fun tangle2/0,
      fun tangle3/0,
      fun tangle4/0,
      fun get_msg_post/0,
      fun get_msg_no_text/0,
      fun get_msg_private/0]}.

setup() ->
    cleanup(ignore),
    Home = filename:join("/tmp", "tangle_" ++
                          integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    Home.

cleanup(Home) ->
    [catch gen_server:stop(Name)
     || Name <- [ssb_links, view_manager, ssb_store, ssb_feed_sup,
                 mess_auth, keys, config]],
    case Home of
        ignore -> ok;
        _ ->
            os:cmd("rm -rf " ++ Home),
            application:unset_env(ssb, ssb_home)
    end,
    ok.

basic() ->
    {Auth, Priv, Feed} = init(),
    #message{id = Id} = make_msg_one(Auth, Priv, Feed),
    #message{content = {Content}} = ssb_feed:fetch_msg(Feed, Id),
    ?assert(~"bar" == ?pgv(~"foo", Content)).

tangle1() ->
    {Auth, Priv, Feed} = init(),
    #message{id = Id} = make_msg_one(Auth, Priv, Feed),
    #message{id = Id2} = make_msg(2, Id, Id, Id, Auth, Priv, Feed),


    ?assert({Id, Auth, [{Id2, Auth}]} == tangle:get_tangle(Id)).

tangle2() ->
    {Auth, Priv, Feed} = init(),
    #message{id = Id} = make_msg_one(Auth, Priv, Feed),
    #message{id = Id2} = make_msg(2, Id, Id, Id, Auth, Priv, Feed),
    #message{id = Id3} = make_msg(3, Id2, Id, Id2, Auth, Priv, Feed),

    ?assert({Id, Auth, [{Id2, Auth,
                   [{Id3, Auth}]}]} == tangle:get_tangle(Id)).

tangle3() ->
    {Auth, Priv, Feed} = init(),
    #message{id = Id} = make_msg_one(Auth, Priv, Feed),
    #message{id = Id2} = make_msg(2, Id, Id, Id, Auth, Priv, Feed),
    #message{id = Id3} = make_msg(3, Id2, Id, Id2, Auth, Priv, Feed),

    ?assert({Id, Auth, [{Id2, Auth,
                   [{Id3, Auth}]}]} == tangle:get_tangle(Id)),

    %% Now create another feed
    {Auth2, Priv2, Feed2} = create_id(),
    #message{id = Id4} = make_msg(4, Id2, Id, Id2, Auth2, Priv2, Feed2),


    %% Siblings come back oldest-first (Id3 before Id4).  The old
    %% references file was folded into a reversed accumulator, so it
    %% yielded newest-first; ssb_links:refs/1 preserves indexing order,
    %% which is the more useful one for reading a conversation.
    ?assert({Id, Auth, [{Id2, Auth,
                   [{Id3, Auth},
                    {Id4, Auth2}]}]} == tangle:get_tangle(Id)).

tangle4() ->
    {Auth, Priv, Feed} = init(),
    #message{id = Id} = make_msg_one(Auth, Priv, Feed),
    #message{id = Id2} = make_msg(2, Id, Id, Id, Auth, Priv, Feed),
    #message{id = Id3} = make_msg(3, Id2, Id, Id2, Auth, Priv, Feed),

    ?assert({Id, Auth, [{Id2, Auth,
                   [{Id3, Auth}]}]} == tangle:get_tangle(Id)),

    %% Now create another feed
    {Auth2, Priv2, Feed2} = create_id(),
    #message{id = Id4} = make_msg(4, Id2, Id, Id2, Auth2, Priv2, Feed2),
    #message{id = Id5} = make_msg(5, Id4, Id, [Id4, Id3], Auth2, Priv2, Feed2),


    %% Id5 branches off both Id3 and Id4, so it appears under each.
    ?assert({Id, Auth, [{Id2, Auth,
                   [{Id3, Auth,  [{Id5, Auth2}]},
                    {Id4, Auth2, [{Id5, Auth2}]}]}]} == tangle:get_tangle(Id)).

%% get_msg/2 must tolerate every content shape a tangle can contain.
get_msg_post() ->
    {Auth, Priv, Feed} = init(),
    Post = message:new_msg(nil, 1, {[{~"type", ~"post"},
                                     {~"text", ~"hello tangle"}]}, {Auth, Priv}),
    ssb_feed:store_msg(Feed, Post),
    ?assertEqual(~"hello tangle", get_msg(Post#message.id, Auth)).

%% A non-post object with no text field returns undefined, not a crash.
get_msg_no_text() ->
    {Auth, Priv, Feed} = init(),
    Vote = message:new_msg(nil, 1, {[{~"type", ~"vote"}]}, {Auth, Priv}),
    ssb_feed:store_msg(Feed, Vote),
    ?assertEqual(undefined, get_msg(Vote#message.id, Auth)).

%% Private content addressed to us is decrypted; content for someone else
%% becomes a placeholder instead of badmatching the {Content} pattern.
get_msg_private() ->
    {Auth, Priv, Feed} = init(),
    Me = keys:pub_key_disp(),
    Mine = message:new_msg(nil, 1, private_box:encrypt(~"secret words", [Me]),
                           {Auth, Priv}),
    ssb_feed:store_msg(Feed, Mine),
    ?assertEqual(~"secret words", get_msg(Mine#message.id, Auth)),

    {OtherPub, _} = utils:create_key_pair(),
    OtherId = utils:display_pub(OtherPub),
    Theirs = message:new_msg(nil, 2, private_box:encrypt(~"not yours", [OtherId]),
                             {Auth, Priv}),
    ssb_feed:store_msg(Feed, Theirs),
    ?assertEqual(?ENCRYPTED_PLACEHOLDER, get_msg(Theirs#message.id, Auth)).

%% The tangle walk reads the ssb_links core view rather than a per-feed
%% references file, so these tests need the view manager and the view
%% running — and need the view caught up before storing anything, since
%% a view mid-catch-up receives no ingests.
init() ->
    config:start_link("test/ssb.cfg"),
    keys:start_link(),
    mess_auth:start_link(),
    ssb_feed_sup:start_link(),
    ssb_store:start_link(),
    view_manager:start_link(),
    ssb_links:start_link(),
    ok = wait_caught_up(ssb_links, 250),
    create_id().

wait_caught_up(_Mod, 0) ->
    error(never_caught_up);
wait_caught_up(Mod, N) ->
    case view_manager:caught_up(Mod) of
        true  -> ok;
        false -> timer:sleep(20), wait_caught_up(Mod, N - 1)
    end.

create_id() ->
    {Pub, Priv} = utils:create_key_pair(),
    Auth = utils:display_pub(Pub),
    Feed = ssb_feed_sup:find_or_start(Auth),
    {Auth, Priv, Feed}.

make_msg_one(Auth, Priv, Feed) ->
    Msg = message:new_msg(nil, 1, {[{~"foo", ~"bar"}]}, {Auth, Priv}),
    ssb_feed:store_msg(Feed, Msg),
    Msg.

make_msg(N, Prev, Root, BranchList, Auth, Priv, Feed) ->
    Msg = message:new_msg(Prev, N, {[{~"type", ~"post"},
                                    {~"test", ~"bar"},
                                    {~"root", Root},
                                    {~"branch", BranchList}]}, {Auth, Priv}),

    ssb_feed:store_msg(Feed, Msg),
    Msg.

-endif.
