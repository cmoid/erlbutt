%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(social_msg).
-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-import(message, [decode/2]).

%% API
-export([dispatch/1,
         is_follow/1,
         is_block/1,
         is_about/1,
         is_reply/1,
         is_branch/1,
         is_private_box/1]).

%% Dispatch a stored message to application-layer handlers based on type.
dispatch(#message{author = Author, content = {Props}}) ->
    %% any blob referenced by the message gets requested from peers
    blob_fetcher:want_refs({Props}),
    Type = ?pgv(~"type", Props),
    dispatch_type(Type, Author, Props);
%% Private (encrypted) messages addressed to us: the content is a ".box"
%% string, so the clause above can't see inside it. Decrypt with our key and,
%% if it is for us, request any blob the sender attached (e.g. an image in a
%% DM). We only extract blob refs here — we do not run dispatch_type on
%% private content.
dispatch(#message{content = Content}) when is_binary(Content) ->
    case private_box:decrypt(Content) of
        {ok, Plain} ->
            %% Decrypted content is normally a JSON object, but a DM body can be
            %% any plaintext — nat_decode throws on non-JSON, so guard it.
            try utils:nat_decode(Plain) of
                {Props} -> blob_fetcher:want_refs({Props});
                _       -> ok
            catch _:_ -> ok
            end;
        _ ->
            ok
    end;
dispatch(_) ->
    ok.

dispatch_type(~"pub", _Author, Props) ->
    case ?pgv(~"address", Props) of
        {AddrProps} ->
            Host = ?pgv(~"host", AddrProps),
            Port = ?pgv(~"port", AddrProps),
            Key  = ?pgv(~"key",  AddrProps),
            case is_binary(Host) andalso is_integer(Port) andalso is_binary(Key) of
                true ->
                    KeyBody = case Key of
                        <<"@", Rest/binary>> -> Rest;
                        Rest                 -> Rest
                    end,
                    KeyB64 = hd(string:replace(KeyBody, ".ed25519", "")),
                    Addr = iolist_to_binary([~"net:", Host, ~":",
                                             integer_to_binary(Port),
                                             ~"~shs:", KeyB64]),
                    Meta = #{~"host" => Host, ~"port" => Port,
                             ~"key"  => Key,  ~"type" => ~"pub"},
                    conn_db:remember(Addr, Meta, ~"pub");
                false ->
                    ok
            end;
        _ -> ok
    end;

%% A contact message can carry `following` and/or `blocking`; apply each
%% to its respective graph independently.
dispatch_type(~"contact", Author, Props) ->
    Contact = ?pgv(~"contact", Props),
    case check_contact(Contact, ?pgv(~"following", Props)) of
        {C, F} -> friends:update(Author, C, F);
        nope   -> ok
    end,
    case check_block(Contact, ?pgv(~"blocking", Props)) of
        {Cb, B} -> friends:update_block(Author, Cb, B);
        nope    -> ok
    end,
    ok;

%% Self-assigned profile names feed the name index; abouts naming other
%% feeds (or carrying only image/description) are ignored.
dispatch_type(~"about", Author, Props) ->
    case {?pgv(~"about", Props), ?pgv(~"name", Props)} of
        {Author, Name} when is_binary(Name) ->
            friends:update_name(Author, Name);
        _ ->
            ok
    end;

dispatch_type(_, _, _) ->
    ok.

is_follow(#message{content = Val}) when is_binary(Val) ->
    nope;

is_follow(#message{content = {ContentProps}}) ->
    Type = ?pgv(~"type", ContentProps),
    case Type of
        ~"contact" ->
            check_contact(?pgv(~"contact",ContentProps),
                          ?pgv(~"following", ContentProps));
        _Else ->
            nope
    end.

is_block(#message{content = Val}) when is_binary(Val) ->
    nope;

is_block(#message{content = {ContentProps}}) ->
    case ?pgv(~"type", ContentProps) of
        ~"contact" ->
            check_block(?pgv(~"contact",  ContentProps),
                        ?pgv(~"blocking", ContentProps));
        _Else ->
            nope
    end.

is_about(#message{content = Content}) when is_binary(Content) ->
    false;

is_about(#message{content = {ContentProps}}) ->
    Type = ?pgv(~"type", ContentProps),
    case Type of
        undefined ->
            false;
        Type ->
            Type == ~"about"
    end.

is_branch(#message{content = Content}) when is_binary(Content) ->
    false;

is_branch(#message{content = {Content}}) ->
    Root = ?pgv(~"root", Content),
    Branch = ?pgv(~"branch", Content),
    build_branch(Root, Branch).

build_branch(undefined, _Branch) ->
    false;
build_branch(_Root, undefined) ->
    false;
build_branch(false, _Branch) ->
    false;
build_branch(_Root, false) ->
    false;
build_branch(Root, Branch) when is_list(Branch) ->
    {Root, Branch};
build_branch(Root, Branch) ->
    {Root, [Branch]}.

is_reply(#message{content = Content}) when is_binary(Content) ->
    false;

is_reply(#message{content = {Content}}) ->
    build_reply(?pgv(~"reply", Content)).

build_reply(undefined) ->
    false;

build_reply(Reply) ->
    Reply.

is_private_box(#message{content = Content}) ->
    private_box:is_private(Content).

check_contact(undefined, _Following) ->
    nope;
check_contact(<<>>, _Following) ->
    nope;
check_contact(Contact, true) ->
    {Contact, true};
check_contact(Contact, false) ->
    {Contact, false};
check_contact(_Contact, _Following) ->
    nope.

check_block(undefined, _Blocking) ->
    nope;
check_block(<<>>, _Blocking) ->
    nope;
check_block(Contact, true) ->
    {Contact, true};
check_block(Contact, false) ->
    {Contact, false};
check_block(_Contact, _Blocking) ->
    nope.


-ifdef(TEST).


dispatch_pub_test() ->
    ConfigStarted = case whereis(config) of
        undefined -> {ok, _} = config:start_link("test/ssb.cfg"), true;
        _         -> false
    end,
    {ok, DbPid} = conn_db:start_link(),
    Msg = #message{id = ~"%test.sha256", previous = null,
                   author = ~"@author=.ed25519", sequence = 1,
                   timestamp = 0, hash = ~"sha256", received = 0,
                   validated = true, swapped = false, signature = ~"sig",
                   content = {[{~"type",    ~"pub"},
                                {~"address", {[{~"host", ~"pub.example.com"},
                                               {~"port", 8008},
                                               {~"key",  ~"@pubkey=.ed25519"}]}}]}},
    ok = social_msg:dispatch(Msg),
    All = conn_db:all(),
    ExpAddr = ~"net:pub.example.com:8008~shs:pubkey=",
    ?assert(maps:is_key(ExpAddr, All)),
    gen_server:stop(DbPid),
    file:delete(?b2l(config:ssb_repo_loc()) ++ "conn.json"),
    case ConfigStarted of
        true -> gen_server:stop(config);
        false -> ok
    end.

dispatch_contact_test() ->
    Msg = #message{id = ~"%c.sha256", previous = null,
                   author = ~"@author=.ed25519", sequence = 2,
                   timestamp = 0, hash = ~"sha256", received = 0,
                   validated = true, swapped = false, signature = ~"sig",
                   content = {[{~"type",      ~"contact"},
                                {~"contact",   ~"@other=.ed25519"},
                                {~"following", true}]}},
    ?assertEqual(ok, social_msg:dispatch(Msg)).

dispatch_about_test() ->
    FriendsStarted = case whereis(friends) of
        undefined -> {ok, _} = friends:start_link(), true;
        _         -> false
    end,
    Self = #message{id = ~"%a1.sha256", previous = null,
                    author = ~"@author=.ed25519", sequence = 3,
                    timestamp = 0, hash = ~"sha256", received = 0,
                    validated = true, swapped = false, signature = ~"sig",
                    content = {[{~"type",  ~"about"},
                                 {~"about", ~"@author=.ed25519"},
                                 {~"name",  ~"zelda"}]}},
    ok = social_msg:dispatch(Self),
    ?assertEqual(~"zelda", friends:name(~"@author=.ed25519")),
    %% An about naming a different feed must not set that feed's name.
    Other = Self#message{id = ~"%a2.sha256", sequence = 4,
                         content = {[{~"type",  ~"about"},
                                      {~"about", ~"@other=.ed25519"},
                                      {~"name",  ~"impostor"}]}},
    ok = social_msg:dispatch(Other),
    ?assertEqual(undefined, friends:name(~"@other=.ed25519")),
    case FriendsStarted of
        true  -> gen_server:stop(friends);
        false -> ok
    end.

%% Without the friends server running, dispatch must still succeed.
dispatch_about_no_friends_test() ->
    Msg = #message{id = ~"%a3.sha256", previous = null,
                   author = ~"@author=.ed25519", sequence = 5,
                   timestamp = 0, hash = ~"sha256", received = 0,
                   validated = true, swapped = false, signature = ~"sig",
                   content = {[{~"type",  ~"about"},
                                {~"about", ~"@author=.ed25519"},
                                {~"name",  ~"zelda"}]}},
    ?assertEqual(ok, social_msg:dispatch(Msg)).

dispatch_unknown_type_test() ->
    Msg = #message{id = ~"%p.sha256", previous = null,
                   author = ~"@author=.ed25519", sequence = 3,
                   timestamp = 0, hash = ~"sha256", received = 0,
                   validated = true, swapped = false, signature = ~"sig",
                   content = {[{~"type", ~"post"}, {~"text", ~"hello"}]}},
    ?assertEqual(ok, social_msg:dispatch(Msg)).

%% A post with a blob mention must dispatch fine without blob_fetcher running.
dispatch_blob_mention_no_fetcher_test() ->
    BlobId = <<"&", (base64:encode(crypto:hash(sha256, ~"pic")))/binary, ".sha256">>,
    Msg = #message{id = ~"%m.sha256", previous = null,
                   author = ~"@author=.ed25519", sequence = 6,
                   timestamp = 0, hash = ~"sha256", received = 0,
                   validated = true, swapped = false, signature = ~"sig",
                   content = {[{~"type", ~"post"}, {~"text", ~"see pic"},
                                {~"mentions", [{[{~"link", BlobId}]}]}]}},
    ?assertEqual(ok, social_msg:dispatch(Msg)).

dispatch_private_test() ->
    Msg = #message{id = ~"%priv.sha256", previous = null,
                   author = ~"@author=.ed25519", sequence = 4,
                   timestamp = 0, hash = ~"sha256", received = 0,
                   validated = true, swapped = false, signature = ~"sig",
                   content = ~"box.encrypted"},
    ?assertEqual(ok, social_msg:dispatch(Msg)).

is_about_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "about.full",
    {ok, FilBin} = file:read_file(F),

    ?assert(social_msg:is_about(decode(FilBin, true))).

is_reply_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "reply.full",
    {ok, FilBin} = file:read_file(F),

    ?assert(social_msg:is_reply(decode(FilBin, true)) /= false).

is_multi_reply_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "mult-reply.full",
    {ok, FilBin} = file:read_file(F),
    Msg = decode(FilBin, true),
    {ListPairs} = social_msg:is_reply(Msg),
    ?assert(length(ListPairs) == 2).

is_not_reply_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "not_reply.full",
    {ok, FilBin} = file:read_file(F),

    ?assert(not social_msg:is_reply(decode(FilBin, true))).

is_single_branch_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "single-branch.full",
    {ok, FilBin} = file:read_file(F),
    {_Root, Branches} = social_msg:is_branch(decode(FilBin, true)),
    ?assert(is_list(Branches) andalso length(Branches) == 1).

is_multi_branch_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "multi-branch.full",
    {ok, FilBin} = file:read_file(F),
    {_Root, Branches} = social_msg:is_branch(decode(FilBin, true)),
    ?assert(is_list(Branches) andalso length(Branches) == 2).

follow_true_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "follow_true.full",
    {ok, FilBin} = file:read_file(F),
    {_FeedId, Bool} = social_msg:is_follow(decode(FilBin, true)),

    ?assert(Bool).

follow_false_test() ->
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "follow_false.full",
    {ok, FilBin} = file:read_file(F),
    {_FeedId, Bool} = social_msg:is_follow(decode(FilBin, true)),

    ?assert(not Bool).

-endif.
