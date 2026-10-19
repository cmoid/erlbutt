%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(keys).
-include_lib("ssb/include/ssb.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-behaviour(gen_server).

-import(utils, [base_64/1,
                display_pub/1]).

%% API
-export([start_link/0,
         pub_key/0,
         pub_key_disp/0,
         priv_key/0,
         moid/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {pub_key,
                priv_key}).

%%%===================================================================
%%% API
%%%===================================================================

pub_key() ->
    gen_server:call(?MODULE, pub_key, infinity).

pub_key_disp() ->
    gen_server:call(?MODULE, pub_key_disp, infinity).

priv_key() ->
    gen_server:call(?MODULE, priv_key, infinity).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% A secret that is present but unusable stops the node.  It used to boot
%% as {nokey, nokey} (or whatkey), and a mismatched public/private pair
%% booted as the public id while signing with someone else's key: every
%% post an invalid message on our own feed.  Clients that swap the secret
%% between restarts (multiple identities) make that a file a program
%% writes, not one a person never touches — so check it, and refuse.
init([]) ->
    process_flag(trap_exit, true),
    case fetch_key() of
        {ok, {PubKey, PrivKey}} ->
            {ok, #state{pub_key = PubKey, priv_key = PrivKey}};
        first_time ->
            {PubKey, PrivKey} = init_secret(),
            {ok, #state{pub_key = PubKey, priv_key = PrivKey}};
        {error, Reason} ->
            ?LOG_ERROR("keys: refusing to start, unusable secret ~s: ~p",
                       [secret_file(), Reason]),
            {stop, {bad_secret, Reason}}
    end.

handle_call(pub_key, _From, #state{pub_key = PubKey}=State) ->
    {reply, PubKey, State};

handle_call(pub_key_disp, _From, #state{pub_key = PubKey}=State) ->
    {reply, display_pub(PubKey), State};

handle_call(priv_key, _From, #state{priv_key = PrivKey}=State) ->
    {reply, PrivKey, State}.

%% casts

handle_cast(_Msg, State) ->
    {noreply, State}.

%% info

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
secret_file() ->
    ?b2l(config:ssb_repo_loc()) ++ "secret".

fetch_key() ->
    case file:open(secret_file(), [read]) of
        {error, enoent} ->
            ?LOG_INFO("first time empty dir ~n"),
            first_time;
        {error, Reason} ->
            {error, {unreadable, Reason}};
        {ok, IoDevice} ->
            try
                parse_secret(iolist_to_binary(read_non_comment_lines(IoDevice)))
            after
                file:close(IoDevice)
            end
    end.

%% {ok, {PubB64, PrivB64}} — base64 without the .ed25519 suffix, the form
%% the rest of the node expects — or {error, Reason}.
parse_secret(Json) ->
    try json:decode(Json) of
        #{~"public" := Pub, ~"private" := Priv} = KeyMap
          when is_binary(Pub), is_binary(Priv) ->
            check_pair(key_only(Pub), key_only(Priv), maps:find(~"id", KeyMap));
        _ ->
            {error, missing_keys}
    catch _:_ ->
        {error, not_json}
    end.

%% The private key must be the one the public key belongs to: re-derive
%% the pair from the private key's seed and compare both halves.  `id` is
%% optional (ssb-keys always writes it) but must agree when present.
check_pair(PubB64, PrivB64, Id) ->
    try {base64:decode(PubB64), base64:decode(PrivB64)} of
        {<<_:32/binary>> = Pub, <<Seed:32/binary, _:32/binary>> = Priv} ->
            #{public := DerivedPub, secret := DerivedPriv} =
                enacl:sign_seed_keypair(Seed),
            DisplayId = display_pub(PubB64),
            if DerivedPub =/= Pub orelse DerivedPriv =/= Priv ->
                    {error, key_mismatch};
               Id =/= error andalso Id =/= {ok, DisplayId} ->
                    {error, id_mismatch};
               true ->
                    {ok, {PubB64, PrivB64}}
            end;
        _ ->
            {error, bad_key_length}
    catch _:_ ->
        {error, bad_base64}
    end.

read_non_comment_lines(IoDevice) ->
    case file:read_line(IoDevice) of
        {ok, Data} ->
            DataStr = case Data of
                Bin when is_binary(Bin) -> binary_to_list(Bin);
                Str                     -> Str
            end,
            First = string:sub_string(DataStr, 1, 1),
            case First == "%" orelse First == "#" orelse First == "\n" of
                true  -> read_non_comment_lines(IoDevice);
                false -> [DataStr | read_non_comment_lines(IoDevice)]
            end;
        eof ->
            []
    end.

init_secret() ->
    {Pub, Priv} = shs:create_long_pair(),
    SecretFileName = check_secret_file(),
    file:write_file(SecretFileName, prelude(), [append]),
    JsonDoc = make_json({Pub, Priv}),
    file:write_file(SecretFileName, JsonDoc, [append]),
    file:write_file(SecretFileName, suffix(Pub), [append]),
    {?l2b(base_64(Pub)), ?l2b(base_64(Priv))}.

    make_json({Pub, Priv}) ->
        PubEnc  = base_64(Pub)  ++ ".ed25519",
        PrivEnc = base_64(Priv) ++ ".ed25519",
        IdEnc   = "@" ++ base_64(Pub) ++ ".ed25519",
        iolist_to_binary([
            "{\n",
            "  \"curve\": \"ed25519\",\n",
            "  \"public\": \"",  PubEnc,  "\",\n",
            "  \"private\": \"", PrivEnc, "\",\n",
            "  \"id\": \"",      IdEnc,   "\"\n",
            "}\n"
        ]).

check_secret_file() ->
    SSBDir = ?b2l(config:ssb_repo_loc()),
    case file:list_dir(SSBDir) of
        {error, enoent} ->
            file:make_dir(SSBDir);
        _ -> ok
    end,
    SSBDir ++ "secret".

%% The guard is the check that the secret file actually held a string here:
%% these come from json:decode/1 of an on-disk file a user may have edited.
key_only(Key) when is_binary(Key) ->
    binary:replace(Key, ~".ed25519", ~"").


prelude() ->
    "# this is your SECRET name.\n"
    "# this name gives you magical powers.\n"
    "# with it you can mark your messages so that your friends can verify\n"
    "# that they really did come from you.\n"
    "#\n"
    "# if any one learns this name, they can use it to destroy your identity\n"
    "# NEVER show this to anyone!!!\n\n".

suffix(Pub) ->
    ?l2b("\n# WARNING! It's vital that you DO NOT edit OR share your secret name\n"
         "# instead, share your public name\n"
         "# your public name: @" ++ base_64(Pub) ++ ".ed25519\n").

%% hack to get Moid's key, the number one thing I seem to need in my REPL :)
moid() ->
    ~"@Sur8RwcDh6kBjub8pLZpHNWDfuuRpYVyCHrVo+TdA/4=.ed25519".

-ifdef(TEST).

pair() ->
    shs:create_long_pair().

%% A secret file body as ssb-keys writes it, from raw key bytes.
secret_body(Pub, Priv) ->
    iolist_to_binary([prelude(), make_json({Pub, Priv}), suffix(Pub)]).

valid_secret_parses_test() ->
    {Pub, Priv} = pair(),
    PubB64 = ?l2b(base_64(Pub)),
    ?assertEqual({ok, {PubB64, ?l2b(base_64(Priv))}},
                 parse_secret(make_json({Pub, Priv}))).

id_is_optional_test() ->
    {Pub, Priv} = pair(),
    Json = json:encode(#{~"curve" => ~"ed25519",
                         ~"public" => ?l2b(base_64(Pub) ++ ".ed25519"),
                         ~"private" => ?l2b(base_64(Priv) ++ ".ed25519")}),
    ?assertMatch({ok, _}, parse_secret(iolist_to_binary(Json))).

%% The dangerous one: it parses, and the node would boot as Pub while
%% signing with someone else's private key.
mismatched_pair_refused_test() ->
    {Pub, _} = pair(),
    {_, Priv} = pair(),
    ?assertEqual({error, key_mismatch}, parse_secret(make_json({Pub, Priv}))).

id_disagreeing_with_public_refused_test() ->
    {Pub, Priv} = pair(),
    {Other, _} = pair(),
    Json = json:encode(#{~"public" => ?l2b(base_64(Pub) ++ ".ed25519"),
                         ~"private" => ?l2b(base_64(Priv) ++ ".ed25519"),
                         ~"id" => display_pub(?l2b(base_64(Other)))}),
    ?assertEqual({error, id_mismatch}, parse_secret(iolist_to_binary(Json))).

malformed_secrets_refused_test() ->
    {Pub, Priv} = pair(),
    <<Short:32/binary, _/binary>> = Priv,
    ?assertEqual({error, not_json}, parse_secret(~"{\"public\": ")),
    ?assertEqual({error, not_json}, parse_secret(~"")),
    ?assertEqual({error, missing_keys}, parse_secret(~"{\"public\": \"x\"}")),
    ?assertEqual({error, missing_keys}, parse_secret(~"[1, 2]")),
    ?assertEqual({error, missing_keys},
                 parse_secret(~"{\"public\": 1, \"private\": 2}")),
    ?assertEqual({error, bad_base64},
                 parse_secret(~"{\"public\": \"!!\", \"private\": \"!!\"}")),
    ?assertEqual({error, bad_key_length}, parse_secret(make_json({Pub, Short}))).

%% Booting against a bad secret must fail the start, not run as a
%% placeholder identity; a good one boots as exactly that identity.
boot_test_() ->
    {foreach,
     fun() ->
             Home = filename:join("/tmp", "keys_" ++
                                      integer_to_list(erlang:system_time(microsecond))),
             ok = filelib:ensure_dir(Home ++ "/"),
             application:set_env(ssb, ssb_home, Home),
             {ok, _} = config:start_link("no-such-cfg"),
             ok = filelib:ensure_dir(secret_file()),
             Home
     end,
     fun(Home) ->
             catch gen_server:stop(keys),
             catch gen_server:stop(config),
             application:unset_env(ssb, ssb_home),
             os:cmd("rm -rf " ++ Home)
     end,
     [fun(_) ->
              fun() ->
                      {Pub, Priv} = pair(),
                      ok = file:write_file(secret_file(), secret_body(Pub, Priv)),
                      {ok, _} = gen_server:start({local, keys}, keys, [], []),
                      ?assertEqual(display_pub(?l2b(base_64(Pub))), pub_key_disp())
              end
      end,
      fun(_) ->
              fun() ->
                      {Pub, _} = pair(),
                      {_, Priv} = pair(),
                      ok = file:write_file(secret_file(), secret_body(Pub, Priv)),
                      ?assertEqual({error, {bad_secret, key_mismatch}},
                                   gen_server:start({local, keys}, keys, [], []))
              end
      end,
      fun(_) ->
              fun() ->
                      ok = file:write_file(secret_file(), ~"# truncated\n{\"curve\""),
                      ?assertEqual({error, {bad_secret, not_json}},
                                   gen_server:start({local, keys}, keys, [], []))
              end
      end,
      fun(_) ->
              fun() ->
                      %% absent is not bad: a first boot mints one
                      ?assertMatch({ok, _}, gen_server:start({local, keys}, keys, [], [])),
                      ?assertMatch({ok, _}, fetch_key())
              end
      end]}.

-endif.
