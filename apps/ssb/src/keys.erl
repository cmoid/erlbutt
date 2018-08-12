%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(keys).
-include_lib("ssb/include/ssb.hrl").
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

init([]) ->
    process_flag(trap_exit, true),
    {PubKey, PrivKey} =
        case fetch_key() of
            {Pub, Priv} -> {Pub, Priv};
            first_time ->
                init_secret();
            _Any ->
                ?LOG_ERROR("Should not get this ~p ~n",[_Any]),
                {~"whatkey",~"whatkey"}
        end,
    {ok, #state{pub_key = PubKey,
                priv_key = PrivKey}}.

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
fetch_key() ->
    SecretFileName = ?b2l(config:ssb_repo_loc()) ++ "secret",
    case file:open(SecretFileName, [read]) of
        {error, enoent} ->
            ?LOG_INFO("first time empty dir ~n"),
            first_time;
        {ok, IoDevice} ->
                extract_key(IoDevice)
    end.

extract_key(IoDevice) ->
    case file:read_line(IoDevice) of
        {ok, Data} ->
            DataStr = case Data of
                          Bin when is_binary(Bin) -> binary_to_list(Bin);
                          Str when is_list(Str) -> Str
                      end,
            FirstChar = string:sub_string(DataStr,1,1),
            IsCommentOrNewLine = FirstChar == "%" orelse
                FirstChar == "\n",
            if IsCommentOrNewLine ->
                    extract_key(IoDevice);
               true ->
                    DecodedValue = json:decode(iolist_to_binary(DataStr)),
                    case DecodedValue of
                        KeyMap when is_map(KeyMap) ->
                            {PubKey, PrKey} = {maps:get(~"public", KeyMap),
                                               maps:get(~"private", KeyMap)},
                            {key_only(PubKey), key_only(PrKey)};
                        _ ->
                            {~"nokey",~"nokey"}
                    end
            end;
        eof ->
            {~"nokey",~"nokey"}
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
        Doc = {[{~"curve", ~"ed25519"},
                {~"public", ?l2b(base_64(Pub) ++ ".ed25519")},
                {~"private", ?l2b(base_64(Priv) ++ ".ed25519")},
                {~"id", ?l2b("@" ++ base_64(Pub) ++ ".ed25519")}
               ]},
        iolist_to_binary(message:ssb_encoder(Doc, fun message:ssb_encoder/3, [])).

check_secret_file() ->
    SSBDir = ?b2l(config:ssb_repo_loc()),
    case file:list_dir(SSBDir) of
        {error, enoent} ->
            file:make_dir(SSBDir);
        _ -> ok
    end,
    SSBDir ++ "secret".

key_only(Key) ->
    hd(string:replace(Key,".ed25519","")).


prelude() ->
    ~"%% this is your SECRET name.\n%% this name gives you magical powers.\n%% with it you can mark your messages so that your friends can verify\n%% that they really did come from you.%%\n%% if any one learns this name, they can use it to destroy your identity\n%% NEVER show this to anyone!!!\n\n".

suffix(Pub) ->
    ?l2b("\n\n%%WARNING! It's vital that you DO NOT edit OR share your secret name\n%%instead, share your public name\n%%your public name: " ++ ?b2l(utils:display_pub(Pub))).

%% hack to get Moid's key, the number one thing I seem to need in my REPL :)
moid() ->
    ~"@Sur8RwcDh6kBjub8pLZpHNWDfuuRpYVyCHrVo+TdA/4=.ed25519".
