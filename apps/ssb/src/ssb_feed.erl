%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
%%
%% Per-feed gen_server.  Each SSB author gets one instance, managed by
%% ssb_feed_sup.  Owns three append-only files: log.offset (all messages),
%% profile (about messages only), and references (tangle arc records).
-module(ssb_feed).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("ssb/include/ssb.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([whoami/1,
         post_content/2,
         post_private/3,
         store_msg/2,
         fetch_msg/2,
         fetch_last_msg/1,
         store_ref/2,
         references/3,
         foldl/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile({no_auto_import,[size/1]}).
-import(utils, [load_term/1,
                 size/1]).


-record(state, {id,
                last_msg = null,
                last_seq = 0,
                feed,
                profile,
                refs,
                msg_cache}).
%%%===================================================================
%%% API
%%%===================================================================

start_link(FeedId) ->
    gen_server:start_link(?MODULE, [FeedId], []).

whoami(FeedPid) ->
    gen_server:call(FeedPid, whoami).

post_content(FeedPid, Content) ->
    gen_server:call(FeedPid, {post, Content}, infinity).

%% Encrypt Content as a private-box message addressed to RecipientIds
%% (list of <<"@pubkey.ed25519">> strings) and post it to the feed.
post_private(FeedPid, Content, RecipientIds) ->
    JsonContent = iolist_to_binary(message:ssb_encoder(Content, fun message:ssb_encoder/3, [])),
    Encrypted = private_box:encrypt(JsonContent, RecipientIds),
    gen_server:call(FeedPid, {post, Encrypted}, infinity).

store_msg(FeedPid, Msg) ->
    gen_server:call(FeedPid, {store, Msg}, infinity).

fetch_msg(FeedPid, Key) ->
    gen_server:call(FeedPid, {fetch, Key}).

fetch_last_msg(FeedPid) ->
    gen_server:call(FeedPid, {fetch_last_msg}).

%% Cast, not call: tangle calls this from inside a feed's handle_call, so a
%% synchronous call here would deadlock the same process.
store_ref(FeedPid, Arrow) ->
    gen_server:cast(FeedPid, {store_ref, Arrow}).

references(FeedPid, MsgId, RootId) ->
    gen_server:call(FeedPid, {refs, MsgId, RootId}, infinity).

foldl(FeedPid, Fun, Acc) ->
    gen_server:call(FeedPid, {foldl, Fun, Acc}, infinity).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([FeedId]) ->
    process_flag(trap_exit, true),
    DecodeId = utils:decode_id(FeedId),
    {Feed, Profile, Refs} = init_directories(DecodeId),
    State = #state{id = FeedId,
                   feed = Feed,
                   profile = Profile,
                   refs = Refs,
                   msg_cache = ets:new(messages, [])},
    %% Register in the global feed registry when running under ssb_feed_sup.
    %% The guard keeps direct start_link/1 calls (e.g. in unit tests) working.
    case ets:info(ssb_feed_registry) of
        undefined -> ok;
        _         -> ets:insert(ssb_feed_registry, {FeedId, self()})
    end,
    {ok, check_owner_feed(State)}.

handle_call(whoami, _From, #state{id = Id} = State) ->
    {reply, Id, State};

handle_call({post, Content}, _From, #state{id = Id} = State) ->
    %% A given peer can only post to the feed it owns
    CanPost = Id == keys:pub_key_disp(),
    if CanPost ->
            NewState = post(Content, State),
            {reply, ok, NewState};
       true ->
            {reply, no_post, State}
    end;

handle_call({store, Msg}, _From, State) ->
    NewState = store(Msg, State),
    {reply, ok, NewState};


handle_call({fetch, Key}, _From, #state{feed = Feed,
                                       msg_cache = Messages} = State) ->
    Val = ets:lookup(Messages, Key),
    {Pos, Msg} = feed_get(Feed, Val, Key),
    case Val of
        [] ->
            ets:insert(Messages, {Key, Pos});
        _Else ->
            nop
    end,
    {reply, message:decode(Msg, false), State};

handle_call({fetch_last_msg}, _From, #state{feed = Feed,
                                           msg_cache = Messages} = State) ->
    Resp = feed_get_last(Feed),
    case Resp of
        {Pos, Msg, Key} ->
            ets:insert(Messages, {Key, Pos}),
            {reply, message:decode(Msg, false), State};
        Else ->
            {reply, Else, State}
    end;

handle_call({refs, MsgId, TangleId}, _From, #state{refs = Refs} = State) ->
    Fun =
        fun(Data, Acc) ->
                IsArc = has_target(Data, MsgId, TangleId),
                case IsArc of
                    false ->
                        Acc;
                    Targets ->
                        [Targets | Acc]
                end end,

    Result =
        case file:open(Refs, [read, binary]) of
            {ok, IoDev} ->
                int_foldr(Fun, [], IoDev);
            {error, enoent} ->
                ?LOG_INFO("Ill formed tangle arcs file ~n",[]),
                done
        end,
    {reply, Result, State};

handle_call({foldl, Fun, Acc}, _From, #state{feed = Feed} = State) ->
    Result =
        case file:open(Feed, [read, binary]) of
            {ok, IoDev} ->
                int_foldr(Fun, Acc, IoDev);
            {error, enoent} ->
                %%?LOG_INFO("Ill formed feed ~p ~n",[Feed]),
                Acc
        end,

    {reply, Result, State}.

handle_cast({store_ref, Arrow}, #state{refs = Refs} = State) ->
    write_msg(Arrow, Refs),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%% info

handle_info(Info, State) ->
    ?LOG_INFO("WTF: ~p ~n",[Info]),
    {noreply, State}.

%%

terminate(Reason, #state{id = FeedId}) ->
    ?LOG_INFO("Closed gen_server: ~p ~n", [Reason]),
    case ets:info(ssb_feed_registry) of
        undefined -> ok;
        _         -> ets:delete(ssb_feed_registry, FeedId)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

post(Content, #state{id = FeedId, last_msg = Prev,
                     last_seq = Seq} = State) ->
    #message{id = Id} = Msg =
        message:new_msg(Prev, Seq + 1, Content,
                        {FeedId, keys:priv_key()}),
    NewState = store(Msg, State),
    NewState#state{last_msg = Id, last_seq = Seq + 1}.

store(#message{id = Id, author = Auth} = Msg,
      #state{feed = Feed,
             profile = Profile} = State) ->
    mess_auth:put(Id, Auth),
    write_msg(Msg, Feed),
    utils:update_refs(Msg),
    %% need to fix is_about to really look for profile changes
    IsAbout = social_msg:is_about(Msg),
    case IsAbout of
        true ->
            write_msg(Msg, Profile),
            State;
        _Else ->
            State
    end.

write_msg(#message{} = DecMsg, Store) ->
    Msg = message:encode(DecMsg),
    write_msg(Msg, Store);

%% On-disk frame: <<Len:32, Msg:Len/binary, Len:32, NextOffset:32>>
%% Trailing Len enables backward seek to find the last record.
%% NextOffset is the absolute file position of the following record's Len field,
%% used by scan/3 to step forward without re-reading the leading length.
write_msg(Msg, Store) ->
    DataSiz = size(Msg),
    O = open_file(Store),
    ok = file:write(O,
               <<DataSiz:32, Msg/binary, DataSiz:32>>),
    FileSize = filelib:file_size(Store) + 4,
    ok = file:write(O, <<FileSize:32>>),
    close_file(O).

init_directories(AuthDir) ->
    Location = config:feed_loc(),
    %% Author is already decoded as hex, use first two chars for directory
    <<Dir:2/binary,RestAuth/binary>> = AuthDir,
     FeedDir = <<Location/binary,Dir/binary,~"/"/binary,RestAuth/binary>>,
    Feed = <<FeedDir/binary,~"/"/binary,~"log.offset"/binary>>,
    Profile = <<FeedDir/binary,~"/"/binary,~"profile"/binary>>,
    Refs = <<FeedDir/binary,~"/"/binary,~"references"/binary>>,
    filelib:ensure_dir(Feed),
    filelib:ensure_dir(Profile),
    filelib:ensure_dir(Refs),
    {Feed, Profile, Refs}.

%% Only feed corresponding to the owner of the peer can post.
%% All the other feeds are only meant to be read
check_owner_feed(#state{id = FeedId, feed = Feed,
                       msg_cache = Messages} = State) ->
    IsOwner = FeedId == keys:pub_key_disp(),
    if IsOwner ->
            Resp = feed_get_last(Feed),
            case Resp of
                no_file ->
                    State;
                done ->
                    State;
                {Pos, Msg, Key} ->
                    ets:insert(Messages, {Key, Pos}),
                    #message{sequence = Seq} = message:decode(Msg, true),
                    State#state{last_msg = Key,
                                last_seq = Seq}
            end;
       true ->
            State
    end.

feed_get(Feed, [], Key) ->
    feed_get(Feed, [{Key, 0}], Key);

feed_get(Feed, [{Key, Pos}], Key) ->
    try
        {ok, IoDev} = file:open(Feed, [read, binary]),
        file:position(IoDev, Pos),
        Data = scan(IoDev, Pos, Key),
        file:close(IoDev),
        Data
    catch
        {error, enoent} ->
            ?LOG_INFO("Probably bad input ~n",[]),
            done
    end.

feed_get_last(Feed) ->
    case filelib:is_file(Feed) of
        true ->
            case file:open(Feed, [read, binary]) of
                {ok, IoDev} ->
                    %% Last 8 bytes = trailing Len(4) + NextOffset(4) of final record.
                    %% Read trailing Len, then seek back Len+4 to reach record start.
                    Beg = filelib:file_size(Feed) - 8,
                    file:position(IoDev, Beg),
                    case file:read(IoDev, 4) of
                        {ok, <<TermLenInt:32/integer>>} ->
                            file:position(IoDev, Beg - (TermLenInt + 4)),
                            {ok, Data} = load_term(IoDev),
                            file:close(IoDev),
                            Key = extract_key(Data),
                            {Beg - (TermLenInt + 4), Data, Key};
                        _Else ->
                            file:close(IoDev),
                            done
                    end;
               {error, Error} ->
                    ?LOG_INFO("Probably bad input ~p ~n",[{Error, Feed}]),
                    done
            end;
        false ->
            no_file
    end.

extract_key(Data) ->
    {DataProps} = utils:nat_decode(Data),
    ?pgv(~"key", DataProps).

scan(IoDev, Pos, Key) ->
    case load_term(IoDev) of
        {ok, Data} ->
            KeyVal = extract_key(Data),
            if KeyVal == Key ->
                    {Pos, Data};
               true ->
                    {ok, <<NextPos:32/integer>>} = file:read(IoDev, 4),
                    scan(IoDev, NextPos, Key)
            end;
        {error, eof} ->
            ?LOG_INFO("Key not found: ~p ~n",[Key]),
            not_found;
        {error, Error} ->
            ?LOG_INFO("Error ~p scanning for key: ~p ~n",[Error, Key])
    end.

int_foldr(Fun, Acc, IoDev) ->
    case load_term(IoDev) of
        {ok, Data} ->
            file:read(IoDev, 4),
            int_foldr(Fun, Fun(Data, Acc), IoDev);
        {error, _Error} ->
            file:close(IoDev),
            Acc
    end.

has_target(Msg, Id, RootId) ->
    {DecProps} = utils:nat_decode(Msg),
    Root = ?pgv(~"root", DecProps),
    IsRootId = RootId == Root,
    [Src, _AuthId] = ?pgv(~"src", DecProps),
    case IsRootId of
        true ->
            if Src == Id ->
                    ?pgv(~"tar", DecProps);
               true ->
                    false
            end;
        false ->
            false
    end.

open_file(File) ->
    Open = file:open(File, [append, sync]),
    case Open of
        {ok, F} ->
            F;
        Else ->
            ?LOG_INFO("Tried to open failed: ~p ~n",[Else]),
            nil
    end.

close_file(File) ->
    ok = file:close(File).

-ifdef(TEST).
instance_feed_test() ->
    config:start_link("test/ssb.cfg"),
    keys:start_link(),
    mess_auth:start_link(),
    {ok, F1} = ssb_feed:start_link(keys:pub_key_disp()),
    ok = ssb_feed:post_content(F1, ~"foo").
-endif.
