%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2018 Dionne Associates, LLC.
-module(ssb_feed).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ssb.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2]).

-export([open/1,
         is_open/1,
         close/1,
         process_msg/2,
         fetch_msg/2,
         foldl/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {id,
                feed_open = false,
                feed,
                feed_file = nil,
                meta,
                meta_file = nil,
                msg_cache}).
%%%===================================================================
%%% API
%%%===================================================================

start_link(FeedId, Location) ->
    gen_server:start_link(?MODULE, [FeedId, Location], []).

%% Msg is the content field of a message, the assumption being that
%% last_msg has all the other fields needed
process_msg(FeedPid, Msg) ->
    gen_server:call(FeedPid, {process, Msg}).

fetch_msg(FeedPid, Key) ->
    gen_server:call(FeedPid, {fetch, Key}).

foldl(FeedPid, Fun, Acc) ->
    gen_server:call(FeedPid, {foldl, Fun, Acc}).

open(Pid) ->
    gen_server:call(Pid, {open}).

is_open(Pid) ->
    gen_server:call(Pid, {is_open}).

close(Pid) ->
    gen_server:call(Pid, {close}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([FeedId, Location]) ->
    process_flag(trap_exit, true),
    <<"@",Id/binary>> = FeedId,
    RawId = hd(string:replace(Id,".ed25519","")),
    DecodeId = integer_to_binary(binary:decode_unsigned(base64:decode(RawId)),16),
    {Feed, Meta} = init_directories(DecodeId, Location),
    {ok, #state{id = FeedId,
                feed = Feed,
                meta = Meta,
                msg_cache = ets:new(messages, [])}}.

handle_call({open}, _From, #state{feed_open = false} = State) ->
    NewState = open_feed(State),
    {reply, ok, NewState};

handle_call({open}, _From, #state{feed_open = true} = State) ->
    %% already open, do nothing
    {reply, ok, State};

handle_call({is_open}, _From, #state{feed_open = false} = State) ->
    {reply, false, State};

handle_call({is_open}, _From, #state{feed_open = true} = State) ->
    {reply, true, State};


handle_call({close}, _From, #state{feed_open = true} = State) ->
    NewState = close_feed(State),
    {reply, ok, NewState};

handle_call({close}, _From, #state{feed_open = false} = State) ->
    %% already closed, do nothing
    {reply, ok, State};

handle_call({process, Msg}, _From, #state{feed_open = true} = State) ->
    NewState = store(Msg, State),
    {reply, ok, NewState};

handle_call({process, Msg}, _From, #state{feed_open = false} = State) ->
    OpenState = open_feed(State),
    NewState = store(Msg, OpenState),
    ClosedState = close_feed(NewState),
    {reply, ok, ClosedState};

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
    {reply, Msg, State};

handle_call({foldl, Fun, Acc}, _From, #state{feed = Feed} = State) ->
    Result =
        case file:open(Feed, [read, binary]) of
            {ok, IoDev} ->
                int_foldr(Fun, Acc, IoDev);
            {error, enoent} ->
                ?info("Ill formed feed ~n",[]),
                done
        end,

    {reply, Result, State}.


handle_cast(_Request, State) ->
    {noreply, State}.

%% info

handle_info(Info, State) ->
    ?info("WTF: ~p ~n",[Info]),
    {noreply, State}.

%%

terminate(Reason, #state{feed_open = IsOpen} = State) ->
    ?info("Closed gen_server: ~p ~n",[Reason]),
    if IsOpen ->
            close_feed(State);
       true ->
            nop
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
store(Msg, #state{id = Id,
                feed_open = true,
                feed = Feed,
                feed_file = F,
                meta = Meta,
                meta_file = M} = State) ->
    write_msg(Msg, F, Feed),
    IsAbout = is_about(Msg, Id),
    if IsAbout ->
            write_msg(Msg, M, Meta);
       true ->
            nop
    end,
    State;

store(_Msg, #state{feed_open = false} = State) ->
    State.


write_msg(Msg, O, Store) ->
    DataSiz = size(Msg),
    ok = file:write(O,
               <<DataSiz:32, Msg/binary, DataSiz:32>>),
    FileSize = filelib:file_size(Store) + 4,
    ok = file:write(O, <<FileSize:32>>).

is_about(Msg, Id) ->
    {DecProps} = jiffy:decode(Msg),
    {Value} = ?pgv(<<"value">>, DecProps),
    Content = ?pgv(<<"content">>, Value),
    case is_binary(Content) of
        true ->
            %% this is encrypted
            false;
        _Else ->
            {ContentProps} = Content,
            Type = ?pgv(<<"type">>, ContentProps),
            About = ?pgv(<<"about">>, ContentProps),
            case Type of
                undefined ->
                    false;
                Type ->
                    (Type == <<"about">>) andalso
                        (About == Id)
            end
    end.

init_directories(AuthDir, Location) ->
    %% Author is already decoded as hex, use first two chars for directory
    <<Dir:2/binary,RestAuth/binary>> = AuthDir,
    FeedDir = <<Location/binary,Dir/binary,<<"/">>/binary,RestAuth/binary>>,
    Feed = <<FeedDir/binary,<<"/">>/binary,<<"log.offset">>/binary>>,
    Meta = <<FeedDir/binary,<<"/">>/binary,<<"meta">>/binary>>,
    filelib:ensure_dir(Feed),
    filelib:ensure_dir(Meta),
    {Feed, Meta}.

feed_get(Feed, [], Key) ->
    ?info("opening ~p ~n",[Feed]),
    case file:open(Feed, [read, binary]) of
        {ok, IoDev} ->
            scan(IoDev, 0, Key);
        {error, enoent} ->
            ?info("Probably bad input ~n",[]),
            done
    end;

feed_get(Feed, [{Key, Pos}], Key) ->
    case file:open(Feed, [read, binary]) of
        {ok, IoDev} ->
            file:position(IoDev, Pos),
            scan(IoDev, Pos, Key);
        {error, enoent} ->
            ?info("Probably bad input ~n",[]),
            done
    end.

scan(IoDev, Pos, Key) ->
    case load_term(IoDev) of
        {ok, Data} ->
            {DataProps} = jiffy:decode(Data),
            KeyVal = ?pgv(<<"key">>, DataProps),
            if KeyVal == Key ->
                    {Pos, Data};
               true ->
                    {ok, <<NextPos:32/integer>>} = file:read(IoDev, 4),
                    scan(IoDev, NextPos, Key)
            end;
        {error, eof} ->
            ?info("Key not found: ~p ~n",[Key]),
            not_found;
        {error, Error} ->
            ?info("Error ~p scanning for key: ~p ~n",[Error, Key])
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


load_term(IoDev) ->
    case file:read(IoDev, 4) of
        {ok, <<TermLenInt:32/integer>>} ->
            case file:read(IoDev, TermLenInt) of
                {ok, TermData} ->
                    check_data(IoDev, TermData, TermLenInt);
                {error, Reason} ->
                    {error, Reason}
            end;
        eof ->
            {error, eof};
        {error, Reason} ->
            {error, Reason}
    end.


check_data(IoDev, Data, Len) ->
    case file:read(IoDev, 4) of
        {ok, TermLen} ->
            <<TermLenInt:32/integer>> = TermLen,
            %% the length of the term is also stored at the end of the term
            %% and can be used to check
            if TermLenInt == Len ->
                    {ok, Data};
               true ->
                    {error, data_size_no_match}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

open_feed(#state{feed_open = false,
                 feed = Feed,
                 meta = Meta} = State) ->
    FileOpen = file:open(Feed, [append, sync]),
    MetaOpen = file:open(Meta, [append, sync]),
    case {FileOpen, MetaOpen} of
        {{ok, F}, {ok, M}} ->
            State#state{feed_open = true,
                feed_file = F,
                meta_file = M};
        Else ->
            ?info("Tried to open failed: ~p ~n",[Else]),
            State
    end.

close_feed(#state{feed_open = true,
                  feed_file = F,
                  meta_file = M} = State) ->
    ok = file:close(F),
    ok = file:close(M),
    State#state{feed_open = false,
                feed_file = nil,
                meta_file = nil};
close_feed(#state{feed_open = false} = State) ->
    State.


-ifdef(TEST).

-endif.
