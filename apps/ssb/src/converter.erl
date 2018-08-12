%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid

-module(converter).

%% converter is a utility that takes an existing log.offset file from the
%% javascript reference implementation of scuttlebutt and produces separate
%% feeds for each author in the log. Each feed is stored as a log file with the
%% same format, but in it's own directory along with a profile file and a references file.

-include_lib("ssb/include/ssb.hrl").

-export([convert/3,
         build_refs/1]).

-import(utils, [load_term/1,
                update_refs/1]).

convert(OffsetLog, Sleep, Feeds)->

    %% create initial store if needed, this info will come from config or
    %% environment at build time
    {ok, [[Home]]} = init:get_argument(home),
    File = Home ++ "/code/erlbutt/" ++ OffsetLog,

    case file:open(File, [read, binary]) of
        {ok, IoDev} ->
            convert_terms(IoDev, 0, Sleep, Feeds),
            file:close(IoDev),
            {BiggestId, NumMsgs} =
                lists:foldl(fun(Elem, Acc) ->
                                    case count(Elem) > count(Acc) of
                                        true -> Elem;
                                        _Else -> Acc
                                    end
                            end,{~"FFF", {0, 0}},get()),
            mess_auth:close(),
            ?LOG_INFO("number of unique feeds: ~p ~n",[length(get())]),
            ?LOG_INFO("largest feed belongs to: ~p ~n",
                  [{BiggestId, NumMsgs}]);
        {error, enoent} ->
            ?LOG_INFO("Probably bad input ~n",[]),
            done
    end.

convert_terms(IoDev, Found, Sleep, Feeds) ->
    case load_term(IoDev) of
        {ok, Data} ->
            store(Data, Sleep, Feeds),
            SleepCnt = Found rem 5000 == 0,
            if SleepCnt ->
                    timer:sleep(Sleep),
                    io:format(".", []);
               true ->
                    true
            end,
            %% read spacer in file, at end this will cause eof but that will be picked
            %% up in the next iteration
            {ok, <<_PosInt:32/integer>>} = file:read(IoDev, 4),
            convert_terms(IoDev, Found + 1, Sleep, Feeds);
        {error, eof} ->
            ?LOG_INFO("Found ~p messages ~n",[Found]),
            done;
        {error, Error} ->
            ?LOG_INFO("Error loading the ~p term: ~p ~n",[Found, Error])
    end.

count({_key, {_Pid, Count}}) ->
    Count;
count(_) ->
    0.

get_feed(Author, Sleep) ->
    Val = get(Author),
    case Val of
        undefined ->
            {ok, Pid} = ssb_feed:start_link(Author),
            put(Author, {Pid, 1}),
            Pid;
        {Pid, Count} when is_integer(Count) ->
            put(Author, {Pid, Count + 1}),
            PrintCount = Count rem 10000 == 0,
            if PrintCount ->
                    timer:sleep(Sleep),
                    io:format("~n",[]),
                    ?LOG_INFO("This author ~p has ~p records ~n", [Author, Count]);
               true ->
                    true
            end,
            Pid;
        _Else ->
            bad
    end.

store(Msg, Sleep, Feeds) ->

    DecMsg =  message:decode(Msg, true),

    #message{id = MsgId,
             author = AuthId,
             validated = Valid} = DecMsg,

    Belongs = lists:member(AuthId, Feeds) orelse
        (hd(Feeds) == all),

    if Belongs ->
            FeedPid = get_feed(AuthId, Sleep),
            ssb_feed:store_msg(FeedPid, DecMsg),
            %% Not needed, ssb_feed_store_msg already handles it
            %%gmess_auth:put(MsgId, AuthId),
            if Valid ->
                    nop;
               true ->
                    io:format("~n",[]),
                    ?LOG_INFO("Stored message that does not validate ~p ~n",[{MsgId, AuthId}])
            end;
            %% need to do this in two passes now, in order to look up message/auth pairs
            %% in the cache.
            %%check_for_refs(DecMsg, Sleep);
       true ->
            nop
    end.

build_refs(FeedId) ->
    {ok, Feed} = ssb_feed:start_link(FeedId),
    Fun = fun(Term, Acc) ->
                  %% possibly no need to validate this here
                  Msg = message:decode(Term, false),
                  [update_refs(Msg) | Acc]
          end,
    ssb_feed:foldl(Feed, Fun, []).
