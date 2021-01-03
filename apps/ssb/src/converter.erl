%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2020 Dionne Associates, LLC.

-module(converter).

%% converter is a utility that takes an existing log.offset file from the
%% javascript reference implementation of scuttlebutt and produces separate
%% feeds for each author in the log. Each feed is stored as a log file with the
%% same format, but in it's own directory along with a meta file

-include("ssb.hrl").

-export([convert/1]).

convert(OffsetLog)->
    %% create initial store if needed, this info will come from config or
    %% environment at build time
    {ok, [[Home]]} = init:get_argument(home),
    File = Home ++ "/emacs/erlbutt/" ++ OffsetLog,
    DataStore = ?l2b(Home ++ "/.ssberl/feeds/"),
    file:make_dir(DataStore),

    case file:open(File, [read, binary]) of
        {ok, IoDev} ->
            convert_terms(IoDev, 0, DataStore),
            file:close(IoDev),
            ?info("There are ~p ids in this log ~n",[length(get())]),
            {BiggestId, NoMsgs} =
                lists:foldl(fun(Elem, Acc) ->
                                    case count(Elem) > count(Acc) of
                                        true -> Elem;
                                        _Else -> Acc
                                    end
                            end,{<<"FFF">>, {0, 0}},get()),
            ?info("The largest feed belongs to: ~p ~n",
                  [{BiggestId, NoMsgs}]);
        {error, enoent} ->
            ?info("Probably bad input ~n",[]),
            done
    end.

convert_terms(IoDev, Found, DataStore) ->
    case load_term(IoDev) of
        {ok, Data} ->
            store(Data, DataStore),
            SleepCnt = Found rem 500 == 0,
            if SleepCnt ->
                    timer:sleep(500),
                    ?info("Sleeping... ~n", []);
               true ->
                    true
            end,
            %% read spacer in file, at end this will cause eof but that will be picked
            %% up in the next iteration
            {ok, <<_PosInt:32/integer>>} = file:read(IoDev, 4),
            %%?info("The pos is ~p ~n",[PosInt]),
            convert_terms(IoDev, Found + 1, DataStore);
        {error, eof} ->
            ?info("Found ~p messages ~n",[Found]),
            done;
        {error, Error} ->
            ?info("Error loading the ~p term: ~p ~n",[Found, Error])
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

count(Element)->
    element(2, element(2, Element)).

extract_author(DataProps) ->
    {Value} = ?pgv(<<"value">>, DataProps),
    <<"@",Id/binary>> = ?pgv(<<"author">>, Value),
    hd(string:replace(Id,".ed25519","")).

get_feed(Author, Location) ->
    Val = get(Author),
    case Val of
        undefined ->
            {ok, Pid} = ssb_feed2:start_link(Author, Location),
            put(Author, {Pid, 1}),
            Pid;
        {Pid, Count} ->
            put(Author, {Pid, Count + 1}),
            PrintCount = Count rem 1000 == 0,
            if PrintCount ->
                    timer:sleep(500),
                    ?info("This author ~p has ~p records ~n", [Author, Count]);
               true ->
                    true
            end,
            Pid;
        _Else ->
            bad
    end.

store(Msg, Location) ->

    {DecProps} = jiffy:decode(Msg),
    AuthId = extract_author(DecProps),

    FeedPid = get_feed(AuthId, Location),
    ssb_feed2:process_msg(FeedPid, Msg).
