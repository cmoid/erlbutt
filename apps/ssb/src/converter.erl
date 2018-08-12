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
                                    case element(2, Elem) > element(2, Acc) of
                                        true -> Elem;
                                        _Else -> Acc
                                    end
                            end,{<<"FFF">>, 0},get()),
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

extract_author(DataProps) ->
    {Value} = ?pgv(<<"value">>, DataProps),
    <<"@",Id/binary>> = ?pgv(<<"author">>, Value),
    Author = hd(string:replace(Id,".ed25519","")),
    integer_to_binary(binary:decode_unsigned(base64:decode(Author)),16).

count(Author) ->
    Count = get(Author),
    case Count of
        undefined ->
            put(Author, 1);
        _Else ->
            put(Author, Count + 1),
            PrintCount = Count rem 10000 == 0,
            if PrintCount ->
                    ?info("This author ~p has ~p records ~n", [Author, Count]);
               true ->
                    true
            end

    end.

store(Msg, Location) ->

    {DecProps} = jiffy:decode(Msg),
    AuthDir = extract_author(DecProps),

    count(AuthDir),

    %% Author is already decoded as hex, use first two chars for directory
    <<Dir:2/binary,RestAuth/binary>> = AuthDir,
    file:make_dir(<<Location/binary,Dir/binary>>),
    FeedDir = <<Location/binary,Dir/binary,<<"/">>/binary,RestAuth/binary>>,
    file:make_dir(FeedDir),
    %% write msg to feed
    Feed = <<FeedDir/binary,<<"/">>/binary,<<"log.offset">>/binary>>,
    {ok, Out} = file:open(Feed, [append]),
    DataSiz = size(Msg),
    file:write(Out,
               <<DataSiz:32, Msg/binary, DataSiz:32>>),
    FileSize = filelib:file_size(Feed),
    file:write(Out, <<FileSize:32>>),
    file:close(Out).
