%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2018 Dionne Associates, LLC.
-module(delete_feed).
-include("ssb.hrl").

-export([filter/3]).

filter(Id, InFile, OutFile) ->
    {ok, [[Home]]} = init:get_argument(home),
    File = Home ++ "/ssb-backup/" ++ InFile,
    FilteredFile = Home ++ "/ssb-backup/" ++ OutFile,
    filter_log(File, FilteredFile, Id).

filter_log(InFile, OutFile, IdToFilter) ->
    {ok, In} = file:open(InFile, [read, binary]),
    {ok, Out} = file:open(OutFile, [append]),
    load_terms(In, Out, IdToFilter, 0),
    file:close(In),
    file:close(Out).

load_terms(In, Out, Id, Pos) ->
    case load_term(In) of
        {ok, Data} ->
            DecData = jiffy:decode(Data, [return_maps]),
            FilterId =
                Id == maps:get(<<"author">>,
                               maps:get(<<"value">>, DecData)),
            NewPos =
                if FilterId ->
                        ?info("not write out: ~p at ~p ~n",
                              [maps:get(<<"key">>, DecData), Pos]),
                        Pos;
                   true ->
                        DataSiz = size(Data),
                        file:write(Out, <<DataSiz:32>>),
                        file:write(Out, Data),
                        file:write(Out, <<DataSiz:32>>),
                        Pos + DataSiz + 12
                end,
            file:write(Out, <<NewPos:32>>),
            Continue = case file:read(In, 4) of
                           {ok, <<FileLen:32/integer>>} ->
                               ?info("~p . ~p . ",[FileLen, NewPos]),
                               true;
                           eof ->
                               false
                       end,
            if Continue ->
                    load_terms(In, Out, Id, NewPos);
               true ->
                    done
            end;
        {error, Error} ->
            ?info("Error loading the term: ~p ~n",[Error])
    end.

load_term(IoDev) ->
    case file:read(IoDev, 4) of
        {ok, <<TermLenInt:32/integer>>} ->
            %%?info("Reading term of length: ~p ~n",[TermLenInt]),
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
            if TermLenInt == Len ->
                    {ok, Data};
               true ->
                    {error, data_size_no_match}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
