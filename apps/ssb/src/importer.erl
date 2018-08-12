%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2018 Dionne Associates, LLC.
-module(importer).
-include("ssb.hrl").

-export([import/3]).

import(Id, OffsetLog, PrintNth)->
    {ok, [[Home]]} = init:get_argument(home),
    File = Home ++ "/ssb-backup/" ++ OffsetLog,

    case file:open(File, [read, binary]) of
        {ok, IoDev} ->
            load_terms(IoDev, Id, 0, PrintNth),
            file:close(IoDev);
        {error, enoent} ->
            ?info("Probably bad input ~n",[]),
            done
    end.

load_terms(IoDev, Id, Found, PrintNth) ->
    case load_term(IoDev) of
        {ok, Data} ->
            {DecDataProps} = jiffy:decode(Data),
            FilterId = (Id == all) orelse
                (Id == extract_author(DecDataProps)),
            NewFound =
                if FilterId ->
                        Msg = message:build_msg(DecDataProps),
                        log(Found, Msg, PrintNth),
                        msg_store:persist(Msg),
                        Found + 1;
                   true ->
                        Found
                end,
            %% read spacer in file, at end this will cause eof but that will be picked
            %% up in the next iteration
            _MaybeDone = file:read(IoDev, 4),
            load_terms(IoDev, Id, NewFound, PrintNth);
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
    ?pgv(<<"author">>, Value).

log(_Found, _Msg, none) ->
    ok;
log(_Found, Msg, all) ->
    ?info("A message: ~p ~n", [Msg]);
log(Found, Msg, Nth) when ((Found rem Nth) == 0) ->
    ?info("A message: ~p ~n", [Msg]);
log(_Found, _Msg, _Nth) ->
    done.
