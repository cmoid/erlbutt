%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Guards the layering rule of doc/persistence.md §5:
%%
%%   apps/ssb_conv depends on apps/ssb.  Nothing in apps/ssb may call
%%   into apps/ssb_conv.
%%
%% Erlang enforces no app boundary at compile time — a call from the
%% foundation up into the convention layer links and runs perfectly well,
%% and the layering quietly stops being true.  This test is the
%% enforcement.  It is a source scan rather than an xref run because the
%% mistake it catches is textual and obvious, and a cheap check that
%% runs on every `make test` beats a thorough one that nobody wires up.
-module(ssb_layering_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FOUNDATION, "apps/ssb/src").
-define(CONVENTION, "apps/ssb_conv/src").

foundation_does_not_call_conventions_test() ->
    ConvMods = modules_in(?CONVENTION),
    FoundSrc = sources_in(?FOUNDATION),
    %% Fail loudly rather than vacuously passing if run from elsewhere.
    ?assert(length(ConvMods) > 0),
    ?assert(length(FoundSrc) > 0),
    Violations =
        [{filename:basename(File), Mod}
         || File <- FoundSrc,
            Mod  <- ConvMods,
            calls(File, Mod)],
    ?assertEqual([], Violations).

%% Module names (as strings) defined in Dir.
modules_in(Dir) ->
    [filename:basename(F, ".erl") || F <- filelib:wildcard(Dir ++ "/*.erl")].

sources_in(Dir) ->
    filelib:wildcard(Dir ++ "/*.erl").

%% Does File contain a remote call to Mod?  Matches "mod:" but not a
%% mention inside a longer atom (foo_mod:) or in prose without the colon.
calls(File, Mod) ->
    case file:read_file(File) of
        {ok, Bin} ->
            case re:run(Bin, "(^|[^a-zA-Z0-9_])" ++ Mod ++ ":",
                        [{capture, none}, multiline]) of
                match   -> true;
                nomatch -> false
            end;
        _ ->
            false
    end.
