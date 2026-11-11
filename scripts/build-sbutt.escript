#!/usr/bin/env escript
%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% build-sbutt.escript SRC OUT — compile the source escript SRC into a
%% beam-mode escript at OUT.
%%
%% The release ships OUT, not the source.  A source escript is compiled in
%% memory every time it runs, which is why prod used to need the compiler
%% app; a beam escript needs nothing but the runtime.  The checkout keeps
%% running the source directly (./sbutt.escript), so there is still only
%% one file to edit.
%%
%% Run by a rebar3 pre_hook before every `release` and `tar`, so the
%% overlay always copies a build of the current source.  The beam is built
%% by the same OTP that builds the release, which is the ERTS the release
%% bundles, so the two cannot disagree about the beam format.
main([Src, Out]) ->
    case escript:extract(Src, [compile_source]) of
        {ok, Sections} ->
            {source, Beam} = lists:keyfind(source, 1, Sections),
            {emu_args, Emu} = lists:keyfind(emu_args, 1, Sections),
            ok = filelib:ensure_dir(Out),
            ok = escript:create(Out, [shebang,
                                      {comment, "Built from " ++
                                           filename:basename(Src) ++
                                           " by scripts/build-sbutt.escript"},
                                      {emu_args, Emu},
                                      {beam, Beam}]),
            ok = file:change_mode(Out, 8#755);
        {error, Reason} ->
            io:format(standard_error, "build-sbutt: ~s: ~p~n", [Src, Reason]),
            halt(1)
    end;
main(_) ->
    io:format(standard_error, "usage: build-sbutt.escript SRC OUT~n", []),
    halt(2).
