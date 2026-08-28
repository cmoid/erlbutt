%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% The on-disk data format version, and the refusal to run against data
%% written by a NEWER build than this one.
%%
%% Views already handle both directions: view_manager compares stored and
%% code versions for equality, so a downgrade rebuilds exactly as an
%% upgrade does.  This is for the state that is NOT derived and cannot be
%% rebuilt — the feed logs, the archive segments, the hint files, the
%% ingest journal, and the validation floors.
%%
%% What a rollback costs without this check is specific, silent and
%% expensive:
%%
%%   A floored feed loses its floor.  maybe_restore_floor/1 is what carries
%%   it across a restart; a build without that reads the empty log, reports
%%   sequence 0, and asks peers for the entire history the operator
%%   deliberately declined.
%%
%%   An archived feed can restart at 0.  recover_from_archives/1
%%   reconstructs last_seq from segment content when the live log is
%%   missing — do_archive's crash window.  A build without it re-stores
%%   sequences that already exist, into a feed you own and cannot retract.
%%
%% So the rule is one-directional and deliberate: newer code may read older
%% data, older code may not read newer data.  A node that refuses to start
%% is a problem an operator can solve (run the newer build, or restore the
%% data directory).  A node that starts and quietly re-fetches nine years
%% of history, or forks its own feed, is not.
%%
%% Deliberately depends on almost nothing.  It reads a plain integer from a
%% plain file and computes its path the same way config:default_repo/1
%% does, rather than calling config — the check has to work when the thing
%% that changed is the store, or the config format itself.
-module(data_version).

-export([current/0,
         stored/0,
         check/0,
         file_path/0]).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Bump this when a change makes data written by this build unreadable, or
%% readable but WRONG, to the build before it.  Not for additive changes a
%% previous version simply ignores — a new SQLite table, a new config key,
%% a new message type all leave older builds working, and bumping for them
%% would only teach operators that the refusal is noise.
%%
%% 1 — first released format.
-define(DATA_VERSION, 1).

current() ->
    ?DATA_VERSION.

file_path() ->
    Home = application:get_env(ssb, ssb_home, "."),
    filename:join([Home, ".ssberl", "data_version"]).

%% The version recorded on disk: an integer, or `none` when there is no
%% marker (a fresh node, or one that predates this check).
stored() ->
    case file:read_file(file_path()) of
        {ok, Bin} ->
            try binary_to_integer(string:trim(Bin))
            catch _:_ -> bad
            end;
        _ ->
            none
    end.

%% ok | {error, {data_too_new, Found, Current}}
%%
%% Records the current version whenever it is safe to proceed, so the
%% marker exists from the first release onward and an absent one keeps
%% meaning "older than this check" rather than "unknown".
check() ->
    Cur = current(),
    case stored() of
        Cur ->
            ok;
        none ->
            %% Fresh node, or one installed before the marker existed.
            %% Either way this build can read it: adopt the version.
            write(Cur);
        bad ->
            %% A truncated or hand-edited marker is not evidence of a newer
            %% format, and refusing to boot over a cosmetic problem is worse
            %% than the problem.  Say so and repair it.
            ?SSB_ERROR("data_version: unreadable marker at ~s; "
                       "assuming version ~p and rewriting it",
                       [file_path(), Cur]),
            write(Cur);
        Found when is_integer(Found), Found < Cur ->
            ?SSB_INFO("data_version: upgrading on-disk format ~p -> ~p",
                      [Found, Cur]),
            write(Cur);
        Found ->
            ?SSB_ERROR(
               "data_version: REFUSING TO START.  The data directory was "
               "written by a newer erlbutt (on-disk format ~p; this build "
               "understands ~p).~n"
               "Running an older build against it can lose validation "
               "floors and re-store sequences into your own feed, neither "
               "of which can be undone.~n"
               "Either run the newer build, or restore ~s from backup.",
               [Found, Cur, application:get_env(ssb, ssb_home, ".")]),
            {error, {data_too_new, Found, Cur}}
    end.

write(Version) ->
    Path = file_path(),
    ok = filelib:ensure_dir(Path),
    case file:write_file(Path, [integer_to_list(Version), $\n]) of
        ok ->
            ok;
        {error, Reason} ->
            %% A node that cannot record the marker still runs — the check
            %% is a guard rail, not a dependency.  But it is worth knowing,
            %% because the guard is not armed for the next rollback.
            ?SSB_ERROR("data_version: could not write ~s: ~p", [Path, Reason]),
            ok
    end.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

with_home(Fun) ->
    Home = filename:join("testdata", "dv-" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = filelib:ensure_dir(filename:join([Home, ".ssberl", "x"])),
    Before = application:get_env(ssb, ssb_home),
    application:set_env(ssb, ssb_home, Home),
    try Fun(Home)
    after
        case Before of
            {ok, V} -> application:set_env(ssb, ssb_home, V);
            undefined -> application:unset_env(ssb, ssb_home)
        end,
        _ = file:del_dir_r(Home)
    end.

%% A node with no marker is either fresh or predates the check.  Both are
%% readable by this build, so it adopts the version rather than refusing —
%% otherwise the first release would refuse to start on every node that
%% upgraded into it.
adopts_an_unmarked_directory_test() ->
    with_home(fun(_Home) ->
        ?assertEqual(none, stored()),
        ?assertEqual(ok, check()),
        ?assertEqual(current(), stored())
    end).

matching_version_is_a_no_op_test() ->
    with_home(fun(_Home) ->
        ?assertEqual(ok, check()),
        ?assertEqual(ok, check()),
        ?assertEqual(current(), stored())
    end).

%% Older data, newer code: the supported direction.
older_data_is_upgraded_test() ->
    with_home(fun(_Home) ->
        ok = file:write_file(file_path(), "0\n"),
        ?assertEqual(ok, check()),
        ?assertEqual(current(), stored())
    end).

%% The one that matters.  Newer data, older code: refuse, and do NOT
%% rewrite the marker — a refusal that overwrote the evidence would let the
%% second attempt succeed and do the damage.
newer_data_is_refused_test() ->
    with_home(fun(_Home) ->
        Newer = current() + 1,
        ok = file:write_file(file_path(), integer_to_list(Newer)),
        ?assertMatch({error, {data_too_new, _, _}}, check()),
        ?assertEqual(Newer, stored())
    end).

%% A corrupt marker is not evidence of a newer format, and bricking a node
%% over one would be worse than the problem.
unreadable_marker_is_repaired_test() ->
    with_home(fun(_Home) ->
        ok = file:write_file(file_path(), "not a number"),
        ?assertEqual(bad, stored()),
        ?assertEqual(ok, check()),
        ?assertEqual(current(), stored())
    end).

%% The refusal has to reach the application, not just the module — this is
%% the wiring, and it is what actually stops the node.
app_refuses_to_start_on_newer_data_test() ->
    with_home(fun(_Home) ->
        ok = file:write_file(file_path(), integer_to_list(current() + 1)),
        application:set_env(ssb, port, 0),
        %% ensure_all_started, and match the REASON.  application:start/1
        %% returns {error, {not_started, Dep}} when the dependencies are
        %% not up, which matches a bare {error, _} while proving nothing
        %% about this check — the test passed with the refusal removed.
        %% OTP wraps a start/2 error with the MFA that returned it.
        ?assertMatch({error, {ssb, {{data_too_new, _, _}, {ssb_app, start, _}}}},
                     application:ensure_all_started(ssb)),
        ?assertEqual(undefined, whereis(ssb_sup)),
        application:stop(ssb)
    end).

-endif.
