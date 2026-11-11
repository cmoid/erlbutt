%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% One running node per data directory.
%%
%% Two nodes on one store is the failure that cannot be undone: both
%% append to the same feed logs and SQLite file, and both sign with the
%% same key, so the identity forks — two different messages at the same
%% sequence, which every peer that sees both treats as a broken feed.
%% It is easy to do by accident: a second `bin/ssb console` in another
%% terminal, a client that spawns its own engine while one is already up,
%% a systemd unit plus a hand-started node.
%%
%% The lock is a file, <data>/LOCK, created with O_EXCL and holding the
%% owner's OS pid, that process's start time, host and node name.  It is
%% removed when the ssb application stops.  A node that dies without
%% stopping (kill -9, power loss) leaves it behind; the next start takes
%% it over when no process with that pid AND that start time is running
%% on this host.
%%
%% The start time is what makes this safe.  A pid alone is reused after a
%% reboot, and the process NAME is no help: a relx node shows as `ssb`, a
%% VM under rebar3 as `./rebar3`, so "is it a beam" misjudges live nodes
%% as dead — and taking over a live node's lock is the fork itself.  A lock from ANOTHER host is never
%% taken over — on a shared filesystem we cannot tell whether that node is
%% alive, and guessing wrong is the fork.
-module(store_lock).
-include_lib("ssb/include/ssb.hrl").

-export([acquire/1,
         release/1,
         holder/1]).

-define(LOCK_FILE, "LOCK").

%% ok | {error, {locked, Holder}} | {error, Reason}
acquire(Dir) ->
    acquire(Dir, 2).

acquire(_Dir, 0) ->
    {error, lock_contended};
acquire(Dir, Tries) ->
    File = lock_file(Dir),
    ok = filelib:ensure_dir(File),
    case file:open(File, [write, exclusive, raw]) of
        {ok, Fd} ->
            try ok = file:write(Fd, io_lib:format("~p.~n", [me()])),
                ok = file:sync(Fd)
            after file:close(Fd)
            end;
        {error, eexist} ->
            Me = os:getpid(),
            case holder(Dir) of
                {live, #{os_pid := Me}} ->
                    %% Ours already: an earlier start in this same VM got
                    %% as far as taking the lock and then failed, so its
                    %% stop/1 never ran.  One VM runs one ssb app.
                    _ = file:delete(File),
                    acquire(Dir, Tries - 1);
                {live, Holder} ->
                    {error, {locked, Holder}};
                {stale, Holder} ->
                    ?SSB_ERROR("store_lock: taking over ~s, left by ~p, "
                               "which is no longer running", [File, Holder]),
                    _ = file:delete(File),
                    acquire(Dir, Tries - 1);
                none ->
                    %% Vanished between the open and the read: its owner
                    %% just stopped.  Try again.
                    acquire(Dir, Tries - 1)
            end;
        {error, _} = Err ->
            Err
    end.

%% Only removes a lock this OS process holds, so a node refused at boot
%% cannot delete the lock of the node that refused it.
release(Dir) ->
    File = lock_file(Dir),
    case read(File) of
        {ok, #{os_pid := Pid, host := Host}} ->
            case {Pid, Host} =:= {os:getpid(), host()} of
                true  -> _ = file:delete(File), ok;
                false -> ok
            end;
        _ ->
            ok
    end.

%% none | {live, Holder} | {stale, Holder}.  An unreadable lock file (a
%% torn write) holds nothing anyone could be running under: stale.
holder(Dir) ->
    File = lock_file(Dir),
    case read(File) of
        {error, enoent} ->
            none;
        {ok, #{os_pid := Pid, host := Host} = Holder} ->
            case Host =:= host() of
                false -> {live, Holder};
                true  ->
                    case running(Pid, maps:get(started, Holder, any)) of
                        true  -> {live, Holder};
                        false -> {stale, Holder}
                    end
            end;
        _ ->
            {stale, #{file => File}}
    end.

%%%===================================================================

lock_file(Dir) ->
    filename:join(Dir, ?LOCK_FILE).

read(File) ->
    case file:consult(File) of
        {ok, [#{} = H]} -> {ok, H};
        {ok, _}         -> {error, bad_lock};
        Err             -> Err
    end.

me() ->
    Pid = os:getpid(),
    #{os_pid => Pid, started => started(Pid), host => host(), node => node()}.

host() ->
    {ok, H} = inet:gethostname(),
    H.

%% Is the process that wrote the lock still running?  `any` (a lock with
%% no start time) can only check that the pid exists, so errs towards
%% live: refusing to start is recoverable, a fork is not.
running(Pid, Started) ->
    case started(Pid) of
        ""  -> false;
        Now -> Started =:= any orelse Now =:= Started
    end.

%% The process's start time as ps prints it, "" if there is no such
%% process.  `lstart` is in both BSD (macOS) and procps (Linux) ps.  It is
%% local time, so TZ and the locale are pinned: the node under systemd and
%% an operator's shell can differ in both, and a live node whose start
%% time merely PRINTS differently would be judged dead.
started(Pid) ->
    string:trim(os:cmd("TZ=UTC LC_ALL=C ps -o lstart= -p " ++ only_digits(Pid)
                       ++ " 2>/dev/null")).

only_digits(Pid) ->
    [C || C <- Pid, C >= $0, C =< $9].

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

with_dir(Fun) ->
    Dir = filename:join("/tmp", "lock_" ++
                        integer_to_list(erlang:unique_integer([positive])) ++
                        "_" ++ integer_to_list(rand:uniform(1 bsl 30))),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    try Fun(Dir) after os:cmd("rm -rf " ++ Dir) end.

write_holder(Dir, H) ->
    ok = file:write_file(lock_file(Dir), io_lib:format("~p.~n", [H])).

acquire_and_release_test() ->
    with_dir(fun(Dir) ->
        ?assertEqual(none, holder(Dir)),
        ok = acquire(Dir),
        ?assertMatch({live, #{node := _}}, holder(Dir)),
        ok = release(Dir),
        ?assertEqual(none, holder(Dir))
    end).

%% A start that failed after taking the lock leaves it held by this same
%% VM; the retry must not be locked out by itself.
reacquire_in_same_vm_test() ->
    with_dir(fun(Dir) ->
        ok = acquire(Dir),
        ok = acquire(Dir),
        ok = release(Dir),
        ?assertEqual(none, holder(Dir))
    end).

%% Another live BEAM on this host holds it: refuse.  A real second VM
%% (peer, over stdio — no distribution needed), because liveness is judged
%% from the OS process table.
live_other_holder_refused_test() ->
    with_dir(fun(Dir) ->
        {ok, Peer, _} = peer:start_link(#{connection => standard_io}),
        try
            OsPid = peer:call(Peer, os, getpid, []),
            write_holder(Dir, #{os_pid => OsPid, started => started(OsPid),
                                host => host(), node => 'other@host'}),
            ?assertMatch({error, {locked, #{node := 'other@host'}}},
                         acquire(Dir))
        after
            peer:stop(Peer)
        end
    end).

%% A dead pid is a node that died without stopping.
stale_lock_taken_over_test() ->
    with_dir(fun(Dir) ->
        write_holder(Dir, #{os_pid => "999999999", host => host(),
                            node => 'dead@host'}),
        ?assertMatch({stale, _}, holder(Dir)),
        ok = acquire(Dir),
        ?assertMatch({ok, #{node := _}}, read(lock_file(Dir))),
        ?assertEqual(os:getpid(), maps:get(os_pid, element(2, read(lock_file(Dir))))),
        ok = release(Dir)
    end).

%% A pid that is running, but is not the process that wrote the lock:
%% pid reuse after a reboot.  Pid 1 always exists and never started then.
reused_pid_is_stale_test() ->
    with_dir(fun(Dir) ->
        write_holder(Dir, #{os_pid => "1", started => "Thu Jan  1 00:00:00 1970",
                            host => host(), node => 'x@y'}),
        ?assertMatch({stale, _}, holder(Dir))
    end).

%% Without a start time only existence can be checked; that errs live.
lock_without_start_time_errs_live_test() ->
    with_dir(fun(Dir) ->
        write_holder(Dir, #{os_pid => "1", host => host(), node => 'x@y'}),
        ?assertMatch({live, _}, holder(Dir))
    end).

started_is_stable_test() ->
    ?assertNotEqual("", started(os:getpid())),
    ?assertEqual(started(os:getpid()), started(os:getpid())),
    ?assertEqual("", started("999999999")).

%% Never take over another host's lock: we cannot see whether it runs.
other_host_never_taken_over_test() ->
    with_dir(fun(Dir) ->
        write_holder(Dir, #{os_pid => "999999999", host => "elsewhere.example",
                            node => 'n@elsewhere'}),
        ?assertMatch({error, {locked, _}}, acquire(Dir))
    end).

torn_lock_is_stale_test() ->
    with_dir(fun(Dir) ->
        ok = file:write_file(lock_file(Dir), ~"#{os_pid => \"12"),
        ok = acquire(Dir),
        ok = release(Dir)
    end).

%% release/1 must not delete someone else's lock.
release_leaves_others_lock_test() ->
    with_dir(fun(Dir) ->
        write_holder(Dir, (me())#{os_pid => "999999999"}),
        ok = release(Dir),
        ?assert(filelib:is_regular(lock_file(Dir)))
    end).

-endif.
