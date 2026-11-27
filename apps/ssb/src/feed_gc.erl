%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Feed garbage collection — the PLAN half.
%%
%% A node stores every feed that was ever in its replication set, and the
%% set moves: an unfollow, a friend's unfollow two hops out, a smaller
%% `replication_hops`, a room member who left.  Nothing ever removes the
%% feeds that fell out.  This module works out which ones those are and
%% what removing them would touch.  It removes nothing.
%%
%% WHAT IS KEPT.  Exactly what ebt replicates (ebt:compute_repl_set/0:
%% self ∪ follows(self, hops) ∪ room members ∪ pins − blocks), plus self
%% and every pin unconditionally.  Sharing the computation with ebt means
%% "what we replicate" and "what we keep" cannot drift apart.
%%
%% NO CASCADE.  The follow graph out to N hops reads only the contact
%% messages of feeds within N−1 hops, all of which are kept.  Removing
%% feeds outside the set therefore cannot shrink the set, so one pass is
%% final and a second plan straight after a run finds nothing.
%%
%% GRACE.  The graph flaps — a friend unfollows and refollows, a room
%% member reconnects — and history that is removed may not be fetchable
%% again (peers drop feeds too, and peers that persist what they think we
%% hold will not resend it).  So a feed becomes *eligible* only after it
%% has been a candidate for `grace_days`.  The plan records when each
%% candidate was first seen outside the set in a small ledger,
%% `<data>/gc/candidates`; a feed that comes back into the set leaves the
%% ledger and starts over if it falls out again.  That ledger is the only
%% thing a plan writes, and `record => false` skips even that.
%%
%% REFUSALS.  The graph and pin readers answer [] when the store cannot be
%% read — right for replication, which just replicates less for a while,
%% and exactly wrong here, where an empty keep set means "everything is
%% garbage".  So the plan checks the store itself first and refuses
%% rather than guessing: store unavailable, follow graph not caught up, a
%% table unreadable, or a keep set holding nobody but ourselves while
%% replication_hops > 0.
%%
%% WHAT A RUN WILL HAVE TO TOUCH, and so what the plan counts:
%%   - the feed directory (live log, archive segments, hints)
%%   - ingest.journal refs — compacted by copy, survivors keep their order
%%   - view_checkpoint rows — must go WITH the feed: a stale checkpoint
%%     makes view_manager drop a re-followed feed's messages as "at or
%%     under the checkpoint"
%%   - feed_floors rows (local truth, not rebuilt by anything)
%%   - feed_meta rows; other view rows are rebuilt, not counted
-module(feed_gc).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([plan/1,
         report_json/1,
         ledger_file/0]).

-define(DEFAULT_GRACE_DAYS, 7).
-define(DEFAULT_TOP, 20).
-define(DAY, 86400).

%%%===================================================================
%%% API
%%%===================================================================

%% Opts (all optional):
%%   grace_days  — days a feed must have been a candidate to be eligible
%%   top         — how many of the largest candidates to list; all | N
%%   record      — update the ledger (default true)
%%   now         — unix seconds, for tests
plan(Opts) ->
    Grace  = maps:get(grace_days, Opts, ?DEFAULT_GRACE_DAYS),
    Top    = maps:get(top, Opts, ?DEFAULT_TOP),
    Record = maps:get(record, Opts, true),
    Now    = maps:get(now, Opts, erlang:system_time(second)),
    try
        ok = preflight(),
        Keep = keep_set(),
        {ok, build(Keep, Grace, Top, Record, Now)}
    catch
        throw:{refuse, Why} -> {error, Why}
    end.

ledger_file() ->
    filename:join([?b2l(config:ssb_repo_loc()), "gc", "candidates"]).

%%%===================================================================
%%% Preflight and the keep set
%%%===================================================================

preflight() ->
    ssb_store:available() orelse throw({refuse, store_unavailable}),
    view_manager:caught_up(ssb_social_graph)
        orelse throw({refuse, {not_caught_up, ssb_social_graph}}),
    %% reachable at all, and with the table the graph is built from
    _ = checked_q("SELECT count(*) FROM social_edges"),
    ok.

keep_set() ->
    Self = keys:pub_key_disp(),
    Repl = ebt:compute_repl_set(),
    case config:replication_hops() > 0 andalso lists:delete(Self, Repl) =:= [] of
        true  -> throw({refuse, empty_keep_set});
        false -> ok
    end,
    %% Pins read directly, checked: feed_pins:all/0 answers [] on error.
    Pins = [F || [F] <- checked_q("SELECT feed FROM feed_pin")],
    sets:from_list([Self | Repl] ++ Pins, [{version, 2}]).

checked_q(Sql) ->
    try ssb_store:q(Sql) of
        L when is_list(L) -> L;
        Other             -> throw({refuse, {store_read, Sql, Other}})
    catch
        throw:T     -> throw(T);
        _:Reason    -> throw({refuse, {store_read, Sql, Reason}})
    end.

%%%===================================================================
%%% The plan
%%%===================================================================

build(Keep, Grace, Top, Record, Now) ->
    {Feeds, Unrecognised} = feeds_on_disk(),
    {Candidates, Kept} = lists:partition(
                           fun({Id, _Dir}) -> not sets:is_element(Id, Keep) end,
                           Feeds),
    OldLedger = read_ledger(ledger_file()),
    {Ledger, EligibleIds} = classify([Id || {Id, _} <- Candidates],
                                     OldLedger, Now, Grace),
    Recorded = Record andalso write_ledger(ledger_file(), Ledger, Now) =:= ok,

    Stats = [feed_stats(Id, Dir) || {Id, Dir} <- Candidates],
    {JournalTotal, Refs} = journal_refs([Id || {Id, _} <- Candidates]),
    Ckpts  = count_by_feed("SELECT feed, count(*) FROM view_checkpoint"
                           " GROUP BY feed"),
    Meta   = count_by_feed("SELECT feed, count(*) FROM feed_meta"
                           " GROUP BY feed"),
    Floors = count_by_feed("SELECT feed, 1 FROM feed_floors"),
    Blocked = sets:from_list(ssb_social_graph:blocks(keys:pub_key_disp()),
                             [{version, 2}]),

    Totals = fun(Ids) ->
                 #{feeds       => sets:size(Ids),
                   bytes       => sum(Ids, Stats, bytes),
                   messages    => sum(Ids, Stats, seq),
                   journal_refs => sum_map(Ids, Refs),
                   checkpoints => sum_map(Ids, Ckpts),
                   floors      => sum_map(Ids, Floors),
                   feed_meta   => sum_map(Ids, Meta),
                   blocked     => sets:size(sets:intersection(Ids, Blocked))}
             end,
    CandSet = sets:from_list([Id || {Id, _} <- Candidates], [{version, 2}]),
    EligSet = sets:from_list(EligibleIds, [{version, 2}]),

    #{at            => Now,
      hops          => config:replication_hops(),
      grace_days    => Grace,
      keep_set      => sets:size(Keep),
      feeds_on_disk => length(Feeds),
      kept_on_disk  => length(Kept),
      kept_bytes    => lists:sum([dir_bytes(D) || {_, D} <- Kept]),
      unrecognised_dirs => Unrecognised,
      journal_refs  => JournalTotal,
      candidates    => Totals(CandSet),
      eligible      => Totals(EligSet),
      ledger        => ?l2b(ledger_file()),
      recorded      => Recorded,
      top           => top(Stats, Ledger, EligSet, Grace, Now, Top)}.

%% Which candidates are eligible, and the ledger to carry forward: a
%% candidate keeps its first-seen time, a new one starts now, and a feed
%% that is no longer a candidate drops out.
classify(CandidateIds, OldLedger, Now, Grace) ->
    Ledger = maps:from_list([{Id, maps:get(Id, OldLedger, Now)}
                             || Id <- CandidateIds]),
    Eligible = [Id || Id := First <- Ledger, Now - First >= Grace * ?DAY],
    {Ledger, Eligible}.

%%%===================================================================
%%% Feeds on disk
%%%===================================================================

feeds_on_disk() ->
    lists:foldr(fun(Dir, {Ok, Bad}) ->
                        case dir_feed_id(Dir) of
                            {ok, Id} -> {[{Id, Dir} | Ok], Bad};
                            error    -> {Ok, Bad + 1}
                        end
                end, {[], 0}, feed_store:feed_dirs()).

%% The inverse of utils:feed_dir/1, which names a feed's directory by the
%% key in hex, split after two characters — hex WITHOUT leading zeros
%% (integer_to_binary(_, 16)), so a key starting 0x0… has a short name
%% and the digits are padded back to 64 before decoding.
dir_feed_id(Dir) ->
    Hex = filename:basename(filename:dirname(Dir)) ++ filename:basename(Dir),
    case length(Hex) =< 64 andalso
         lists:all(fun(C) -> lists:member(C, "0123456789ABCDEF") end, Hex) of
        true ->
            Padded = lists:duplicate(64 - length(Hex), $0) ++ Hex,
            Key = binary:decode_hex(?l2b(Padded)),
            {ok, utils:display_pub(base64:encode(Key))};
        false ->
            error
    end.

feed_stats(Id, Dir) ->
    {Seq, Ts} = case feed_store:last_frame(Dir) of
                    {ok, Msg} ->
                        try message:decode(Msg, false) of
                            #message{sequence = S, timestamp = T}
                              when is_integer(S) -> {S, T};
                            _ -> {undefined, undefined}
                        catch _:_ -> {undefined, undefined}
                        end;
                    _ -> {undefined, undefined}
                end,
    #{id => Id, bytes => dir_bytes(Dir), seq => Seq, last => Ts}.

dir_bytes(Dir) ->
    case file:list_dir(Dir) of
        {ok, Names} ->
            lists:sum([filelib:file_size(filename:join(Dir, N)) || N <- Names]);
        _ ->
            0
    end.

%%%===================================================================
%%% Journal and derived rows
%%%===================================================================

%% {all refs, #{CandidateId => refs}}.  One pass over the journal.
journal_refs(CandidateIds) ->
    Init = maps:from_keys(CandidateIds, 0),
    ingest_journal:fold_refs(
      fun({F, _Seq}, {T, M}) ->
              case M of
                  #{F := N} -> {T + 1, M#{F := N + 1}};
                  _         -> {T + 1, M}
              end
      end, {0, Init}).

count_by_feed(Sql) ->
    maps:from_list([{F, N} || [F, N] <- checked_q(Sql)]).

sum(Ids, Stats, Key) ->
    lists:sum([V || #{id := Id, Key := V} <- Stats,
                    is_integer(V), sets:is_element(Id, Ids)]).

sum_map(Ids, Map) ->
    sets:fold(fun(Id, Acc) -> Acc + maps:get(Id, Map, 0) end, 0, Ids).

top(Stats, Ledger, EligSet, Grace, Now, Top) ->
    Sorted = lists:sort(fun(#{bytes := A}, #{bytes := B}) -> A >= B end, Stats),
    Some = case Top of
               all -> Sorted;
               N when is_integer(N), N >= 0 -> lists:sublist(Sorted, N)
           end,
    [S#{name       => name(Id),
        first_seen => maps:get(Id, Ledger, Now),
        eligible   => sets:is_element(Id, EligSet),
        grace_left => max(0, Grace * ?DAY - (Now - maps:get(Id, Ledger, Now)))}
     || #{id := Id} = S <- Some].

name(Id) ->
    case ssb_feed_meta:name(Id) of
        N when is_binary(N) -> N;
        _                   -> undefined
    end.

%%%===================================================================
%%% Ledger
%%%===================================================================

%% One line per candidate: "<feed id> <first seen, unix seconds>".
%% Anything unparseable is skipped — the worst a damaged ledger can do is
%% restart some grace periods, which errs toward keeping.
read_ledger(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            maps:from_list(
              [{Id, binary_to_integer(T)}
               || Line <- binary:split(Bin, ~"\n", [global]),
                  [Id, T] <- [binary:split(Line, ~" ")],
                  is_feed_id(Id), is_digits(T)]);
        _ ->
            #{}
    end.

write_ledger(File, Ledger, Now) ->
    Tmp = File ++ ".tmp",
    Body = [io_lib:format("# feed_gc candidates, written ~b: "
                          "<feed id> <first seen outside the keep set>~n",
                          [Now])
            | [[Id, $\s, integer_to_binary(T), $\n]
               || {Id, T} <- lists:sort(maps:to_list(Ledger))]],
    try
        ok = filelib:ensure_dir(File),
        ok = file:write_file(Tmp, Body),
        ok = file:rename(Tmp, File)
    catch _:Reason ->
        ?SSB_ERROR("feed_gc: could not write ~s: ~p", [File, Reason]),
        error
    end.

is_feed_id(<<"@", _/binary>> = Id) -> binary:longest_common_suffix([Id, ~".ed25519"]) =:= 8;
is_feed_id(_) -> false.

is_digits(<<>>) -> false;
is_digits(B)    -> lists:all(fun(C) -> C >= $0 andalso C =< $9 end, ?b2l(B)).

%%%===================================================================
%%% JSON for admin.gc.plan
%%%===================================================================

report_json(R) ->
    {[{~"at",              maps:get(at, R)},
      {~"hops",            maps:get(hops, R)},
      {~"graceDays",       maps:get(grace_days, R)},
      {~"keepSet",         maps:get(keep_set, R)},
      {~"feedsOnDisk",     maps:get(feeds_on_disk, R)},
      {~"keptOnDisk",      maps:get(kept_on_disk, R)},
      {~"keptBytes",       maps:get(kept_bytes, R)},
      {~"unrecognisedDirs", maps:get(unrecognised_dirs, R)},
      {~"journalRefs",     maps:get(journal_refs, R)},
      {~"candidates",      totals_json(maps:get(candidates, R))},
      {~"eligible",        totals_json(maps:get(eligible, R))},
      {~"ledger",          maps:get(ledger, R)},
      {~"recorded",        maps:get(recorded, R)},
      {~"top",             [feed_json(F) || F <- maps:get(top, R)]}]}.

totals_json(T) ->
    {[{~"feeds",       maps:get(feeds, T)},
      {~"bytes",       maps:get(bytes, T)},
      {~"messages",    maps:get(messages, T)},
      {~"journalRefs", maps:get(journal_refs, T)},
      {~"checkpoints", maps:get(checkpoints, T)},
      {~"floors",      maps:get(floors, T)},
      {~"feedMeta",    maps:get(feed_meta, T)},
      {~"blocked",     maps:get(blocked, T)}]}.

feed_json(F) ->
    {[{~"id",        maps:get(id, F)},
      {~"name",      null_if_undefined(maps:get(name, F))},
      {~"bytes",     maps:get(bytes, F)},
      {~"seq",       null_if_undefined(maps:get(seq, F))},
      {~"last",      null_if_undefined(maps:get(last, F))},
      {~"firstSeen", maps:get(first_seen, F)},
      {~"eligible",  maps:get(eligible, F)},
      {~"graceLeft", maps:get(grace_left, F)}]}.

null_if_undefined(undefined) -> null;
null_if_undefined(V)         -> V.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

-define(A, ~"@AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=.ed25519").
-define(B, ~"@BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBA=.ed25519").
-define(C, ~"@CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCA=.ed25519").

%% Every key, including ones whose hex name lost leading zeros, maps back
%% from the directory utils:feed_dir/1 gives it.
dir_round_trip_test_() ->
    [?_test(round_trip(<<0:8, 0:4, 1:4, (crypto:strong_rand_bytes(30))/binary>>)),
     ?_test(round_trip(<<0:4, 7:4, (crypto:strong_rand_bytes(31))/binary>>)),
     ?_test(round_trip(<<255, (crypto:strong_rand_bytes(31))/binary>>)),
     ?_test(round_trip(crypto:strong_rand_bytes(32)))].

round_trip(Key) ->
    Id = utils:display_pub(base64:encode(Key)),
    %% utils:feed_dir/1 prefixes config:feed_loc(); only the last two path
    %% components matter, so build them the same way it does.
    <<D2:2/binary, Rest/binary>> = utils:decode_id(Id),
    Dir = filename:join(["/x/feeds", ?b2l(D2), ?b2l(Rest)]),
    ?assertEqual({ok, Id}, dir_feed_id(Dir)).

dir_not_a_feed_test() ->
    ?assertEqual(error, dir_feed_id("/x/feeds/zz/notHex")),
    ?assertEqual(error, dir_feed_id("/x/feeds/AB/" ++ lists:duplicate(70, $C))).

classify_grace_test() ->
    Now = 1_000_000,
    Old = #{?A => Now - 8 * ?DAY,       %% candidate for 8 days
            ?B => Now - 1 * ?DAY,       %% for 1 day
            ?C => Now - 30 * ?DAY},     %% was a candidate, now kept again
    {Ledger, Elig} = classify([?A, ?B, <<"@new">>], Old, Now, 7),
    ?assertEqual(#{?A => Now - 8 * ?DAY, ?B => Now - ?DAY, <<"@new">> => Now},
                 Ledger),
    ?assertEqual([?A], Elig).

classify_zero_grace_test() ->
    {_, Elig} = classify([?A, ?B], #{}, 5, 0),
    ?assertEqual([?A, ?B], lists:sort(Elig)).

ledger_round_trip_test() ->
    File = filename:join(["/tmp", "feed_gc_test_" ++
                              integer_to_list(erlang:unique_integer([positive])),
                          "candidates"]),
    L = #{?A => 100, ?B => 200},
    ok = write_ledger(File, L, 300),
    ?assertEqual(L, read_ledger(File)),
    %% damaged lines are skipped, not fatal
    {ok, Bin} = file:read_file(File),
    ok = file:write_file(File, [Bin, "garbage\n@x 12\n", ?C, " notanumber\n"]),
    ?assertEqual(L, read_ledger(File)),
    ok = file:delete(File),
    ?assertEqual(#{}, read_ledger(File)),
    _ = file:del_dir(filename:dirname(File)).

-endif.
