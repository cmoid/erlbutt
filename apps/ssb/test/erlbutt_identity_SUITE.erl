%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% One store, several identities, the node stopped by a signal.
%%
%% Clients that offer more than one identity (HighWire) switch by sending
%% the engine SIGTERM, rewriting .ssberl/secret and starting it again over
%% the same store.  Nothing in erlbutt names "the" identity outside the
%% secret, so each switch is a cold boot as someone else.  The damage this
%% can do is a fork: a feed published past a sequence it already holds, or
%% onto a predecessor that is not its real last message.
%%
%% Every node here is a real OS process (OTP `peer`) stopped by a real
%% signal, because the guarantees under test are about what is on disk
%% when the VM goes away, not about orderly gen_server shutdown.
-module(erlbutt_identity_SUITE).

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([identity_switch_round_trip_test/1,
         sigterm_during_publish_test/1,
         sigkill_during_publish_test/1,
         lost_tail_refuses_then_recovers_test/1,
         restored_secret_catches_up_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ssb/include/ssb.hrl").

-define(PORT, 18120).
-define(PUB_PORT, 18121).

all() ->
    [identity_switch_round_trip_test,
     sigterm_during_publish_test,
     sigkill_during_publish_test,
     lost_tail_refuses_then_recovers_test,
     restored_secret_catches_up_test].

init_per_suite(Config) ->
    ssb_ct_helper:ensure_distributed(),
    [{pas, ssb_ct_helper:pa_args()} | Config].

end_per_suite(Config) ->
    Config.

%% A fresh store per case: these tests are about one store's history.
init_per_testcase(Case, Config) ->
    Dir = filename:join(?config(priv_dir, Config), atom_to_list(Case)),
    ok = filelib:ensure_dir(Dir ++ "/"),
    [{dir, Dir} | Config].

%% A failed case can leave its node running on the store and the port.
end_per_testcase(_Case, Config) ->
    [catch signal(N, "KILL") || N <- nodes(), lists:prefix("ident_", atom_to_list(N))],
    Config.

%%% Tests ---------------------------------------------------------------

%% Post as A, switch to B, post, switch back to A, post.  A's feed must
%% pick up exactly where it left off (seq 4 onto A3), B's must be
%% untouched, and both must be single valid chains.
identity_switch_round_trip_test(Config) ->
    Dir = ?config(dir, Config),
    Node1 = boot(Config),
    A = rpc:call(Node1, keys, pub_key_disp, []),
    SecretFile = binary_to_list(rpc:call(Node1, config, ssb_repo_loc, [])) ++ "secret",
    [ok = post(Node1, A, <<"A", (integer_to_binary(N))/binary>>) || N <- [1, 2, 3]],
    #message{id = A3} = last(Node1, A),
    {ok, ASecret} = file:read_file(SecretFile),
    signal(Node1, "TERM"),

    {B, BSecret} = new_secret(),
    ok = file:write_file(SecretFile, BSecret),
    Node2 = boot(Config),
    ?assertEqual(B, rpc:call(Node2, keys, pub_key_disp, [])),
    %% A is held, but no longer ours to sign
    ?assertEqual(3, seq(Node2, A)),
    ?assertEqual(no_post, post(Node2, A, ~"A as B")),
    [ok = post(Node2, B, <<"B", (integer_to_binary(N))/binary>>) || N <- [1, 2]],
    signal(Node2, "TERM"),

    ok = file:write_file(SecretFile, ASecret),
    Node3 = boot(Config),
    ?assertEqual(A, rpc:call(Node3, keys, pub_key_disp, [])),
    ok = post(Node3, A, ~"A4"),
    #message{sequence = 4, previous = A3} = last(Node3, A),

    ?assertEqual(4, valid_chain(Node3, A)),
    ?assertEqual(2, valid_chain(Node3, B)),
    signal(Node3, "TERM"),
    ct:log("round trip ok in ~s", [Dir]).

%% HighWire's switch is SIGTERM.  Whatever a publish acknowledged before
%% the signal must still be there after it, and the next post must chain
%% onto the real last message.
sigterm_during_publish_test(Config) ->
    killed_during_publish(Config, "TERM").

%% No orderly shutdown at all.  A process kill does not lose what reached
%% the OS (one frame is one write), so the same guarantees hold.
sigkill_during_publish_test(Config) ->
    killed_during_publish(Config, "KILL").

%% The fork guard, from the disk alone.  Our own log lost its last two
%% messages (a power cut after peers had them), but the own-head record
%% outlived them: the node boots BEHIND, refuses to publish with no peer
%% in sight, and only publishes again once a pub that holds the feed has
%% given the missing messages back — at seq 6, onto the real 5.
lost_tail_refuses_then_recovers_test(Config) ->
    Dir = ?config(dir, Config),
    Node1 = boot(Config),
    A = rpc:call(Node1, keys, pub_key_disp, []),
    Log = log_file(Node1, A),
    [ok = post(Node1, A, <<"A", (integer_to_binary(N))/binary>>) || N <- lists:seq(1, 5)],
    #message{id = A5} = last(Node1, A),
    Pub = start_pub(Config, Node1, A),
    signal(Node1, "TERM"),

    ok = cut_log(Log, 3),
    Node2 = boot(Config),
    ?assertEqual({behind, 3, 5}, own_status(Node2)),
    ?assertEqual({error, {own_feed_behind, 3, 5}}, post(Node2, A, ~"forks at 4")),
    ?assertEqual(3, seq(Node2, A)),

    %% No EBT on this connection, so the pull alone must do it: an
    %% explicit createHistoryStream from our tail, which is what gets
    %% past an ssb-ebt pub's cached belief about what we hold.
    _ = connect_to(Node2, Pub, ?PUB_PORT, no_ebt),
    ok = rpc:call(Node2, own_feed, pull_now, []),
    ?assert(wait_until(fun() -> own_status(Node2) =:= {confident, 5, 5} end, 80)),
    %% the boot-time pull (no peers yet) may still be the latest for a moment
    ?assert(wait_until(fun() ->
                               rpc:call(Node2, own_feed, last_pull, []) =:=
                                   #{peers => 1, stored => 2}
                       end, 40)),
    ok = post(Node2, A, ~"A6"),
    #message{sequence = 6, previous = A5} = last(Node2, A),
    ?assertEqual(6, valid_chain(Node2, A)),
    signal(Node2, "TERM"),
    signal(Pub, "TERM"),
    ct:log("lost tail recovered in ~s", [Dir]).

%% The patchwork case: the secret, and nothing else.  An empty store
%% cannot know its feed has history, so it boots unconfirmed; the pub's
%% note says the feed reaches 5, which puts it behind; it pulls its own
%% feed back and is confident at 5 before anything new is signed.
restored_secret_catches_up_test(Config) ->
    Dir = ?config(dir, Config),
    Node1 = boot(Config, filename:join(Dir, "original"), ?PORT),
    A = rpc:call(Node1, keys, pub_key_disp, []),
    [ok = post(Node1, A, <<"A", (integer_to_binary(N))/binary>>) || N <- lists:seq(1, 5)],
    #message{id = A5} = last(Node1, A),
    Secret = binary_to_list(rpc:call(Node1, config, ssb_repo_loc, [])) ++ "secret",
    Pub = start_pub(Config, Node1, A),
    signal(Node1, "TERM"),

    Restored = filename:join([Dir, "restored", ".ssberl", "secret"]),
    ok = filelib:ensure_dir(Restored),
    {ok, _} = file:copy(Secret, Restored),
    Node2 = boot(Config, filename:join(Dir, "restored"), ?PORT),
    ?assertEqual(A, rpc:call(Node2, keys, pub_key_disp, [])),
    ?assertEqual({unconfirmed, 0, 0}, own_status(Node2)),

    connect(Node2, Pub),
    ?assert(wait_until(fun() -> own_status(Node2) =:= {confident, 5, 5} end, 80)),
    ok = post(Node2, A, ~"A6"),
    #message{sequence = 6, previous = A5} = last(Node2, A),
    ?assertEqual(6, valid_chain(Node2, A)),
    signal(Node2, "TERM"),
    signal(Pub, "TERM").

%%% Helpers -------------------------------------------------------------

%% A pub (its own identity, its own store) that follows A and has
%% replicated all of A's feed from Node.
start_pub(Config, Node, A) ->
    Pub = boot(Config, filename:join(?config(dir, Config), "pub"), ?PUB_PORT),
    PubId = rpc:call(Pub, keys, pub_key_disp, []),
    ok = post(Pub, PubId, {[{~"type", ~"contact"}, {~"contact", A}, {~"following", true}]}),
    ok = rpc:call(Pub, ebt, refresh_repl_set, []),
    Want = seq(Node, A),
    PeerPid = connect_to(Pub, Node, ?PORT),
    ?assert(wait_until(fun() -> seq(Pub, A) =:= Want end, 80)),
    rpc:call(Pub, gen_server, stop, [PeerPid]),
    Pub.

%% Node dials the pub and starts EBT.
connect(Node, Pub) ->
    _ = connect_to(Node, Pub, ?PUB_PORT),
    ok.

connect_to(From, To, Port) ->
    PeerPid = connect_to(From, To, Port, no_ebt),
    rpc:call(From, ssb_peer, request_ebt, [PeerPid]),
    PeerPid.

connect_to(From, To, Port, no_ebt) ->
    Curve = base64:decode(rpc:call(To, keys, pub_key, [])),
    {ok, PeerPid} = rpc:call(From, ssb_peer, start, ["localhost", Port, Curve]),
    timer:sleep(300),
    PeerPid.

own_status(Node) ->
    rpc:call(Node, own_feed, status, []).

log_file(Node, FeedId) ->
    binary_to_list(rpc:call(Node, utils, feed_dir, [FeedId])) ++ "/log.offset".

%% Keep the first Keep whole frames of a log: the tail a power cut takes.
cut_log(Log, Keep) ->
    {ok, Bin} = file:read_file(Log),
    file:write_file(Log, binary:part(Bin, 0, frames_end(Bin, 0, Keep))).

frames_end(_Bin, Pos, 0) ->
    Pos;
frames_end(Bin, Pos, N) ->
    <<_:Pos/binary, Len:32, _/binary>> = Bin,
    frames_end(Bin, Pos + Len + 12, N - 1).

wait_until(_F, 0) -> false;
wait_until(F, N) ->
    case F() of
        true  -> true;
        false -> timer:sleep(250), wait_until(F, N - 1)
    end.

killed_during_publish(Config, Signal) ->
    Node1 = boot(Config),
    Self = rpc:call(Node1, keys, pub_key_disp, []),
    Pid = rpc:call(Node1, utils, find_or_create_feed_pid, [Self]),
    Ct = self(),
    _ = rpc:call(Node1, erlang, spawn, [fun() -> publish_loop(Ct, Pid, 1) end]),
    %% let the burst get going, so the signal lands mid-write
    Acked = await_acks(50),
    signal(Node1, Signal),
    LastAcked = drain_acks(Acked),
    ct:log("SIG~s after ~p acknowledged posts", [Signal, LastAcked]),

    Node2 = boot(Config),
    Held = seq(Node2, Self),
    ?assert(Held >= LastAcked),
    #message{id = LastId} = last(Node2, Self),
    ok = post(Node2, Self, ~"after the signal"),
    #message{sequence = Next, previous = Prev} = last(Node2, Self),
    ?assertEqual(Held + 1, Next),
    ?assertEqual(LastId, Prev),
    ?assertEqual(Next, valid_chain(Node2, Self)),
    signal(Node2, "TERM").

%% Runs on the peer node.  Reports each acknowledged post to the CT node.
publish_loop(Ct, Pid, N) ->
    ok = ssb_feed:post_content(Pid, <<"burst ", (integer_to_binary(N))/binary>>),
    Ct ! {acked, N},
    publish_loop(Ct, Pid, N + 1).

await_acks(Want) ->
    receive
        {acked, N} when N >= Want -> N;
        {acked, _}                -> await_acks(Want)
    after 30_000 ->
        ct:fail(no_publish_burst)
    end.

drain_acks(Last) ->
    receive {acked, N} -> drain_acks(max(N, Last))
    after 0 -> Last
    end.

boot(Config) ->
    boot(Config, ?config(dir, Config), ?PORT).

boot(Config, Dir, Port) ->
    ok = filelib:ensure_dir(Dir ++ "/"),
    Name = list_to_atom("ident_" ++ integer_to_list(erlang:unique_integer([positive]))),
    {ok, _Peer, Node} = ssb_ct_helper:start_ssb_node(Name, Dir, Port,
                                                     ?config(pas, Config)),
    Node.

%% Signal the node's OS process and wait until it is gone — not just
%% disconnected — so the next boot never shares the store with it.
signal(Node, Sig) ->
    OsPid = rpc:call(Node, os, getpid, []),
    erlang:monitor_node(Node, true),
    _ = os:cmd("kill -" ++ Sig ++ " " ++ OsPid),
    receive {nodedown, Node} -> ok
    after 30_000 -> ct:fail({no_nodedown, Node, Sig})
    end,
    await_exit(OsPid, 300).

await_exit(OsPid, 0) ->
    ct:fail({still_running, OsPid});
await_exit(OsPid, N) ->
    case os:cmd("kill -0 " ++ OsPid ++ " 2>/dev/null && echo alive") of
        "alive" ++ _ -> timer:sleep(100), await_exit(OsPid, N - 1);
        _            -> ok
    end.

feed(Node, FeedId) ->
    rpc:call(Node, utils, find_or_create_feed_pid, [FeedId]).

post(Node, FeedId, Content) ->
    rpc:call(Node, ssb_feed, post_content, [feed(Node, FeedId), Content]).

last(Node, FeedId) ->
    rpc:call(Node, ssb_feed, fetch_last_msg, [feed(Node, FeedId)]).

seq(Node, FeedId) ->
    rpc:call(Node, ssb_feed, current_seq, [feed(Node, FeedId)]).

%% Walk the whole log: one author, sequences 1..N without a gap, each
%% previous the id of the one before, every signature valid.  Returns N.
valid_chain(Node, FeedId) ->
    Raw = lists:reverse(rpc:call(Node, ssb_feed, foldl,
                                 [feed(Node, FeedId), fun(D, Acc) -> [D | Acc] end, []])),
    Msgs = [rpc:call(Node, message, decode, [D, true]) || D <- Raw],
    lists:foldl(
      fun(#message{sequence = Seq, previous = Prev, author = Author,
                   validated = Valid, id = Id}, {Want, WantPrev}) ->
              ?assertEqual({FeedId, Want, WantPrev, true},
                           {Author, Seq, Prev, Valid}),
              {Want + 1, Id}
      end, {1, null}, Msgs),
    length(Msgs).

%% A fresh identity as an ssb-keys secret file, the way a client that
%% manages identities writes one.
new_secret() ->
    #{public := Pub, secret := Priv} = enacl:sign_keypair(),
    PubB64 = base64:encode(Pub),
    Id = <<"@", PubB64/binary, ".ed25519">>,
    Json = iolist_to_binary(
             json:encode(#{~"curve"   => ~"ed25519",
                           ~"public"  => <<PubB64/binary, ".ed25519">>,
                           ~"private" => <<(base64:encode(Priv))/binary, ".ed25519">>,
                           ~"id"      => Id})),
    {Id, <<"# WARNING: Never show this to anyone.\n", Json/binary, "\n">>}.
