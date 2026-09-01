%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% patchwork.thread.sorted: the replies of one thread, for the thread
%% page.  It streams every reply (a post/about whose content.root is the
%% thread root) in causal order, then a {sync: true} sentinel, then —
%% in live mode — replies as they arrive.
%%
%% The root itself is NOT streamed: the renderer fetches the root
%% separately (get) and prepends it, then appends this stream's replies.
%% Emitting the root here too would put it in the thread twice and make
%% ssb-sort throw "thread has duplicate message" when composing a reply.
%%
%% No view of its own: the reply set is the backlinks to the root,
%% filtered to thread replies, so it reads the backlinks view and
%% subscribes to its change events for the live tail.
%%
%% Replies come out in causal order — every reply after the messages it
%% references — because the client appends them as they arrive and never
%% re-sorts, so this order is what the reader sees.  See "Causal
%% ordering" below.  Block filtering is still not applied.
%% Registered by silkpurse_app (stateless).
-module(silkpurse_thread).

-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

-export([manifest/0, handle_rpc/3]).

-define(DEFAULT_TYPES, [~"post", ~"about"]).

manifest() ->
    [{[~"patchwork", ~"thread", ~"sorted"], source, owner}].

handle_rpc([~"patchwork", ~"thread", ~"sorted"], [{Opts}], _Caller) ->
    case ?pgv(~"dest", Opts) of
        Dest when is_binary(Dest) ->
            Types = case ?pgv(~"types", Opts) of
                        Ts when is_list(Ts) -> Ts;
                        _                    -> ?DEFAULT_TYPES
                    end,
            Live = ?pgv(~"live", Opts) =:= true,
            Snapshot = snapshot(Dest, Types),
            case Live of
                false ->
                    {source, [{json, Bin} || {_Id, Bin} <- Snapshot]};
                true ->
                    EventFun = fun(Event) -> live_reply(Event, Dest, Types) end,
                    {live_source, Snapshot, silkpurse_backlinks, EventFun}
            end;
        _ ->
            {error, ~"thread.sorted needs a dest"}
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

%% [{Id, EncodedEnvelope}] for the thread's replies in causal order,
%% followed by a {sync: true} sentinel (the renderer waits for it before
%% showing the thread).  The root is deliberately excluded — see the
%% module doc.
snapshot(Dest, Types) ->
    Replies = lists:filtermap(
                fun(Id) ->
                        case reply_msg(Id, Dest, Types) of
                            {Meta, Bin} -> {true, {Id, Meta, Bin}};
                            undefined   -> false
                        end
                end, silkpurse_backlinks:refs(Dest)),
    [{Id, Bin} || {Id, _Meta, Bin} <- causal_sort(Replies)]
        ++ [{make_ref(), encode_json({[{~"sync", true}]})}].

%%%===================================================================
%%% Causal ordering
%%%
%%% The client appends these in the order they arrive and does not
%%% re-sort, so this order IS the display order.  Sorting on the asserted
%%% timestamp alone — what this did before — dropped the branch structure
%%% on the floor: a reply that forked off mid-thread landed wherever its
%%% author's clock said, which reads as replying to the wrong message.
%%% Asserted timestamps are author-controlled and skew across peers, so
%%% that ordering is not even reliably chronological.
%%%
%%% This is a real topological pass rather than a port of ssb-sort's
%%% pairwise comparator.  Ancestry is a PARTIAL order, and running
%%% Array.prototype.sort over a partial comparator can emit a child
%%% before its parent; Kahn cannot.  Ties — messages with no causal
%%% relation to each other — fall back to ssb-sort's own chain so they
%%% land where the client would have put them.
%%%===================================================================

%% Replies ordered so that each one follows every message in this thread
%% that it references.
causal_sort(Replies) ->
    Index = maps:from_list([{Id, Meta} || {Id, Meta, _Bin} <- Replies]),
    Deps  = maps:from_list(
              [{Id, [L || L <- maps:get(links, Meta),
                          L =/= Id, maps:is_key(L, Index)]}
               || {Id, Meta, _Bin} <- Replies]),
    Order = kahn(Deps, Index, []),
    Rows  = maps:from_list([{Id, {Id, Meta, Bin}} || {Id, Meta, Bin} <- Replies]),
    [maps:get(Id, Rows) || Id <- Order].

kahn(Deps, _Index, Acc) when map_size(Deps) =:= 0 ->
    lists:reverse(Acc);
kahn(Deps, Index, Acc) ->
    case [Id || {Id, []} <- maps:to_list(Deps)] of
        [] ->
            %% Unreachable with honest hashes — you cannot reference a
            %% message that does not exist yet — but a forged or corrupt
            %% set must not hang the thread page.  Emit the remainder in
            %% tiebreak order rather than looping.
            lists:reverse(Acc) ++ sort_ids(maps:keys(Deps), Index);
        Ready ->
            [Next | _] = sort_ids(Ready, Index),
            Deps1 = maps:map(fun(_K, D) -> lists:delete(Next, D) end,
                             maps:remove(Next, Deps)),
            kahn(Deps1, Index, [Next | Acc])
    end.

sort_ids(Ids, Index) ->
    lists:sort(fun(A, B) -> before(A, B, Index) end, Ids).

%% ssb-sort's tiebreak chain: received timestamp, then the asserted one,
%% then the key.  The key comparison is DESCENDING — that is ssb-sort's
%% (`a.key > b.key ? -1 : ...`), not a slip — so a tie resolves the same
%% way it would client-side.
before(A, B, Index) ->
    #{received := RA, asserted := SA} = maps:get(A, Index),
    #{received := RB, asserted := SB} = maps:get(B, Index),
    if RA =/= RB -> RA < RB;
       SA =/= SB -> SA < SB;
       true      -> A > B
    end.

%% A live backlinks event that names a new reply to this thread.
live_reply({link, Target, MsgId}, Dest, Types) when Target =:= Dest ->
    case reply_msg(MsgId, Dest, Types) of
        {_Meta, Bin} -> {send, MsgId, Bin};
        undefined    -> skip
    end;
live_reply(_Event, _Dest, _Types) ->
    skip.

%% {Timestamp, EncodedEnvelope} when MsgId is a reply to Dest of an allowed
%% type, else undefined.
%%
%% A private reply is boxed, so its root and type can only be read by
%% decrypting it — and it is then served DECRYPTED (as privateFeed does,
%% and as get({private: true}) does), since the thread page renders the
%% content directly.  Decryption happens per query; nothing is stored.
reply_msg(MsgId, Dest, Types) ->
    case get_msg(MsgId) of
        #message{content = {Props} = ContentObj} = M ->
            case is_reply(Props, Dest, Types) of
                true  -> {meta(M, ContentObj), message:encode(M)};
                false -> undefined
            end;
        #message{content = Box} = M when is_binary(Box) ->
            case decrypt(Box) of
                {ok, {Props} = ContentObj} ->
                    case is_reply(Props, Dest, Types) of
                        true  -> {meta(M, ContentObj),
                                  message:encode_decrypted(M, ContentObj)};
                        false -> undefined
                    end;
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

%% What the ordering needs from a reply: ssb-sort's two timestamps, and
%% the ids it references (its outgoing edges in the thread DAG).
%%
%% received is the envelope timestamp the client sees (message:encode/1
%% writes #message.received there) and asserted is value.timestamp, which
%% is why they are compared in that order.
%%
%% Links come from ssb_links:links_of/1 — every reference anywhere in the
%% content, not just `branch`.  That matches ssb-sort, which walks the
%% whole value for message refs; a reply quoting an earlier message in the
%% thread is causally after it whether or not it branched from it.  The
%% already-decrypted content object is passed in so a private reply is not
%% decrypted twice.
meta(#message{timestamp = Asserted, received = Received}, ContentObj) ->
    #{received => num(Received),
      asserted => num(Asserted),
      links    => [T || {T, _Field, msg} <- ssb_links:links_of(ContentObj)]}.

%% Timestamps in this store are NOT reliably integers.  Real values seen
%% on live data: integers, floats (1787580763554.0059), binaries
%% (<<"1787796169351">>) and absent — the store predates a conversion and
%% nothing has ever normalised it.
%%
%% Coercing matters because these are sort keys: mapping every odd shape
%% to 0 (which the first cut did) does not merely lose precision, it
%% sorts those messages to the FRONT of every tie, which is the ordering
%% bug this function exists to avoid.  0 stays only for genuinely absent.
num(N) when is_integer(N) -> N;
num(N) when is_float(N)   -> trunc(N);
num(B) when is_binary(B)  ->
    try binary_to_integer(B)
    catch _:_ ->
            try trunc(binary_to_float(B))
            catch _:_ -> 0
            end
    end;
num(_) -> 0.

is_reply(Props, Dest, Types) ->
    lists:member(?pgv(~"type", Props), Types)
        andalso ?pgv(~"root", Props) =:= Dest.

%% The plaintext content object of a box addressed to us, if we can read it.
decrypt(Box) ->
    case private_box:is_private(Box) andalso private_box:decrypt(Box) of
        {ok, Plain} ->
            try utils:nat_decode(Plain) of
                {_} = ContentObj -> {ok, ContentObj};
                _                -> undefined     %% not a content object
            catch _:_ -> undefined                %% body need not be JSON
            end;
        _ ->
            undefined
    end.

get_msg(MsgId) ->
    case mess_auth:get(MsgId) of
        not_found -> undefined;
        Author ->
            try
                Pid = utils:find_or_create_feed_pid(Author),
                case ssb_feed:fetch_msg(Pid, MsgId) of
                    not_found -> undefined;
                    Msg       -> Msg
                end
            catch _:_ -> undefined
            end
    end.

encode_json(Term) ->
    iolist_to_binary(message:ssb_encoder(Term, fun message:ssb_encoder/3, [pretty])).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% One {Id, Meta, Bin} row as causal_sort/1 consumes them.  The body
%% doubles as the id so the assertions read as orderings.
r(Id, Received, Asserted, Links) ->
    {Id, #{received => Received, asserted => Asserted, links => Links}, Id}.

ids(Sorted) -> [Id || {Id, _Meta, _Bin} <- Sorted].

%% The reason this ordering exists.  Asserted timestamps are written by
%% the author and skew across peers, so a reply whose clock is wrong used
%% to land wherever that clock said — reading as a reply to the wrong
%% message.  The link says otherwise and the link wins.
causal_order_beats_clock_skew_test() ->
    A = ~"%A.sha256", B = ~"%B.sha256", C = ~"%C.sha256",
    %% C replies to B replies to A; every timestamp claims the reverse
    Replies = [r(C, 1, 1, [B]), r(B, 2, 2, [A]), r(A, 3, 3, [])],
    ?assertEqual([A, B, C], ids(causal_sort(Replies))).

%% Two replies branching off the same parent have no causal relation to
%% each other, so the tiebreak decides — received first, which is the
%% arrival time we actually observed, ahead of the author's claim.
fork_siblings_order_by_received_test() ->
    A = ~"%A.sha256", B = ~"%B.sha256", C = ~"%C.sha256",
    Replies = [r(C, 20, 1, [A]), r(B, 10, 999, [A]), r(A, 1, 1, [])],
    ?assertEqual([A, B, C], ids(causal_sort(Replies))).

%% ssb-sort breaks a total tie on the key DESCENDING (`a.key > b.key ?
%% -1`).  Matched so a tie lands where the client would have put it.
key_tiebreak_is_descending_test() ->
    A = ~"%AAA.sha256", Z = ~"%ZZZ.sha256",
    ?assertEqual([Z, A], ids(causal_sort([r(A, 5, 5, []), r(Z, 5, 5, [])]))).

%% Every reply references its root, and the root is deliberately NOT in
%% this set (the client supplies it).  If out-of-thread refs counted as
%% dependencies nothing would ever be ready and the thread would come
%% back empty.
links_outside_the_thread_are_ignored_test() ->
    Root = ~"%ROOT.sha256", A = ~"%A.sha256",
    Replies = [r(A, 1, 1, [Root, ~"%elsewhere.sha256"])],
    ?assertEqual([A], ids(causal_sort(Replies))).

%% Unreachable with honest hashes — you cannot reference a message that
%% does not exist yet — but a forged or corrupt set must not spin the
%% thread page forever.  Everything still comes out, exactly once.
cycle_terminates_without_losing_messages_test() ->
    A = ~"%A.sha256", B = ~"%B.sha256",
    Out = ids(causal_sort([r(A, 1, 1, [B]), r(B, 2, 2, [A])])),
    ?assertEqual([A, B], lists:sort(Out)),
    ?assertEqual(2, length(Out)).

%% Live data carries timestamps as integers, floats, binaries and not at
%% all.  Mapping the odd shapes to 0 sorted those messages to the front
%% of every tie rather than into their real position.
timestamp_shapes_are_coerced_test() ->
    ?assertEqual(1787796169351, num(1787796169351)),
    ?assertEqual(1787580763554, num(1787580763554.0059)),
    ?assertEqual(1787796169351, num(~"1787796169351")),
    ?assertEqual(1787580763554, num(~"1787580763554.0059")),
    %% genuinely absent still sorts first, which is all 0 should mean
    ?assertEqual(0, num(undefined)),
    ?assertEqual(0, num(null)),
    ?assertEqual(0, num(~"not a number")).

%% A message whose received timestamp is a binary must order by its real
%% value, not land at the front because the shape was unexpected.
binary_timestamp_orders_correctly_test() ->
    A = ~"%A.sha256", B = ~"%B.sha256",
    %% A is older but its timestamp arrived as a binary
    Replies = [r(B, num(2000), 2000, []), r(A, num(~"1000"), 1000, [])],
    ?assertEqual([A, B], ids(causal_sort(Replies))).

%% A reply that quotes itself must not become its own dependency.
self_reference_is_not_a_dependency_test() ->
    A = ~"%A.sha256",
    ?assertEqual([A], ids(causal_sort([r(A, 1, 1, [A])]))).

-endif.
