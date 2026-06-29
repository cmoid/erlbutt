%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2023 Charles Moid
-module(utils).
-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type input() :: tuple() | binary().

-export([create_key_pair/0,
         base_64/1,
         display_pub/1,
         decode_id/1,
         encode_rec/1,
         concat/1,
         incr/1,
         combine/2,
         send_data/4,
         load_term/1,
         fold_log_file/3,
         feed_dir/1,
         find_or_create_feed_pid/1,
         check_id/1,
         update_refs/1,
         log/1,
         ping_req/1,
         whoami_req/1,
         error_msg/2,
         nat_decode/1,
         size/1]).

-doc "Calculate the size of an input, suggested by elp".
-spec size(input()) -> non_neg_integer().
size(Input) when is_tuple(Input) ->
    erlang:tuple_size(Input);
size(Input) when is_binary(Input) ->
    erlang:byte_size(Input).

nat_decode(Msg) ->
    {Json, _, _} = json:decode(Msg,[], #{object_finish =>
                              fun(Acc,OldAcc) ->
                                      {{lists:reverse(Acc)}, OldAcc} end}),
    Json.


create_key_pair() ->
    {Pub, Priv} = shs:create_long_pair(),
    {?l2b(base_64(Pub)), ?l2b(base_64(Priv))}.

base_64(Binary) ->
    binary_to_list(base64:encode(Binary)).

display_pub(PubKey) ->
    ?l2b("@" ++ ?b2l(PubKey) ++ ".ed25519").

decode_id(<<"@",RemId/binary>>) ->
    RawId = hd(string:replace(RemId,".ed25519","")),
    decode_id1(RawId);

decode_id(<<"%",RemId/binary>>) ->
    RawId = hd(string:replace(RemId,".sha256","")),
    decode_id1(RawId);

decode_id(<<"&",RemId/binary>>) ->
    RawId = hd(string:replace(RemId,".sha256","")),
    decode_id1(RawId).

decode_id1(RawId) ->
    integer_to_binary(binary:decode_unsigned(base64:decode(RawId)),16).

concat(ListOfBins) ->
    iolist_to_binary(ListOfBins).

%% Increment a box-stream nonce, preserving its width. binary:encode_unsigned/1
%% strips leading zero bytes, so a nonce whose high-order byte became 0x00 would
%% shrink below 24 bytes and make the next secretbox call crash with badarg.
%% Keep the original byte width and wrap at 2^(Size*8), matching SSB's
%% big-endian nonce semantics (all-0xFF wraps back to 0).
incr(Nonce) ->
    Size = byte_size(Nonce),
    <<(binary:decode_unsigned(Nonce) + 1):Size/big-integer-unit:8>>.

combine(nil, Bin) ->
    Bin;

combine(Bin1, Bin2) ->
    Bin1Len = utils:size(Bin1),
    <<Bin1:Bin1Len/binary, Bin2/binary>>.

%% Tunnel transport: inner-box the data, then hand the ciphertext to the
%% room-facing peer as one muxrpc frame (it applies the outer box-stream).
%% The room only ever relays this opaque inner ciphertext.
send_data(Data, {tunnel, OwnerPid, ReqNo}, Nonce, SecretBoxKey) ->
    {EncBox, NewNonce} =
        boxstream:box(Data, Nonce, SecretBoxKey),

    ssb_peer:send_frame(OwnerPid, ReqNo, EncBox),

    NewNonce;

send_data(Data, Socket, Nonce, SecretBoxKey) ->

    {EncBox, NewNonce} =
        boxstream:box(Data, Nonce, SecretBoxKey),

    gen_tcp:send(Socket, EncBox),

    NewNonce.

%% Specific to the SSB feed format, each message is prefixed with a 4 byte length
%% field and followed by the same length field. This enables on to check that a message
%% was correctly read.
load_term(IoDev) ->
    case file:read(IoDev, 4) of
        {ok, <<TermLenInt:32/integer>>} ->
            case file:read(IoDev, TermLenInt) of
                {ok, TermData} when byte_size(TermData) == TermLenInt ->
                    check_data(IoDev, TermData, TermLenInt);
                %% Short read: a torn final record (we no longer fsync each
                %% write, so a record can be partially flushed). Stop cleanly.
                {ok, _Partial} ->
                    {error, truncated};
                eof ->
                    {error, truncated};
                {error, Reason} ->
                    {error, Reason}
            end;
        %% Fewer than 4 bytes left: a torn length prefix — treat as end.
        {ok, _Partial} ->
            {error, truncated};
        eof ->
            {error, eof};
        {error, Reason} ->
            {error, Reason}
    end.

check_data(IoDev, Data, Len) ->
    %% The term length is repeated after the term as an integrity check.
    %% file:read returns the bare atom `eof` (not {error,eof}) at end of file;
    %% a torn record with no trailing length must end the fold, not crash.
    case file:read(IoDev, 4) of
        {ok, <<Len:32/integer>>} ->
            {ok, Data};
        {ok, <<_:32/integer>>} ->
            {error, data_size_no_match};
        {ok, _Partial} ->
            {error, truncated};
        eof ->
            {error, truncated};
        {error, Reason} ->
            {error, Reason}
    end.

%% Fold over every message frame in an offset log file (the global log.offset
%% or any per-feed log.offset).  Fun(MsgBinary, Acc) -> NewAcc.
%% Returns Acc unchanged if the file does not exist.
fold_log_file(Fun, Acc, FilePath) ->
    case file:open(FilePath, [read, binary]) of
        {ok, IoDev} ->
            fold_log_loop(Fun, Acc, IoDev);
        {error, enoent} ->
            Acc;
        {error, _} ->
            Acc
    end.

fold_log_loop(Fun, Acc, IoDev) ->
    case load_term(IoDev) of
        {ok, Data} ->
            file:read(IoDev, 4),          %% skip NextOffset field
            fold_log_loop(Fun, Fun(Data, Acc), IoDev);
        {error, _} ->
            file:close(IoDev),
            Acc
    end.

%% On-disk directory holding a feed's log.offset, profile, contacts and
%% references files.  Takes the display id (@...=.ed25519); raises on a
%% malformed id (see decode_id/1).
feed_dir(FeedId) ->
    DecodeId = decode_id(FeedId),
    Location = config:feed_loc(),
    <<Dir:2/binary, RestAuth/binary>> = DecodeId,
    <<Location/binary, Dir/binary, "/", RestAuth/binary>>.

find_or_create_feed_pid(Id) ->
    case check_id(Id) of
        bad -> bad;
        ok  -> ssb_feed_sup:find_or_start(Id)
    end.

update_refs(#message{id = Id, author = AuthId} = Msg) ->
    Branches = social_msg:is_branch(Msg),
    case Branches of
        false ->
            none;
        {Root, BranchList} ->
            Target = {~"tar", [Id, AuthId]},
            lists:map(fun(Bi) ->
                              %% this is really ugly, branch most often is either a binary
                              %% or a list of such, but occasionally it's an
                              %% object like {<<"0">>:<<"%msgid....">>}
                              %% this is the price one pays for immutable feeds, garbage
                              %% lasts forever
                              Ai = if is_binary(Bi) ->
                                           mess_auth:get(Bi);
                                      true ->
                                           not_found
                                   end,
                              case Ai of
                                  not_found ->
                                      nop;
                                  _Else ->
                                      Record = {[{~"root", Root},
                                                 {~"src", [Bi, Ai]},
                                                 Target]},
                                      Pid = find_or_create_feed_pid(Ai),
                                      ssb_feed:store_ref(Pid, encode_rec(Record))
                              end
                      end, BranchList)
    end.

encode_rec(Record) ->
    iolist_to_binary(message:ssb_encoder(Record, fun message:ssb_encoder/3, [])).

check_id(<<"@",Id/binary>>) ->
        try
            case binary:matches(Id,[~".ed25519"]) of
                [] ->
                    ?LOG_DEBUG("Bad author ~p ~n", [Id]),
                    bad;
                _Else ->
                    RawId = hd(string:replace(Id,".ed25519","")),
                    _DecodedId = integer_to_binary(binary:decode_unsigned(base64:decode(?b2l(RawId))),16),
                    ok
            end
        catch
            error:_Reason ->
                bad
        end;
check_id(_Else) ->
    bad.

ping_req(ReqNo) ->
    Flags = rpc_processor:create_flags(1,0,2),
    Body = encode_rec({[{~"name",[~"gossip",~"ping"]},
                          {~"args",[{[{~"timeout", 300000}]}]},
                          {~"type",~"duplex"}]}),
    Header = rpc_processor:create_header(Flags, utils:size(Body), ReqNo),
    utils:combine(Header, Body).

whoami_req(ReqNo) ->
    Flags = rpc_processor:create_flags(1,0,2),
    Body = encode_rec({[{~"name",[?whoami]},
                          {~"args",[]},
                          {~"type",~"sync"}]}),
    Header = rpc_processor:create_header(Flags, utils:size(Body), ReqNo),
    utils:combine(Header, Body).

error_msg(Name, Mess) ->
    encode_rec({[{~"name", Name},
                 {~"message", Mess},
                 {~"stack", ~"_"}
                 ]}).

log({Socket, Data}) ->
    ?LOG_DEBUG("received a tcp packet of size: ~p ~n on socket ~p ~n",
           [utils:size(Data), Socket]);

log(Info) ->
    ?LOG_DEBUG("received random info message ~p ~n",
           [Info]).

-ifdef(TEST).

combine_test() ->
    ~"foo" = combine(nil, ~"foo"),
    ~"foobar" = combine(~"foo",~"bar").

%% fold_log_file must return the complete records and stop cleanly when the
%% file ends in a torn record (possible now that writes are not fsync'd),
%% rather than crashing on the bare `eof` from file:read.
fold_log_truncated_test() ->
    File = "test/fold_torn.offset",
    Rec  = fun(M) -> L = byte_size(M), <<L:32, M/binary, L:32, 0:32>> end,
    Torn = fun(M) -> L = byte_size(M), <<L:32, M/binary>> end,  %% no trailing len
    ok = file:write_file(File, <<(Rec(~"one"))/binary,
                                 (Rec(~"two"))/binary,
                                 (Torn(~"three"))/binary>>),
    Got = fold_log_file(fun(D, Acc) -> [D | Acc] end, [], File),
    ?assertEqual([~"one", ~"two"], lists:reverse(Got)),
    file:delete(File).

create_pid_test() ->
    config:start_link("test/ssb.cfg"),
    keys:start_link(),
    ssb_feed_sup:start_link(),
    {Pub, _Priv} = create_key_pair(),
    FeedId1 = display_pub(Pub),
    Pid1 = find_or_create_feed_pid(FeedId1),
    ?assert(is_pid(Pid1)),
    {Pub2, _Priv2} = create_key_pair(),
    FeedId2 = display_pub(Pub2),
    Pid2 = find_or_create_feed_pid(FeedId2),
    ?assert(is_pid(Pid2)),
    ?assert(Pid1 =/= Pid2).

create_pid_kill_test() ->
    config:start_link("test/ssb.cfg"),
    keys:start_link(),
    ssb_feed_sup:start_link(),
    {Pub, _Priv} = create_key_pair(),
    FeedId1 = display_pub(Pub),
    Pid1 = find_or_create_feed_pid(FeedId1),
    exit(Pid1, kill),
    timer:sleep(50),
    Pid2 = find_or_create_feed_pid(FeedId1),
    %% After a crash the supervisor restarts the feed; we get a fresh pid.
    ?assert(is_pid(Pid2)),
    ?assert(Pid1 =/= Pid2).

simple_blob_decode_test() ->
    Coded = <<"&ybENuaMAdmfjmwR852FNDsj3biaMl5P4HF/jJj7OtQQ=.sha256">>,
    {ok, Cwd} = file:get_cwd(),
    F = Cwd ++ "/testdata/" ++ "b10db9a3007667e39b047ce7614d0ec8f76e268c9793f81c5fe3263eceb504",
    {ok, Blob} = file:read_file(F),
    CompCoded = list_to_binary("&" ++
                         utils:base_64(crypto:hash(sha256, Blob))
                     ++
                         ".sha256"),
    ?assert(CompCoded == Coded).

%% incr/1 preserves the nonce width even when the result has leading zero
%% bytes — regression test for the 23-byte-nonce secretbox badarg crash.
incr_preserves_width_test() ->
    %% leading high-order byte is zero: must stay 24 bytes, not shrink to 23
    LeadingZero = <<0, 1, (binary:copy(<<0>>, 22))/binary>>,
    24 = byte_size(LeadingZero),
    Bumped = utils:incr(LeadingZero),
    ?assertEqual(24, byte_size(Bumped)),
    ?assertEqual(<<0, 1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>, Bumped).

%% a plain low value increments correctly and keeps its width.
incr_basic_test() ->
    Nonce = <<0:184, 41>>,
    24 = byte_size(Nonce),
    ?assertEqual(<<0:184, 42>>, utils:incr(Nonce)).

%% all-0xFF wraps back to zero rather than growing to 25 bytes.
incr_wraps_at_max_test() ->
    MaxNonce = binary:copy(<<255>>, 24),
    Bumped = utils:incr(MaxNonce),
    ?assertEqual(24, byte_size(Bumped)),
    ?assertEqual(<<0:192>>, Bumped).

%% incrementing is width-agnostic (works for any size binary).
incr_arbitrary_width_test() ->
    ?assertEqual(<<1>>, utils:incr(<<0>>)),
    ?assertEqual(<<0>>, utils:incr(<<255>>)),
    ?assertEqual(<<1, 0>>, utils:incr(<<0, 255>>)).

-endif.
