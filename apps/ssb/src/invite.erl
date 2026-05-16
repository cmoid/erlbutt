%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% SSB pub invite code parsing and redemption (client side).
%%
%% Invite code format: "host:port:@pubkey.ed25519~seed_base64"
%%
%% Redemption flow:
%%   1. Derive an Ed25519 keypair from the invite seed.
%%   2. Connect to the pub and complete SHS using that keypair as our identity.
%%   3. Send blobs.createWants invite.use with our real feed ID.
%%   4. Parse the pub's follow-message response.
%%   5. Post a contact (follow) message and a pub-address message to our feed.
-module(invite).

-compile({no_auto_import,[size/1]}).
-import(utils, [combine/2, size/1, send_data/4]).

-export([parse/1, create/2, validate_and_consume/1, redeem/1]).

%% Parse an SSB pub invite code.
%% Returns {Host, Port, PubKey, Seed} with PubKey and Seed as raw binaries.
parse(Code) when is_binary(Code) ->
    parse(binary_to_list(Code));
parse(Code) ->
    [AddrPart, SeedB64] = string:split(Code, "~"),
    [Host, PortStr, PubKeyDisp] = string:split(AddrPart, ":", all),
    PubKey = base64:decode(key_only(PubKeyDisp)),
    Seed   = base64:decode(SeedB64),
    {Host, list_to_integer(PortStr), PubKey, Seed}.

%% Create an invite code for this pub at Host:Port.
%% Generates a fresh Ed25519 keypair from a random seed, stores the public key
%% in invite_store, and returns the invite string.
create(Host, Port) when is_list(Host), is_integer(Port) ->
    Seed  = crypto:strong_rand_bytes(32),
    #{public := InvPk} = enacl:sign_seed_keypair(Seed),
    ok = invite_store:store(InvPk),
    %% Third field is OUR long-term pub key — the client needs it to do SHS.
    %% The seed after ~ is the invite keypair seed (the client's identity).
    OurPubB64 = binary_to_list(keys:pub_key()),
    SeedB64   = binary_to_list(base64:encode(Seed)),
    Code = Host ++ ":" ++ integer_to_list(Port) ++
           ":@" ++ OurPubB64 ++ ".ed25519~" ++ SeedB64,
    {ok, list_to_binary(Code)}.

%% Delegate to invite_store — exposed here so callers have a single module.
validate_and_consume(ClientPk) ->
    invite_store:validate_and_consume(ClientPk).

%% Redeem an invite code.
%% Returns {ok, FollowMsg} where FollowMsg is the decoded follow message the
%% pub sent back, or {error, Reason} on failure.
redeem(Code) ->
    {Host, Port, PubKey, Seed} = parse(Code),
    #{public := InvPk, secret := InvSk} = enacl:sign_seed_keypair(Seed),
    Socket = connect(Host, Port),
    {ok, {Socket, DecBoxKey, DecNonce, EncBoxKey, EncNonce}} =
        shs:client_shake_hands(Socket, PubKey, {InvPk, InvSk}),

    %% Send invite.use with our real feed ID
    OurId  = keys:pub_key_disp(),
    ReqBody = utils:encode_rec({[{~"name", [~"invite", ~"use"]},
                                 {~"args", [{[{~"feed", OurId}]}]},
                                 {~"type", ~"async"}]}),
    Flags  = rpc_processor:create_flags(0, 0, 2),
    Header = rpc_processor:create_header(Flags, size(ReqBody), 1),
    _N1 = send_data(combine(Header, ReqBody), Socket, EncNonce, EncBoxKey),

    %% Receive and unbox the pub's response
    {RpcFrame, _N2} = recv_unbox(Socket, DecBoxKey, DecNonce),
    <<_RpcHeader:9/binary, ResponseBody/binary>> = RpcFrame,
    FollowMsg = utils:nat_decode(ResponseBody),

    %% Post follow and pub messages to our own feed.
    %% display_pub expects a base64 binary (no .ed25519 suffix), not raw bytes.
    PubId = utils:display_pub(base64:encode(PubKey)),
    post_follow(PubId),
    post_pub(list_to_binary(Host), Port, PubId),

    gen_tcp:close(Socket),
    {ok, FollowMsg}.

%%%===================================================================
%%% Internal
%%%===================================================================

post_follow(PubId) ->
    OurId   = keys:pub_key_disp(),
    FeedPid = utils:find_or_create_feed_pid(OurId),
    Content = {[{~"type",      ~"contact"},
                {~"contact",   PubId},
                {~"following", true}]},
    ssb_feed:post_content(FeedPid, Content).

post_pub(Host, Port, PubId) ->
    OurId   = keys:pub_key_disp(),
    FeedPid = utils:find_or_create_feed_pid(OurId),
    Content = {[{~"type", ~"pub"},
                {~"address", {[{~"host", Host},
                               {~"port", Port},
                               {~"key",  PubId}]}}]},
    ssb_feed:post_content(FeedPid, Content).

%% Receive boxstream frames until we have a complete one.
recv_unbox(Socket, Key, Nonce) ->
    recv_unbox(Socket, Key, Nonce, <<>>).
recv_unbox(Socket, Key, Nonce, Buf) ->
    {ok, Data} = gen_tcp:recv(Socket, 0, 10000),
    NewBuf = combine(Buf, Data),
    case boxstream:unbox(Key, Nonce, NewBuf) of
        {partial, _, _, _} ->
            recv_unbox(Socket, Key, Nonce, NewBuf);
        {complete, Body, NewNonce, _Rem} ->
            {Body, NewNonce}
    end.

connect(Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port,
                                   [binary, {packet, 0}, {active, false}]),
    Socket.

%% Strip "@" prefix and ".ed25519" suffix from a pub key display string.
key_only(Str) ->
    Trimmed = string:trim(Str),
    NoAt = case Trimmed of
        [$@ | Rest] -> Rest;
        Other       -> Other
    end,
    hd(string:split(NoAt, ".ed25519")).
