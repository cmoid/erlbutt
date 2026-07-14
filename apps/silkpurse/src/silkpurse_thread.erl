%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% patchwork.thread.sorted: the replies of one thread, for the thread
%% page.  It streams every reply (a post/about whose content.root is the
%% thread root) in timestamp order, then a {sync: true} sentinel, then —
%% in live mode — replies as they arrive.
%%
%% The root itself is NOT streamed: the renderer fetches the root
%% separately (get) and prepends it, then appends this stream's replies.
%% Emitting the root here too would put it in the thread twice and make
%% ssb-sort throw "thread has duplicate message" when composing a reply.
%%
%% No view of its own: the reply set is the backlinks to the root,
%% filtered to thread replies, so it reads the backlinks view and
%% subscribes to its change events for the live tail.  Replies are
%% ordered by asserted timestamp (an approximation of ssb-sort's causal
%% order; block filtering and branch/fork handling are not yet applied).
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

%% [{Id, EncodedEnvelope}] for the thread's replies in timestamp order,
%% followed by a {sync: true} sentinel (the renderer waits for it before
%% showing the thread).  The root is deliberately excluded — see the
%% module doc.
snapshot(Dest, Types) ->
    Replies = lists:filtermap(
                fun(Id) ->
                        case reply_msg(Id, Dest, Types) of
                            {Ts, Bin} -> {true, {sort_key(Ts), Id, Bin}};
                            undefined -> false
                        end
                end, silkpurse_backlinks:refs(Dest)),
    Sorted = [{Id, Bin} || {_Key, Id, Bin} <- lists:sort(Replies)],
    Sorted ++ [{make_ref(), encode_json({[{~"sync", true}]})}].

sort_key(Ts) when is_integer(Ts) -> Ts;
sort_key(_)                      -> 0.

%% A live backlinks event that names a new reply to this thread.
live_reply({link, Target, MsgId}, Dest, Types) when Target =:= Dest ->
    case reply_msg(MsgId, Dest, Types) of
        {_Ts, Bin} -> {send, MsgId, Bin};
        undefined  -> skip
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
        #message{content = {Props}, timestamp = Ts} = M ->
            case is_reply(Props, Dest, Types) of
                true  -> {Ts, message:encode(M)};
                false -> undefined
            end;
        #message{content = Box, timestamp = Ts} = M when is_binary(Box) ->
            case decrypt(Box) of
                {ok, {Props} = ContentObj} ->
                    case is_reply(Props, Dest, Types) of
                        true  -> {Ts, message:encode_decrypted(M, ContentObj)};
                        false -> undefined
                    end;
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

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
                ssb_feed:fetch_msg(Pid, MsgId)
            catch _:_ -> undefined
            end
    end.

encode_json(Term) ->
    iolist_to_binary(message:ssb_encoder(Term, fun message:ssb_encoder/3, [pretty])).
