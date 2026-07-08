%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% The patchwork.contacts surface the UI binds for follow/block state
%% (sidebar following list, profile followers, block filtering).  Backed
%% by the friends view — no view of its own — so it is a stateless
%% plugin registered by silkpurse_app.
%%
%% contacts.stateStream({feedId, live, reverse}) is a live dict stream:
%% it emits the full {contactId => state} map first (state true = follow,
%% false = block, matching getContactState), then {contact => state}
%% deltas as contact messages arrive.  Forward = the edges feedId
%% asserts; reverse = the edges asserted about feedId (its followers /
%% blockers).  Deltas come from the friends view's change events.
-module(silkpurse_contacts).

-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

-export([manifest/0, handle_rpc/3]).

manifest() ->
    [{[~"patchwork", ~"contacts", ~"stateStream"],  source, owner},
     {[~"patchwork", ~"contacts", ~"isFollowing"],  async,  owner},
     {[~"patchwork", ~"contacts", ~"isBlocking"],   async,  owner},
     {[~"patchwork", ~"contacts", ~"ignoreStream"], source, owner}].

handle_rpc([~"patchwork", ~"contacts", ~"stateStream"], [{Opts}], _Caller) ->
    case ?pgv(~"feedId", Opts) of
        FeedId when is_binary(FeedId) ->
            Reverse = ?pgv(~"reverse", Opts) =:= true,
            Live    = ?pgv(~"live", Opts) =:= true,
            Dict = case Reverse of
                       false -> friends:edges(FeedId);
                       true  -> friends:reverse_edges(FeedId)
                   end,
            Snapshot = encode_json(dict_ejson(Dict)),
            case Live of
                false ->
                    {source, [{json, Snapshot}]};
                true ->
                    EventFun = fun(Event) -> delta(Event, FeedId, Reverse) end,
                    {live_source, [{make_ref(), Snapshot}], friends, EventFun}
            end;
        _ ->
            {error, ~"contacts.stateStream needs a feedId"}
    end;

handle_rpc([~"patchwork", ~"contacts", ~"isFollowing"], [{Opts}], _Caller) ->
    {reply, friends:edge(?pgv(~"source", Opts), ?pgv(~"dest", Opts)) =:= true};

handle_rpc([~"patchwork", ~"contacts", ~"isBlocking"], [{Opts}], _Caller) ->
    {reply, friends:edge(?pgv(~"source", Opts), ?pgv(~"dest", Opts)) =:= false};

%% Ignore list (private feeds hidden by the user): not tracked yet, so
%% a single empty dict that stays open — settles the client's observable.
handle_rpc([~"patchwork", ~"contacts", ~"ignoreStream"], _Args, _Caller) ->
    {live_source, [{make_ref(), <<"{}">>}], friends, fun(_) -> skip end}.

%%%===================================================================
%%% Internal
%%%===================================================================

%% A live-mode delta for one friends change event.  Forward: the event's
%% author must be feedId, and we report its edge toward the contact.
%% Reverse: the event's contact must be feedId, and we report the
%% author's edge toward feedId.  edge/2 returns true|false|null, which
%% is exactly the contact state the client expects.
delta({_Tag, Author, Contact, _Bool}, FeedId, false) when Author =:= FeedId ->
    {send, encode_json({[{Contact, friends:edge(FeedId, Contact)}]})};
delta({_Tag, Author, Contact, _Bool}, FeedId, true) when Contact =:= FeedId ->
    {send, encode_json({[{Author, friends:edge(Author, FeedId)}]})};
delta(_Event, _FeedId, _Reverse) ->
    skip.

dict_ejson(Map) ->
    {[{K, V} || K := V <- Map]}.

encode_json(Term) ->
    iolist_to_binary(message:ssb_encoder(Term, fun message:ssb_encoder/3, [pretty])).
