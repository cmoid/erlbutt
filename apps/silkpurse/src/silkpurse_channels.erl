%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Channel index: which channels exist, how active each is, and which
%% ones the node owner subscribes to.  A channel is named by a message's
%% content.channel; a subscription is a {type: channel, channel, subscribed}
%% message (a boolean toggle).  The view keeps, per normalized channel
%% name, a post count and last-activity timestamp, plus the owner's latest
%% subscription state.
%%
%% An ssb_view over a named public ETS set (tagged keys {stat,Ch} and
%% {sub,Ch}) plus an ssb_plugin serving the discovery surface:
%%   channels.suggest({text, limit})  async  -> [{id, count, subscribed}]
%%   channels.recentStream({limit})    source -> live [channelName] by recency
%%
%% channel.obs.subscribed (a feed's own subscriptions) is computed client
%% side from its feed via createUserStream, and subscribing is a publish,
%% so neither needs a method here.
-module(silkpurse_channels).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).
-behaviour(ssb_view).
-behaviour(ssb_plugin).

-include_lib("ssb/include/ssb.hrl").

-export([start_link/0]).
-export([view_version/0, view_load/0, view_reset/0, view_save/0, view_entry/1]).
-export([manifest/0, handle_rpc/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         handle_info/2, terminate/2, code_change/3]).

-define(TAB, silkpurse_channels).
-define(MARKER, '$complete').

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% ssb_view callbacks (run in the view_manager process)
%%%===================================================================

%% 2: subscriptions are tracked per subscriber feed (key {sub,Ch,Feed}),
%% not just the owner, so channels.subscriptions can list subscribers.
view_version() -> 2.

view_load() ->
    Loaded = try ets:lookup(?TAB, ?MARKER) =/= []
             catch error:badarg -> false
             end,
    case Loaded of true -> ok; false -> empty end.

view_reset() ->
    ets:delete_all_objects(?TAB),
    ok.

view_save() ->
    ets:insert(?TAB, {?MARKER}),
    File = table_file(),
    filelib:ensure_dir(File),
    ok = ets:tab2file(?TAB, ?b2l(File)),
    ok.

view_entry(#message{author = Author, timestamp = Ts, content = {Props}}) ->
    case normalize_channel(?pgv(~"channel", Props)) of
        Ch when is_binary(Ch), byte_size(Ch) > 0 ->
            case ?pgv(~"type", Props) of
                ~"channel" ->
                    %% a subscription toggle by any feed
                    case ?pgv(~"subscribed", Props) of
                        Sub when is_boolean(Sub) ->
                            set_sub(Ch, Author, Sub, Ts),
                            {events, [{csub, Ch, Author, Sub}]};
                        _ -> ok
                    end;
                _ ->
                    %% any other message tagged with a channel is activity
                    bump(Ch, Ts),
                    {events, [{channel, Ch}]}
            end;
        _ ->
            ok
    end;
view_entry(_) ->
    ok.

%%%===================================================================
%%% ssb_plugin callbacks
%%%===================================================================

manifest() ->
    [{[~"patchwork", ~"channels", ~"suggest"],      async,  owner},
     {[~"patchwork", ~"channels", ~"recentStream"], source, owner},
     {[~"patchwork", ~"subscriptions"],             source, owner}].

handle_rpc([~"patchwork", ~"channels", ~"suggest"], [{Opts}], _Caller) ->
    Text  = case ?pgv(~"text", Opts) of T when is_binary(T) -> T; _ -> ~"" end,
    Limit = case ?pgv(~"limit", Opts) of L when is_integer(L) -> L; _ -> 20 end,
    {reply, suggest(Text, Limit)};
handle_rpc([~"patchwork", ~"channels", ~"suggest"], _Args, _Caller) ->
    {reply, []};

handle_rpc([~"patchwork", ~"channels", ~"recentStream"], Args, _Caller) ->
    Limit = case Args of
                [{Opts}] ->
                    case ?pgv(~"limit", Opts) of
                        L when is_integer(L) -> L;
                        _                    -> 10
                    end;
                _ -> 10
            end,
    %% a value stream: the current recent list now, re-sent (whole) on
    %% any channel change so the sidebar stays current
    Initial  = encode_json(recent(Limit)),
    EventFun = fun({channel, _Ch}) -> {send, encode_json(recent(Limit))};
                  (_)              -> skip
               end,
    {live_source, [{make_ref(), Initial}], ?MODULE, EventFun};

%% subscriptions({channel}): who subscribes to the channel, as
%% {from, value} toggles — current subscribers first, then live changes.
handle_rpc([~"patchwork", ~"subscriptions"], [{Opts}], _Caller) ->
    case normalize_channel(?pgv(~"channel", Opts)) of
        Ch when is_binary(Ch), byte_size(Ch) > 0 ->
            Snapshot = [{make_ref(), toggle(F, true)} || F <- subscribers(Ch)],
            EventFun = fun({csub, C, F, B}) when C =:= Ch -> {send, toggle(F, B)};
                          (_)                             -> skip
                       end,
            {live_source, Snapshot, ?MODULE, EventFun};
        _ ->
            {source, []}
    end;
handle_rpc([~"patchwork", ~"subscriptions"], _Args, _Caller) ->
    {source, []}.

toggle(Feed, Value) ->
    encode_json({[{~"from", Feed}, {~"value", Value}]}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Create the (empty) table now so it always exists, but defer the
    %% snapshot restore to handle_continue: file2tab of a large snapshot
    %% would otherwise block silkpurse_sup:start_link and thus the whole
    %% node boot (and the shell).  The restores then run concurrently
    %% across the views rather than serialized by the supervisor.
    ets:new(?TAB, [set, named_table, public]),
    {ok, #{}, {continue, register}}.

handle_continue(register, State) ->
    %% Swap in the snapshot (if any), then register the plugin (so the
    %% method appears in the manifest) before the view fold.  Guard each
    %% registration independently so a service that is down (bare eunit
    %% setups) does not skip the other.
    maybe_restore(),
    try plugin_registry:register_plugin(?MODULE)
    catch exit:{noproc, _} -> ok end,
    try view_manager:register_view(?MODULE)
    catch exit:{noproc, _} -> ok end,
    {noreply, State}.

maybe_restore() ->
    File = ?b2l(table_file()),
    case filelib:is_regular(File) of
        false ->
            ok;                        %% no snapshot; keep the empty table
        true ->
            ets:delete(?TAB),
            case (try ets:file2tab(File) catch _:_ -> error end) of
                {ok, ?TAB} -> ok;
                _          -> ets:new(?TAB, [set, named_table, public])
            end
    end.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> catch view_save(), ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

table_file() ->
    <<(config:ssb_repo_loc())/binary, "views/channels.tab">>.

owner() ->
    try keys:pub_key_disp() catch _:_ -> undefined end.

%% Lowercase, strip a leading '#', trim; undefined for a non-channel.
normalize_channel(<<"#", Rest/binary>>) ->
    normalize_channel(Rest);
normalize_channel(B) when is_binary(B) ->
    unicode:characters_to_binary(string:lowercase(string:trim(B)));
normalize_channel(_) ->
    undefined.

bump(Ch, Ts) ->
    {Count, Last} = case ets:lookup(?TAB, {stat, Ch}) of
                        [{_, C, L}] -> {C, L};
                        []          -> {0, 0}
                    end,
    ets:insert(?TAB, {{stat, Ch}, Count + 1, max(Last, sort_key(Ts))}).

set_sub(Ch, Feed, Subscribed, Ts) ->
    T = sort_key(Ts),
    case ets:lookup(?TAB, {sub, Ch, Feed}) of
        %% keep the newer toggle; on an equal timestamp the later-folded
        %% message wins (feeds fold in sequence order), so accept it
        [{_, _Old, OldTs}] when OldTs > T -> ok;
        _ -> ets:insert(?TAB, {{sub, Ch, Feed}, Subscribed =:= true, T})
    end.

sort_key(Ts) when is_integer(Ts) -> Ts;
sort_key(_)                      -> 0.

%% Whether the node owner currently subscribes to Ch (for suggest).
is_subscribed(Ch) ->
    case ets:lookup(?TAB, {sub, Ch, owner()}) of
        [{_, Sub, _}] -> Sub;
        []            -> false
    end.

%% Feeds currently subscribed to Ch.
subscribers(Ch) ->
    ets:foldl(fun({{sub, C, F}, true, _}, Acc) when C =:= Ch -> [F | Acc];
                 (_, Acc)                                    -> Acc
              end, [], ?TAB).

%% Channels whose name contains Text (empty text matches all), most posts
%% first, capped at Limit, as [{id, count, subscribed}].
suggest(Text, Limit) ->
    Needle = unicode:characters_to_binary(string:lowercase(Text)),
    Matched = ets:foldl(
                fun({{stat, Ch}, Count, _Last}, Acc) ->
                        case string:find(Ch, Needle) of
                            nomatch -> Acc;
                            _       -> [{Ch, Count} | Acc]
                        end;
                   (_, Acc) -> Acc
                end, [], ?TAB),
    Sorted = lists:sort(fun({_, A}, {_, B}) -> A >= B end, Matched),
    [ {[{~"id", Ch}, {~"count", Count}, {~"subscribed", is_subscribed(Ch)}]}
      || {Ch, Count} <- lists:sublist(Sorted, Limit) ].

%% The most recently active channel names, newest first, capped at Limit.
recent(Limit) ->
    Acts = ets:foldl(
             fun({{stat, Ch}, _Count, Last}, Acc) -> [{Last, Ch} | Acc];
                (_, Acc) -> Acc
             end, [], ?TAB),
    Sorted = lists:sort(fun({A, _}, {B, _}) -> A >= B end, Acts),
    [Ch || {_Last, Ch} <- lists:sublist(Sorted, Limit)].

encode_json(Term) ->
    iolist_to_binary(message:ssb_encoder(Term, fun message:ssb_encoder/3, [pretty])).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

channels_test_() ->
    {setup, fun ch_setup/0, fun ch_teardown/1,
     fun(_) -> [?_test(index_suggest_recent()),
                ?_test(subscription_tracked())] end}.

ch_setup() ->
    ch_teardown(ignore),
    Home = filename:join("/tmp", "ch_" ++
                          integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = view_manager:start_link(),
    {ok, _} = silkpurse_channels:start_link(),
    Home.

ch_teardown(Home) ->
    [catch gen_server:stop(N)
     || N <- [silkpurse_channels, view_manager, ssb_feed_sup, blobs,
              mess_auth, keys, config]],
    case Home of
        ignore -> ok;
        _ -> os:cmd("rm -rf " ++ Home), application:unset_env(ssb, ssb_home)
    end,
    ok.

post_in(Pid, Channel) ->
    ok = ssb_feed:post_content(
           Pid, {[{~"type", ~"post"}, {~"text", ~"hi"}, {~"channel", Channel}]}),
    ok.

sub_in(Pid, Channel, Subscribed) ->
    ok = ssb_feed:post_content(
           Pid, {[{~"type", ~"channel"}, {~"channel", Channel},
                  {~"subscribed", Subscribed}]}),
    ok.

index_suggest_recent() ->
    Pid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    post_in(Pid, ~"#general"),
    post_in(Pid, ~"#Random"),
    post_in(Pid, ~"#general"),
    %% suggest: general has 2 posts, random 1; #-stripped, lowercased
    ?assertMatch([{[{~"id", ~"general"}, {~"count", 2} | _]}],
                 suggest(~"gen", 20)),
    %% recent lists the active channels (exact order is by timestamp, but
    %% these posts share a millisecond so compare as a set)
    ?assertEqual([~"general", ~"random"], lists:sort(recent(10))).

subscription_tracked() ->
    Pid = utils:find_or_create_feed_pid(keys:pub_key_disp()),
    post_in(Pid, ~"#elm"),
    ?assertNot(is_subscribed(~"elm")),
    sub_in(Pid, ~"#elm", true),
    ?assert(is_subscribed(~"elm")),
    sub_in(Pid, ~"#elm", false),
    ?assertNot(is_subscribed(~"elm")),
    %% suggest reflects the (now unsubscribed) state
    ?assertMatch([{[{~"id", ~"elm"}, {~"count", 1}, {~"subscribed", false}]}],
                 suggest(~"elm", 20)),
    %% subscribe again and check the subscribers list
    sub_in(Pid, ~"#elm", true),
    ?assertEqual([keys:pub_key_disp()], subscribers(~"elm")).

-endif.
