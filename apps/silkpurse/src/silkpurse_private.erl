%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Private feed rollup: the thread index behind patchwork's private feed.
%% Like silkpurse_threads but over the private messages we can read — a
%% stored message whose content is a `.box` we decrypt with our key.
%%
%% IMPORTANT: only metadata (ids, counts, timestamps, author) is kept in
%% the view and its on-disk snapshot — never the decrypted plaintext.
%% Bodies are decrypted on demand at query time, so private content is
%% not written to disk beyond the feed's own at-rest encryption.
%%
%% An ssb_view over a named public ETS set
%%   RootId => #{author, ts, total, recent :: [{ReplyId, Ts}], last}
%% plus an ssb_plugin serving privateFeed.roots/latest (source, owner).
-module(silkpurse_private).

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

-define(TAB, silkpurse_private).
-define(MARKER, '$complete').
-define(RECENT_KEEP, 8).
-define(RECENT_SHOW, 3).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% ssb_view callbacks
%%%===================================================================

view_version() -> 1.

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

%% Decrypt just enough to thread the message; index ids/counts only.
view_entry(#message{id = Id, author = Author, timestamp = Ts, content = Box})
  when is_binary(Box) ->
    case decrypt_content(Box) of
        {ok, {Props}} ->
            case classify(?pgv(~"type", Props), ?pgv(~"root", Props)) of
                root ->
                    set_root(Id, Author, Ts),
                    {events, [{priv, Id}]};
                {reply, RootId} ->
                    add_reply(RootId, Author, Id, Ts),
                    {events, [{priv, RootId}]};
                ignore ->
                    ok
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
    [{[~"patchwork", ~"privateFeed", ~"roots"],  source, owner},
     {[~"patchwork", ~"privateFeed", ~"latest"], source, owner}].

handle_rpc([~"patchwork", ~"privateFeed", ~"roots"], Args, _Caller) ->
    Opts    = opts(Args),
    Reverse = maps:get(reverse, Opts, true),
    Limit   = maps:get(limit, Opts, undefined),
    Resume  = maps:get(resume, Opts, undefined),
    Ordered = order(gather(), Reverse, Resume),
    Limited = take(Ordered, Limit),
    {source, [{json, encode_json(Item)}
              || {RootId, Summary} <- Limited,
                 (Item = item(RootId, Summary)) =/= undefined]};

handle_rpc([~"patchwork", ~"privateFeed", ~"latest"], _Args, _Caller) ->
    EventFun =
        fun({priv, RootId}) ->
                case ets:lookup(?TAB, RootId) of
                    [{RootId, Summary}] ->
                        case item(RootId, Summary) of
                            undefined -> skip;
                            Item      -> {send, encode_json(Item)}
                        end;
                    _ -> skip
                end
        end,
    {live_source, [], ?MODULE, EventFun}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?TAB, [set, named_table, public]),
    {ok, #{}, {continue, register}}.

handle_continue(register, State) ->
    maybe_restore(),
    try plugin_registry:register_plugin(?MODULE)
    catch exit:{noproc, _} -> ok end,
    try view_manager:register_view(?MODULE)
    catch exit:{noproc, _} -> ok end,
    {noreply, State}.

maybe_restore() ->
    File = ?b2l(table_file()),
    case filelib:is_regular(File) of
        false -> ok;
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
%%% Internal: indexing
%%%===================================================================

table_file() ->
    <<(config:ssb_repo_loc())/binary, "views/private.tab">>.

classify(~"post", undefined)               -> root;
classify(~"post", R) when is_binary(R)     -> {reply, R};
classify(~"about", R) when is_binary(R)    -> {reply, R};
classify(_, _)                             -> ignore.

set_root(RootId, Author, Ts) ->
    Cur = current(RootId),
    ets:insert(?TAB, {RootId, Cur#{author => Author, ts => Ts,
                                   last => max_ts(maps:get(last, Cur), Ts)}}).

add_reply(RootId, _ReplyAuthor, ReplyId, Ts) ->
    Cur = current(RootId),
    Recent = insert_recent({ReplyId, Ts}, maps:get(recent, Cur)),
    ets:insert(?TAB, {RootId, Cur#{total  => maps:get(total, Cur) + 1,
                                   recent => Recent,
                                   last   => max_ts(maps:get(last, Cur), Ts)}}).

current(RootId) ->
    case ets:lookup(?TAB, RootId) of
        [{RootId, Summary}] -> Summary;
        []                  -> #{author => undefined, ts => undefined,
                                 total => 0, recent => [], last => 0}
    end.

insert_recent(Entry, Recent) ->
    Deduped = lists:keydelete(element(1, Entry), 1, Recent),
    Sorted = lists:sort(fun({_, A}, {_, B}) -> A >= B end, [Entry | Deduped]),
    lists:sublist(Sorted, ?RECENT_KEEP).

max_ts(A, B) when is_integer(A), is_integer(B) -> max(A, B);
max_ts(undefined, B) -> B;
max_ts(A, undefined) -> A;
max_ts(_, _)         -> 0.

%%%===================================================================
%%% Internal: query
%%%===================================================================

opts([{Props}]) ->
    lists:foldl(
      fun({~"reverse", V}, Acc) when is_boolean(V) -> Acc#{reverse => V};
         ({~"limit",   V}, Acc) when is_integer(V) -> Acc#{limit => V};
         ({~"resume",  V}, Acc) when is_integer(V) -> Acc#{resume => V};
         (_, Acc) -> Acc
      end, #{}, Props);
opts(_) ->
    #{}.

%% Private threads whose root has been seen (author known).
gather() ->
    ets:foldl(
      fun({?MARKER}, Acc) -> Acc;
         ({RootId, #{author := A} = S}, Acc) when is_binary(A) ->
              [{RootId, S} | Acc];
         (_, Acc) -> Acc
      end, [], ?TAB).

order(Threads, Reverse, Resume) ->
    Cmp = case Reverse of
              true  -> fun({_, #{last := A}}, {_, #{last := B}}) -> A >= B end;
              false -> fun({_, #{last := A}}, {_, #{last := B}}) -> A =< B end
          end,
    Sorted = lists:sort(Cmp, Threads),
    case Resume of
        undefined -> Sorted;
        _ -> [T || {_, #{last := L}} = T <- Sorted,
                   case Reverse of true -> L < Resume; false -> L > Resume end]
    end.

take(List, undefined) -> List;
take(List, N) when is_integer(N), N >= 0 -> lists:sublist(List, N);
take(List, _) -> List.

%% The roots item with decrypted root + recent replies, or undefined if
%% the root body can no longer be decrypted.
item(RootId, #{total := Total, recent := Recent, last := Last}) ->
    case decrypted(RootId) of
        {RootProps} ->
            Replies = [R || {Id, _Ts} <- lists:sublist(Recent, ?RECENT_SHOW),
                            (R = decrypted(Id)) =/= undefined],
            {RootProps ++ [{~"totalReplies", Total},
                           {~"latestReplies", Replies},
                           {~"bumps", []},
                           {~"rts", Last}]};
        undefined ->
            undefined
    end.

%% The stored private message decrypted to a {key, value, timestamp}
%% EJSON envelope (content object, private: true), or undefined.
decrypted(MsgId) ->
    case fetch_raw(MsgId) of
        #message{content = Box} = Msg when is_binary(Box) ->
            case decrypt_content(Box) of
                {ok, ContentObj} ->
                    try utils:nat_decode(message:encode_decrypted(Msg, ContentObj))
                    catch _:_ -> undefined
                    end;
                _ -> undefined
            end;
        _ -> undefined
    end.

fetch_raw(MsgId) ->
    case mess_auth:get(MsgId) of
        not_found -> undefined;
        Author ->
            try
                Pid = utils:find_or_create_feed_pid(Author),
                ssb_feed:fetch_msg(Pid, MsgId)
            catch _:_ -> undefined
            end
    end.

%% {ok, ContentObj} when Box is a private message we can read.
decrypt_content(Box) ->
    case private_box:is_private(Box) andalso private_box:decrypt(Box) of
        {ok, Plain} ->
            try {ok, utils:nat_decode(Plain)}
            catch _:_ -> error
            end;
        _ ->
            error
    end.

encode_json(Term) ->
    iolist_to_binary(message:ssb_encoder(Term, fun message:ssb_encoder/3, [pretty])).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

private_test_() ->
    {setup, fun pv_setup/0, fun pv_teardown/1,
     fun(_) -> [?_test(rolls_up_private_thread())] end}.

pv_setup() ->
    pv_teardown(ignore),
    Home = filename:join("/tmp", "pv_" ++
                          integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = ssb_feed_sup:start_link(),
    {ok, _} = view_manager:start_link(),
    {ok, _} = silkpurse_private:start_link(),
    Home.

pv_teardown(Home) ->
    [catch gen_server:stop(N)
     || N <- [silkpurse_private, view_manager, ssb_feed_sup, blobs,
              mess_auth, keys, config]],
    case Home of
        ignore -> ok;
        _ -> os:cmd("rm -rf " ++ Home), application:unset_env(ssb, ssb_home)
    end,
    ok.

rolls_up_private_thread() ->
    Me     = keys:pub_key_disp(),
    OwnPid = utils:find_or_create_feed_pid(Me),
    ok = ssb_feed:post_private(
           OwnPid, {[{~"type", ~"post"}, {~"text", ~"dm root"},
                     {~"recps", [Me]}]}, [Me]),
    #message{id = RootId} = ssb_feed:fetch_last_msg(OwnPid),
    ok = ssb_feed:post_private(
           OwnPid, {[{~"type", ~"post"}, {~"text", ~"dm reply"},
                     {~"root", RootId}, {~"recps", [Me]}]}, [Me]),
    %% a public post must NOT enter the private feed
    ok = ssb_feed:post_content(OwnPid, {[{~"type", ~"post"}, {~"text", ~"public"}]}),
    {source, Items} =
        handle_rpc([~"patchwork", ~"privateFeed", ~"roots"], [{[]}],
                   #{class => owner, feed_id => Me}),
    Decoded = [utils:nat_decode(B) || {json, B} <- Items],
    ?assertEqual(1, length(Decoded)),
    [{Props}] = Decoded,
    ?assertEqual(RootId, proplists:get_value(~"key", Props)),
    ?assertEqual(1, proplists:get_value(~"totalReplies", Props)),
    {Value} = proplists:get_value(~"value", Props),
    ?assertEqual(true, proplists:get_value(~"private", Value)),
    {Content} = proplists:get_value(~"content", Value),
    ?assertEqual(~"dm root", proplists:get_value(~"text", Content)).

-endif.
