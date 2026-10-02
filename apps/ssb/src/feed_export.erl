%% SPDX-License-Identifier: GPL-2.0-only
%%
%% Copyright (C) 2026 Charles Moid
%%
%% Writing feeds and their blobs out as a self-contained bundle.  Your
%% data is not sovereign if it is not portable: this is how a feed leaves
%% the node that holds it — onto a USB stick, into another client, into
%% another network.
%%
%% The packaging idea is borrowed from sneakerweb's drop format (one
%% directory, a manifest, an optional human-readable preview); the data
%% model is not.  Willow's entries are a set, and a receiver of a set
%% cannot tell whether anything was withheld.  An SSB feed is a signed
%% hash chain, so a bundle holding messages 1..N provably IS the whole of
%% that feed up to N.  The bundle keeps the chain exactly as signed.
%%
%% LAYOUT
%%
%%   <dir>/manifest.json            what is here (see manifest/5)
%%   <dir>/feeds/<hex>.jsonl.gz     one {"key","value","timestamp"} per
%%                                  line, sequence order, from 1
%%   <dir>/blobs/<hex>              every blob the exported messages
%%                                  reference that this node holds
%%   <dir>/index.html               a plain preview, for a person who
%%                                  does not run SSB at all
%%
%% <hex> is the lowercase hex of the 32 key/hash bytes — always 64 chars,
%% unlike the store's own directory names (utils:decode_id/1 drops
%% leading zeros), which are an erlbutt detail and stay out of here.
%%
%% The lines are the stored records verbatim.  They are what
%% createHistoryStream({keys: true}) yields, so nothing erlbutt-specific
%% leaks into the bundle: the log framing is dropped, and the archive
%% hint files are a local cache an importer rebuilds for itself.  The
%% outer `timestamp` is this node's receive time — useful, but unsigned,
%% so an importer must not trust it.
%%
%% WHAT IS REFUSED.  A feed is exported only if this node holds it from
%% sequence 1 as one unbroken chain.  A floored feed holds a suffix, and a
%% suffix is exactly the case EBT clocks cannot express
%% (doc/research/archive-boundaries.md); exporting one would push that
%% problem onto every importer.  Refusals are recorded in the manifest
%% rather than failing the whole export.
%%
%% Private messages are exported as they sit in the feed: still
%% encrypted.  Their blob references are found by decrypting with our
%% key (as the blob fetcher does), so attachments in our own private
%% messages travel with the bundle.
%%
%% Network keys are deliberately NOT in the manifest.  A private network's
%% key is an access credential, and a bundle is made to be handed to
%% someone.
-module(feed_export).

-include_lib("ssb/include/ssb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([export/2,
         manifest_json/1,
         describe_error/1,
         feed_file/1,
         blob_file/1]).

-define(FORMAT, ~"erlbutt-export").
-define(VERSION, 1).

%% Export Opts#{feeds} (default: our own feed) into OutDir, which must
%% not exist yet.
%%
%% The bundle is assembled in OutDir ++ ".partial" and renamed into place
%% at the end, so OutDir only ever appears complete.
%%
%% {ok, Manifest} | {error, Reason}.  The manifest is returned as the
%% same EJSON that was written to manifest.json.
export(OutDir0, Opts) ->
    OutDir = filename:absname(OutDir0),
    Feeds  = maps:get(feeds, Opts, [keys:pub_key_disp()]),
    Tmp    = OutDir ++ ".partial",
    case filelib:is_file(OutDir) of
        true ->
            {error, {exists, ?l2b(OutDir)}};
        false ->
            %% a .partial is only ever ours, left by an export that died
            _  = file:del_dir_r(Tmp),
            ok = filelib:ensure_dir(filename:join([Tmp, "feeds", "x"])),
            ok = filelib:ensure_dir(filename:join([Tmp, "blobs", "x"])),
            try build(Tmp, lists:usort(Feeds)) of
                {ok, Manifest} ->
                    ok = file:rename(Tmp, OutDir),
                    {ok, Manifest};
                {error, _} = E ->
                    _ = file:del_dir_r(Tmp),
                    E
            catch C:R:St ->
                    _ = file:del_dir_r(Tmp),
                    ?SSB_ERROR("feed_export: ~p:~p ~p", [C, R, St]),
                    {error, {C, R}}
            end
    end.

build(Tmp, FeedIds) ->
    Results = [{Id, export_feed(Id, Tmp)} || Id <- FeedIds],
    Done    = [{Id, Info, Refs, Rows} || {Id, {ok, Info, Refs, Rows}} <- Results],
    Refused = [{Id, Why} || {Id, {refused, Why}} <- Results],
    case Done of
        [] ->
            {error, {nothing_exported,
                     [#{id => Id, reason => reason(Why)} || {Id, Why} <- Refused]}};
        _ ->
            AllRefs = lists:usort(lists:append([R || {_, _, R, _} <- Done])),
            {Blobs, Missing} = copy_blobs(AllRefs, Tmp),
            Manifest = manifest([Info || {_, Info, _, _} <- Done],
                                Blobs, Missing, Refused,
                                erlang:system_time(millisecond)),
            ok = file:write_file(filename:join(Tmp, "manifest.json"),
                                 manifest_json(Manifest)),
            ok = file:write_file(filename:join(Tmp, "index.html"),
                                 preview(Manifest,
                                         [{Info, Rows} || {_, Info, _, Rows} <- Done])),
            {ok, Manifest}
    end.

%%%===================================================================
%%% One feed
%%%===================================================================

-record(walk, {feed,
               fd,
               seq    = 0,
               last   = null,
               refs   = #{},
               rows   = [],
               name   = null}).

%% {ok, Info, BlobRefs, PreviewRows} | {refused, Reason}
export_feed(FeedId, Tmp) ->
    case feed_dir(FeedId) of
        {ok, Dir} ->
            Rel  = feed_file(FeedId),
            Path = filename:join(Tmp, Rel),
            {ok, Fd} = file:open(Path, [write, binary, compressed]),
            Result = try feed_store:fold_feed(fun line/2,
                                              #walk{feed = FeedId, fd = Fd},
                                              Dir)
                     catch throw:{refused, _} = Refused -> Refused
                     after file:close(Fd)
                     end,
            finish(Result, FeedId, Rel, Path);
        refused ->
            {refused, not_held}
    end.

finish({refused, _} = Refused, _FeedId, _Rel, Path) ->
    _ = file:delete(Path),
    Refused;
finish(#walk{seq = 0}, _FeedId, _Rel, Path) ->
    _ = file:delete(Path),
    {refused, empty};
finish(#walk{seq = Seq, last = Last, refs = Refs, rows = Rows, name = Name},
       FeedId, Rel, Path) ->
    Info = #{id     => FeedId,
             name   => Name,
             file   => ?l2b(Rel),
             from   => 1,
             to     => Seq,
             latest => Last,
             sha256 => file_sha256(Path)},
    {ok, Info, maps:keys(Refs), lists:reverse(Rows)}.

%% One stored record.  Checked as a chain — no signature checks, which
%% are the importer's job and were paid when the message was stored —
%% because a feed that does not chain here is a bundle an importer would
%% reject, and it is better to say so now.
line(Data, #walk{feed = FeedId, fd = Fd, seq = Seq, last = Last} = W) ->
    #message{id = Id, author = Author, sequence = MsgSeq,
             previous = Prev} = Msg = message:decode(Data, false),
    if
        Author =/= FeedId -> throw({refused, {wrong_author, Author}});
        MsgSeq =/= Seq + 1, Seq =:= 0 -> throw({refused, {starts_at, MsgSeq}});
        MsgSeq =/= Seq + 1 -> throw({refused, {sequence_gap, Seq, MsgSeq}});
        Seq > 0, Prev =/= Last -> throw({refused, {broken_chain, MsgSeq}});
        true               -> ok
    end,
    Seq > 0 orelse message:is_null_ref(Prev)
        orelse throw({refused, {broken_chain, MsgSeq}}),
    %% Stored records are compact JSON, so one never holds a raw newline;
    %% if one ever did, the line format would silently split it.
    nomatch = binary:match(Data, <<"\n">>),
    ok = file:write(Fd, [Data, $\n]),
    W#walk{seq  = MsgSeq,
           last = Id,
           refs = lists:foldl(fun(R, A) -> A#{R => true} end,
                              W#walk.refs, blob_refs(Msg)),
           rows = [row(Msg) | W#walk.rows],
           name = name_of(Msg, W#walk.name)}.

%% No private key here (or no keys server at all) just means private
%% messages contribute no refs.
blob_refs(Msg) ->
    try blob_fetcher:msg_blob_refs(Msg) catch _:_ -> [] end.

feed_dir(FeedId) ->
    try utils:feed_dir(FeedId) of
        Dir ->
            case filelib:is_dir(Dir) of
                true  -> {ok, ?b2l(Dir)};
                false -> refused
            end
    catch _:_ -> refused
    end.

%%%===================================================================
%%% Blobs
%%%===================================================================

%% Copy every referenced blob we hold.  The ones we do not hold are
%% listed, not fetched: an export is a snapshot of this node, and a
%% bundle that waited on the network could wait forever.
copy_blobs(Refs, Tmp) ->
    lists:foldr(
      fun(Ref, {Have, Miss}) ->
              Src = blobs:file_of(Ref),
              Rel = blob_file(Ref),
              case Src =/= error andalso filelib:is_regular(Src) of
                  true ->
                      {ok, Size} = file:copy(Src, filename:join(Tmp, Rel)),
                      {[#{id => Ref, file => ?l2b(Rel), size => Size} | Have],
                       Miss};
                  false ->
                      {Have, [Ref | Miss]}
              end
      end, {[], []}, Refs).

%%%===================================================================
%%% Manifest
%%%===================================================================

manifest(Feeds, Blobs, Missing, Refused, Now) ->
    #{format       => ?FORMAT,
      version      => ?VERSION,
      created      => Now,
      feeds        => [maps:remove(name, F) || F <- Feeds],
      blobs        => Blobs,
      missingBlobs => Missing,
      refused      => [#{id => Id, reason => reason(Why)}
                       || {Id, Why} <- Refused]}.

reason(not_held)               -> ~"not held on this node";
reason(empty)                  -> ~"no messages";
reason({starts_at, Seq})       -> fmt("held from sequence ~b, not 1 "
                                      "(floored feed)", [Seq]);
reason({sequence_gap, A, B})   -> fmt("sequence gap after ~b (next is ~b)",
                                      [A, B]);
reason({broken_chain, Seq})    -> fmt("previous does not match at ~b", [Seq]);
reason({wrong_author, Author}) -> <<"contains a message by ", Author/binary>>.

%% The manifest as JSON text: what manifest.json holds, and what
%% admin.export answers with.
manifest_json(Manifest) ->
    iolist_to_binary(json:encode(ejson(Manifest))).

%% An export/2 error as one line for a person.
describe_error({exists, Dir}) ->
    <<Dir/binary, " already exists">>;
describe_error({nothing_exported, Refused}) ->
    iolist_to_binary(["nothing exported: ",
                      lists:join(~"; ", [[Id, ~" (", Why, ~")"]
                                         || #{id := Id, reason := Why}
                                                <- Refused])]);
describe_error(Other) ->
    fmt("~p", [Other]).

%% Atom-keyed maps -> the binary-keyed maps json:encode/1 takes.
ejson(M) when is_map(M) ->
    #{atom_to_binary(K) => ejson(V) || K := V <- M};
ejson(L) when is_list(L) ->
    [ejson(V) || V <- L];
ejson(V) ->
    V.

%%%===================================================================
%%% Preview
%%%===================================================================

%% {Seq, ClaimedTimestamp, Type, Text}.  Text is only what a reader
%% would want at a glance; everything else is in the feed file.
row(#message{sequence = Seq, timestamp = Ts, content = {Props}}) ->
    Type = case ?pgv(~"type", Props) of
               T when is_binary(T) -> T;
               _                   -> ~"?"
           end,
    Text = case ?pgv(~"text", Props) of
               X when is_binary(X) -> X;
               _                   -> ~""
           end,
    {Seq, Ts, Type, Text};
row(#message{sequence = Seq, timestamp = Ts}) ->
    {Seq, Ts, ~"private", ~"(encrypted)"}.

%% The feed's own latest self-assigned name, if it has one.
name_of(#message{author = A, content = {Props}}, Name) ->
    case {?pgv(~"type", Props), ?pgv(~"about", Props), ?pgv(~"name", Props)} of
        {~"about", A, N} when is_binary(N) -> N;
        _                                  -> Name
    end;
name_of(_, Name) ->
    Name.

preview(Manifest, FeedRows) ->
    #{created := Now, blobs := Blobs, missingBlobs := Missing,
      refused := Refused} = Manifest,
    ["<!doctype html>\n<html><head><meta charset=\"utf-8\">"
     "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
     "<title>SSB feed export</title><style>"
     "body{font:15px/1.5 system-ui,sans-serif;max-width:60rem;margin:auto;"
     "padding:1rem}td,th{text-align:left;vertical-align:top;padding:.2rem .6rem}"
     "tr:nth-child(even){background:#8881}code{font-size:.85em;word-break:break-all}"
     "</style></head><body>\n<h1>SSB feed export</h1>\n<p>Created ",
     esc(iso8601(Now)),
     ". The signed messages are in <code>feeds/</code>; this page is only "
     "a preview of them.</p>\n",
     [feed_section(Info, Rows) || {Info, Rows} <- FeedRows],
     ~"<h2>Blobs</h2>\n<p>",
     integer_to_binary(length(Blobs)), ~" included",
     case Missing of
         [] -> ~"";
         _  -> [~", ", integer_to_binary(length(Missing)),
                ~" referenced but not held by the exporting node"]
     end,
     ~".</p>\n",
     case Refused of
         [] -> ~"";
         _  -> [~"<h2>Not exported</h2>\n<ul>",
                [[~"<li><code>", esc(Id), ~"</code>: ", esc(Why), ~"</li>"]
                 || #{id := Id, reason := Why} <- Refused],
                ~"</ul>\n"]
     end,
     ~"</body></html>\n"].

feed_section(#{id := Id, name := Name, to := To}, Rows) ->
    [~"<h2>", esc(case Name of null -> Id; _ -> Name end), ~"</h2>\n",
     ~"<p><code>", esc(Id), ~"</code> &mdash; messages 1 to ",
     integer_to_binary(To), ~"</p>\n",
     ~"<table><tr><th>#</th><th>date</th><th>type</th><th>text</th></tr>\n",
     [[~"<tr><td>", integer_to_binary(Seq), ~"</td><td>",
       esc(iso8601(Ts)), ~"</td><td>", esc(Type), ~"</td><td>",
       esc(Text), ~"</td></tr>\n"]
      || {Seq, Ts, Type, Text} <- Rows],
     ~"</table>\n"].

esc(Bin) when is_binary(Bin) ->
    lists:foldl(fun({From, To}, B) -> binary:replace(B, From, To, [global]) end,
                Bin, [{~"&", ~"&amp;"}, {~"<", ~"&lt;"}, {~">", ~"&gt;"},
                      {~"\"", ~"&quot;"}]).

%% The claimed timestamp, as a date.  Anything unusable renders blank
%% rather than failing the export: it is the author's assertion, not ours.
iso8601(Ms) when is_integer(Ms) ->
    try ?l2b(calendar:system_time_to_rfc3339(Ms, [{unit, millisecond},
                                                  {offset, "Z"}]))
    catch _:_ -> ~""
    end;
iso8601(Ms) when is_float(Ms) ->
    iso8601(trunc(Ms));
iso8601(_) ->
    ~"".

%%%===================================================================
%%% Internal
%%%===================================================================

%% Where a feed or blob lives inside a bundle, relative to its root.
%% The importer derives paths with these too, rather than believing the
%% manifest's `file` fields — a bundle is untrusted input, and a `file`
%% of "../../.ssberl/secret" is a path, not a feed.
%%
%% Both raise on anything but a well-formed id.
feed_file(<<"@", _/binary>> = FeedId) ->
    filename:join("feeds", ?b2l(hex_of(FeedId, ~".ed25519")) ++ ".jsonl.gz").

blob_file(<<"&", _/binary>> = BlobId) ->
    filename:join("blobs", ?b2l(hex_of(BlobId, ~".sha256"))).

%% "@<b64>.ed25519" / "&<b64>.sha256" -> 64 lowercase hex chars
hex_of(<<_Sigil, Rest/binary>>, Suffix) ->
    B64 = binary:part(Rest, 0, byte_size(Rest) - byte_size(Suffix)),
    Suffix = binary:part(Rest, byte_size(Rest), -byte_size(Suffix)),
    <<_:32/binary>> = Raw = base64:decode(B64),
    binary:encode_hex(Raw, lowercase).

file_sha256(Path) ->
    {ok, Bin} = file:read_file(Path),
    binary:encode_hex(crypto:hash(sha256, Bin), lowercase).

fmt(F, A) ->
    iolist_to_binary(io_lib:format(F, A)).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

export_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun exports_own_feed_test/1,
      fun includes_archived_history_test/1,
      fun copies_referenced_blobs_test/1,
      fun refuses_existing_dir_test/1,
      fun refuses_unheld_and_floored_test/1,
      fun preview_escapes_test/1]}.

setup() ->
    Home = filename:join("/tmp", "export_" ++
                             integer_to_list(erlang:system_time(microsecond))),
    ok = filelib:ensure_dir(Home ++ "/"),
    application:set_env(ssb, ssb_home, Home),
    {ok, _} = config:start_link("no-such-cfg"),
    {ok, _} = keys:start_link(),
    {ok, _} = ssb_store:start_link(),
    {ok, _} = mess_auth:start_link(),
    {ok, _} = blobs:start_link(),
    {ok, _} = feed_floor:start_link(),
    FeedId = keys:pub_key_disp(),
    {ok, Pid} = ssb_feed:start_link(FeedId),
    {Pid, FeedId, Home}.

teardown({Pid, _, Home}) ->
    catch gen_server:stop(Pid),
    [catch gen_server:stop(Name)
     || Name <- [feed_floor, blobs, mess_auth, ssb_store, keys, config]],
    os:cmd("rm -rf " ++ Home),
    application:unset_env(ssb, ssb_home),
    ok.

post(Pid, Text) ->
    ok = ssb_feed:post_content(Pid, {[{~"type", ~"post"}, {~"text", Text}]}),
    #message{id = Id} = ssb_feed:fetch_last_msg(Pid),
    Id.

out(Home) -> filename:join(Home, "bundle").

read_lines(Dir, #{file := File}) ->
    {ok, Gz} = file:read_file(filename:join(Dir, File)),
    [L || L <- binary:split(zlib:gunzip(Gz), ~"\n", [global]), L =/= ~""].

%% The bundle holds the chain verbatim: every line decodes, with its
%% signature, back to the message that was posted — and the manifest
%% says so.
exports_own_feed_test({Pid, FeedId, Home}) ->
    fun() ->
        K1 = post(Pid, ~"one"),
        K2 = post(Pid, ~"two"),
        {ok, M} = export(out(Home), #{}),
        #{feeds := [Info], refused := []} = M,
        ?assertMatch(#{id := FeedId, from := 1, to := 2, latest := K2}, Info),
        Lines = read_lines(out(Home), Info),
        ?assertEqual([K1, K2],
                     [Id || #message{id = Id, validated = true}
                                <- [message:decode(L, true) || L <- Lines]]),
        %% the manifest on disk is the one returned, and the file hash holds
        {ok, Json} = file:read_file(filename:join(out(Home), "manifest.json")),
        #{~"format" := ~"erlbutt-export", ~"version" := 1,
          ~"feeds" := [#{~"sha256" := Sha}]} = json:decode(Json),
        {ok, Gz} = file:read_file(filename:join(out(Home), maps:get(file, Info))),
        ?assertEqual(Sha, binary:encode_hex(crypto:hash(sha256, Gz), lowercase)),
        ?assert(filelib:is_regular(filename:join(out(Home), "index.html"))),
        ?assertNot(filelib:is_dir(out(Home) ++ ".partial"))
    end.

%% Archived segments are part of the feed.  An export that read only the
%% live log would start at the archive genesis and be refused — or worse,
%% look complete.
includes_archived_history_test({Pid, _FeedId, Home}) ->
    fun() ->
        K1 = post(Pid, ~"before"),
        {ok, _} = ssb_feed:archive(Pid),
        _  = post(Pid, ~"after"),
        #message{sequence = Last} = ssb_feed:fetch_last_msg(Pid),
        {ok, #{feeds := [Info]}} = export(out(Home), #{}),
        ?assertMatch(#{from := 1, to := Last}, Info),
        [First | _] = Lines = read_lines(out(Home), Info),
        ?assertEqual(Last, length(Lines)),
        ?assertMatch(#message{id = K1}, message:decode(First, false))
    end.

copies_referenced_blobs_test({Pid, _FeedId, Home}) ->
    fun() ->
        Data   = crypto:strong_rand_bytes(256),
        Held   = blobs:store(Data),
        Absent = <<"&", (base64:encode(crypto:strong_rand_bytes(32)))/binary,
                   ".sha256">>,
        ok = ssb_feed:post_content(
               Pid, {[{~"type", ~"post"}, {~"text", ~"see attached"},
                      {~"mentions", [{[{~"link", Held}]}, {[{~"link", Absent}]}]}]}),
        {ok, M} = export(out(Home), #{}),
        #{blobs := [#{id := Held, file := File, size := 256}],
          missingBlobs := [Absent]} = M,
        {ok, Copy} = file:read_file(filename:join(out(Home), File)),
        ?assertEqual(Data, Copy),
        ?assertEqual(64, byte_size(filename:basename(File)))
    end.

%% Never write into (or over) something that is already there.
refuses_existing_dir_test({Pid, _FeedId, Home}) ->
    fun() ->
        _ = post(Pid, ~"x"),
        ok = filelib:ensure_dir(filename:join(out(Home), "x")),
        ?assertMatch({error, {exists, _}}, export(out(Home), #{}))
    end.

%% A feed we do not hold is refused, and refused feeds ride along in the
%% manifest without spoiling the ones that did export.  With nothing
%% exportable at all, there is no bundle.
refuses_unheld_and_floored_test({Pid, FeedId, Home}) ->
    fun() ->
        _ = post(Pid, ~"x"),
        Other = <<"@", (base64:encode(crypto:strong_rand_bytes(32)))/binary,
                  ".ed25519">>,
        {ok, M} = export(out(Home), #{feeds => [FeedId, Other]}),
        ?assertMatch(#{feeds := [#{id := FeedId}],
                       refused := [#{id := Other}]}, M),
        Out2 = out(Home) ++ "2",
        ?assertMatch({error, {nothing_exported, [_]}},
                     export(Out2, #{feeds => [Other]})),
        ?assertNot(filelib:is_file(Out2)),
        ?assertNot(filelib:is_file(Out2 ++ ".partial")),
        %% a chain that does not start at 1 is a floored suffix
        W = #walk{feed = FeedId, fd = undefined},
        Msg = message:new_msg(null, 5, {[{~"type", ~"post"}]},
                              {FeedId, keys:priv_key()}),
        ?assertThrow({refused, {starts_at, 5}},
                     line(message:encode(Msg), W))
    end.

%% Bundle paths come from ids alone, and anything that is not a
%% well-formed id raises rather than naming some other path.
bundle_paths_test() ->
    Key = crypto:strong_rand_bytes(32),
    Hex = ?b2l(binary:encode_hex(Key, lowercase)),
    ?assertEqual("feeds/" ++ Hex ++ ".jsonl.gz",
                 feed_file(<<"@", (base64:encode(Key))/binary, ".ed25519">>)),
    ?assertEqual("blobs/" ++ Hex,
                 blob_file(<<"&", (base64:encode(Key))/binary, ".sha256">>)),
    [?assertError(_, F(Bad))
     || F <- [fun feed_file/1, fun blob_file/1],
        Bad <- [~"@../../x.ed25519", ~"&../secret", ~"@.ed25519",
                <<"@", (base64:encode(Key))/binary, ".sha256">>,
                <<"&", (base64:encode(<<1,2,3>>))/binary, ".sha256">>]].

preview_escapes_test({Pid, _FeedId, Home}) ->
    fun() ->
        _ = post(Pid, ~"<script>alert(1)</script>"),
        {ok, _} = export(out(Home), #{}),
        {ok, Html} = file:read_file(filename:join(out(Home), "index.html")),
        ?assertEqual(nomatch, binary:match(Html, ~"<script>")),
        ?assertNotEqual(nomatch, binary:match(Html, ~"&lt;script&gt;"))
    end.

-endif.
