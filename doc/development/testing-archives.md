# Testing archive boundaries with a local second node

Standing up two erlbutt nodes on one machine so that one of them
*discovers* the other's archive boundary, adopts a floor, and fetches the
history behind it. This is the only way to exercise the onboarding path
end to end: everything below the boundary has to be absent, and it is not
absent on the node that did the archiving.

The unit and CT suites cover the mechanism. They did not catch three real
bugs that this procedure found in an afternoon, listed at the bottom.

## The rule that shapes the whole setup

**A feed you follow directly is never floored.**

Floors exist for feeds at the edge of your world — the ones you replicate
because someone you follow follows them. Anything you follow yourself is
fetched in full, deliberately.

So node B cannot simply follow node A: that would replicate A from
sequence 1 and there would be no floor to test. B has to reach A at *two
hops*, which is why step 4 mints a throwaway identity whose only purpose
is to sit in the middle.

That third identity is the part of this procedure that looks arbitrary and
is not.

## Which network you are on

Both nodes must share a network id, and `make rel` builds the **dev**
one — the point being that a node built this way cannot reach the real
network no matter how it is configured.

```
default.vars   1KHLiKZvAvjbY1ziZEHMXawbCEIM6qwjCDm3VYnaR/s=   dev
prod.vars      1KHLiKZvAvjbY1ziZEHMXawbCEIM6qwjCDm3VYRan/s=   mainnet
```

They differ only by transposed characters near the end, which is easy to
miss by eye and worth checking whenever a handshake fails for no visible
reason. A client pointed at a node on the other network fails in a way
that reads like a connection problem.

## 1. Give node A something to archive

Archiving is manual unless a length is configured — `?DEFAULT_ARCHIVE_LENGTH`
is `undefined`, so nothing archives on its own:

```erlang
Self = utils:find_or_create_feed_pid(keys:pub_key_disp()),
ssb_feed:current_seq(Self),          %% where you are now
ssb_feed:archive(Self).
```

Archiving freezes the live log **as it stands**, writing
`log.offset.<from>-<to>.gz` and publishing an archive-genesis message.
Post a few messages afterwards, so A has history on both sides of the
boundary and you can tell restored history from replicated history when
you look at a timeline.

Note the feed id and the boundary sequence; both are needed below.

To make archives happen on their own — useful if you want several layers
without doing this repeatedly:

```erlang
config:set_archive_length(500).
```

## 2. Copy the release for node B

```shell
rsync -a --exclude '.ssberl' --exclude 'log' \
  _build/default/rel/ssb/ /tmp/ssb-nodeb/
```

**The excludes are the point.** `SSB_HOME` defaults to `"."`, so a node run
from the release directory keeps its data *inside* it — node A's
`.ssberl` is in there, and on a real feed that is tens of gigabytes. A
plain `cp -r` copies the entire database and gives node B node A's
identity into the bargain, which quietly defeats the whole exercise.

A correct copy is about 60M.

## 3. Start node B

```shell
SSB_HOME=/tmp/ssb-nodeb \
SSB_PORT=8009 \
SSB_NODE=erlbutt-b@localhost \
SSB_DIST_PORT=9200 \
SSB_DIST_PORT_MAX=9210 \
/tmp/ssb-nodeb/bin/ssb console
```

Let it mint its own `secret`. B must be a different peer.

Two settings exist for this procedure and are worth knowing about:

- `SSB_PORT` (`config/sys.config.src`) — the listen port. It was hardcoded
  to 8008 until this test needed a second node, which is a good example of
  something no unit test would ever have noticed.
- `SSB_DIST_PORT_MAX` (`config/vm.args.src`) — a small range above
  `SSB_DIST_PORT`. `bin/ssb remote_console` starts a *second* Erlang node
  that reads the same `vm.args`, and pinning the port exactly means it
  tries to bind the port the running node already holds. It fails with a
  bare "Could not connect". `vm.args.prod` keeps the exact pin on purpose —
  a public box should not open a range.

## 4. Put A two hops away

From node B's shell. `AId` is node A's feed id:

```erlang
AId = <<"@....ed25519">>,

%% A throwaway identity in the middle, which follows A.
#{public := Pub, secret := Priv} = enacl:sign_keypair(),
MidId = <<"@", (base64:encode(Pub))/binary, ".ed25519">>,
Follow = fun(Target) -> {[{<<"type">>,<<"contact">>},
                          {<<"contact">>,Target},{<<"following">>,true}]} end,
Msg = message:new_msg(null, 1, Follow(AId), {MidId, base64:encode(Priv)}),
stored = ssb_feed:store_msg(utils:find_or_create_feed_pid(MidId), Msg),

%% B follows the middle identity, so A lands at two hops.
Self = utils:find_or_create_feed_pid(keys:pub_key_disp()),
ok = ssb_feed:post_content(Self, Follow(MidId)),
ok = ebt:refresh_repl_set(),
true = ebt:replicate_feed(AId).
```

`base64:encode(Priv)` is not decoration: `message:new_msg/4` base64-decodes
the secret it is handed, and `enacl` returns raw bytes. Passing the raw
key produces a signature that verifies nowhere.

The middle message is sequence 1 with `previous = null`, so it is a valid
one-message feed and needs no chain behind it.

## 5. Connect, and adopt the boundary

```erlang
{ok, Peer} = ssb_peer:start("localhost", 8008,
                            base64:decode(<<"....">>)),   %% A's port and pubkey, no sigil
ok = boundary_discovery:run_now(),
feed_floor:get(AId).
```

What you want to see:

```erlang
{ok, #{state => <<"floored">>, floor_seq => 12528,
       from_seq => 1, to_seq => 12527, blob => <<"&...">>}}
```

and `ssb_feed:current_seq(utils:find_or_create_feed_pid(AId))` reading
exactly `floor_seq - 1`. B is claiming everything below the boundary
without holding it, then climbing from the boundary as EBT delivers —
never fetching what is underneath. That is the design.

## 6. Point a client at B

```shell
export ERLBUTT_SECRET=/tmp/ssb-nodeb/.ssberl/secret
export ERLBUTT_ADDR=127.0.0.1:8009
export ERLBUTT_SHS=1KHLiKZvAvjbY1ziZEHMXawbCEIM6qwjCDm3VYnaR/s=
export ssb_appname=silkpurse-local-nodeb
npm start
```

A distinct `ssb_appname` matters. The search index is keyed on the
identity, but a separate app name keeps the whole data directory apart
from your everyday client, so nothing here can disturb it.

Open A's profile. The archive message should be the **oldest** thing B
has, with the boundary footer offering the history below it.

Clicking Fetch is the real test, because B does not hold the blob and will
not want it on its own: the first click reports *"Downloading the
archive…"*, `blob_fetcher` pulls it from A, and the flow then verifies the
seam, installs the segment and refolds the feed.

## Archives chain — one fetch peels one layer

Each archive freezes only the live log of its day, so a feed archived
twice has two segments and two boundaries:

```
log.offset.1-12527.gz        boundary 12528    covers 1..12527
log.offset.12528-12539.gz    boundary 12540    covers 12528..12539
```

Fetching boundary 12540 returns 12528..12539 and **nothing below**, which
is correct and not a bug. Everything under 12528 sits behind the earlier
boundary, inside a different blob.

`archives.fetch` re-floors after each restore — the first message of a
recovered segment *is* the previous archive genesis — so the footer
reappears offering the next layer down and you peel again. When the last
layer lands the feed is complete from sequence 1, the floor clears for
good, and the footer stays gone. That terminal state is worth confirming
rather than assuming; the failure mode it replaced was a footer that
vanished with history still missing.

To set a floor by hand on a node that already knows the boundary:

```erlang
[Bnd] = [X || #{seq := S} = X <- ssb_archives:for_feed(AId), S =:= 12528],
feed_floor:set(AId, message:decode_value(maps:get(raw, Bnd), true)).
```

## Traps

**Restored messages render as nothing.** If a timeline is empty while
avatars and follows appear, the message *bodies* are unreachable even
though the index found them — view state comes from folds, timelines come
from bodies. Historically this was `install/5` writing segments directly
and bypassing `mess_auth:put/2`; a client resolves an id through
`mess_auth` before fetching a body, so 627 indexed roots rendered as 6.
Repair without redeploying:

```erlang
mess_auth:rebuild().
```

**`make test` fails while a node is running.** Suites that start the app
in the CT node take the default listen port and collide with node A. The
peer suites use high ports deliberately; if you add a suite that starts
`ssb`, give it its own port.

**A stale release under `/tmp`.** Node B runs from a copy, so rebuilding
node A changes nothing for B. Re-`rsync` after every build, or you will
debug behaviour that the code no longer has. `code:add_patha/1` plus
`c:l/1` hot-loads a single module if you would rather not restart:

```erlang
code:add_patha("/tmp/ssb-nodeb/lib/ssb-<vsn>/ebin"),
c:l(ssb_archives).
```

## Starting over

```shell
rm -rf /tmp/ssb-nodeb
```

Node B holds nothing you need — a new identity and a re-run of steps 2
through 5 costs a couple of minutes, and is more trustworthy than trying
to reason about a half-migrated floor. Node A is untouched throughout:
this procedure only ever reads from it.
