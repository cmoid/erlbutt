# Testing feed glimpses with a local second node

Standing up two erlbutt nodes on one machine so that one of them
*discovers* what the other says about itself, without replicating it, and
then decides to replicate it after all.

Two nodes are not a convenience here, they are the whole subject. A
glimpse has to **arrive from a peer**, and it is only ever considered for
a feed the receiving node has **deliberately not replicated**. Neither
condition can exist on one node: your own feed is replicated by
definition, and there is nobody to hear it from.

The unit and CT suites cover the mechanism — `glimpse_discovery`'s policy
in isolation, and the full round between two CT nodes in
`erlbutt_two_node_SUITE`. What they cannot show you is the panel with
somebody else's face in it, or how any of this behaves when a blob takes
its time.

## The rule that shapes the whole setup

**A glimpse is only considered for a feed at hops + 1.**

Everything closer is already replicated, and the round skips it with
`{skip, replicated}` — correctly, because for a feed you carry, the real
message is in the log and a peer's copy is not an improvement on it.
Everything further away is not reachable through your follow graph at
all, and the round skips it with `{skip, not_at_boundary}` — also
correctly, because that is the defence against a peer inventing feeds to
interest you.

So with the default `hops = 2`, node A must sit at **three** hops from
node B, which takes **two** throwaway identities in the middle:

```
B  →  Mid1  →  Mid2  →  A
   1       2        3
        replicated      boundary
```

This is the part of the procedure that looks arbitrary and is not, and it
is one identity longer than the archive test needs. The trap to avoid:

| what you set up | where A lands | what happens |
|---|---|---|
| B follows A | hop 1 | replicated; no glimpse considered |
| B → Mid → A | hop 2 | replicated; no glimpse considered |
| B → Mid1 → Mid2 → A | hop 3 | **staged** |

If you would rather not mint two identities, set `{replication_hops, 1}.`
in node B's `ssb.cfg` and one is enough. That is a smaller test of a
different graph, so prefer the chain if you can be bothered.

## Which network you are on

Both nodes must share a network id, and `make rel` builds the **dev**
one — the point being that a node built this way cannot reach the real
network no matter how it is configured.

```
default.vars   1KHLiKZvAvjbY1ziZEHMXawbCEIM6qwjCDm3VYnaR/s=   dev
prod.vars      1KHLiKZvAvjbY1ziZEHMXawbCEIM6qwjCDm3VYRan/s=   mainnet
```

They differ only by transposed characters near the end. A client pointed
at a node on the other network fails in a way that reads like a
connection problem, so check this before debugging anything else — and
note that the client needs it too (`ERLBUTT_SHS`, step 7).

The dev build also ships `{peer_dialer, false}`, so neither node will
dial anything on its own. Every connection in this procedure is one you
made.

## 1. Give node A something to say

A glimpse carries the author's profile and their statement, and nothing
else. Without an `about` there is no name and no face, and the panel
renders a truncated key over a paragraph — technically correct and a
poor look at whether this works.

**Node A should be the dev release, not your everyday node.** Step 1
publishes an `about` and a glimpse to whatever feed it is pointed at, and
both are permanent. `make rel` and run it from
`_build/default/rel/ssb` — which is also the build that cannot reach
mainnet.

With node A running (`bin/ssb console`):

```shell
sbutt about --name "Node A" --description "the far end of a test" \
            --image /path/to/some.png
sbutt glimpse publish --statement "A test feed at the edge of your world."
sbutt glimpse show
```

`glimpse show` prints the whole `glimpses.get` reply, which is what a peer
would receive. Confirm `found: true`, `source: "feed"`, `held: true`, and
a `payload` carrying your profile and statement.

> `sbutt` connects to **localhost:8008** and the port is compiled in, so
> it only ever talks to node A. Everything on node B is done from its
> Erlang shell.

## 2. Copy the release for node B

```shell
rsync -a --exclude '.ssberl' --exclude 'log' \
  _build/default/rel/ssb/ /tmp/ssb-nodeb/
```

**The excludes are the point.** `SSB_HOME` defaults to `"."`, so a node
run from the release directory keeps its data *inside* it — node A's
`.ssberl` is in there, and a plain `cp -r` would give node B node A's
identity into the bargain, which quietly defeats the whole exercise.

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

`ssb_glimpses` is a new core view, so B builds it on first start. On an
empty node that is instant; on a copy of a real data directory it is a
full fold of the log, and nothing below will work until
`view_manager:caught_up(ssb_glimpses)` is `true`.

## 4. Put A three hops from B

From node B's shell. `AId` is node A's feed id (`sbutt whoami` on A):

```erlang
AId = <<"@....ed25519">>,

Follow = fun(Target) -> {[{<<"type">>, <<"contact">>},
                          {<<"contact">>, Target},
                          {<<"following">>, true}]} end,

%% A throwaway one-message feed that follows Target.
Mint = fun(Target) ->
    #{public := Pub, secret := Priv} = enacl:sign_keypair(),
    Id = <<"@", (base64:encode(Pub))/binary, ".ed25519">>,
    Msg = message:new_msg(null, 1, Follow(Target), {Id, base64:encode(Priv)}),
    stored = ssb_feed:store_msg(utils:find_or_create_feed_pid(Id), Msg),
    Id
end,

Mid2 = Mint(AId),            %% follows A
Mid1 = Mint(Mid2),           %% follows Mid2

Self = utils:find_or_create_feed_pid(keys:pub_key_disp()),
ok = ssb_feed:post_content(Self, Follow(Mid1)),
ok = ebt:refresh_repl_set().
```

`base64:encode(Priv)` is not decoration: `message:new_msg/4`
base64-decodes the secret it is handed, and `enacl` returns raw bytes.
Passing the raw key produces a signature that verifies nowhere.

Each middle message is sequence 1 with `previous = null`, so it is a valid
one-message feed and needs no chain behind it.

Now check the shape, which is the thing most likely to be wrong:

```erlang
true  = ebt:replicate_feed(Mid2),        %% two hops: carried
false = ebt:replicate_feed(AId),         %% three hops: deliberately not
true  = sets:is_element(AId, glimpse_discovery:boundary_set()).
```

The follow edges are indexed by a view, so give it a second and retry if
`replicate_feed(Mid2)` is still false.

## 5. Connect, and run a round

```erlang
{ok, Peer} = ssb_peer:start("localhost", 8008,
                            base64:decode(<<"....">>)),   %% A's pubkey, no sigil
ok = glimpse_discovery:run_now(),
ssb_glimpses:edge().
```

What you want to see:

```erlang
[#{feed => <<"@...">>, seq => 3, blob => <<"&...">>,
   size => 412, source => edge, ...}]
```

`source => edge` is the whole point: B is holding a self-description for
a feed it has no other trace of. There is no `ssb_feed` process for A on
B, `mess_auth` has never seen A's messages, and `ssb_feed:current_seq/1`
for A is 0.

The round is on a timer otherwise — first at 90 seconds, then every 10
minutes — and `run_now/0` is the impatient version.

## 6. Watch the payload arrive

The message is a pointer; the blob behind it comes over the ordinary want
path, afterwards and separately.

```erlang
{ok, #{blob := Blob}} = ssb_glimpses:for_feed(AId),
blobs:has(Blob).                       %% false, then true
{ok, Bin} = blobs:fetch(Blob),
utils:nat_decode(Bin).
```

Both states are real and the client renders both, so it is worth seeing
the `false` before you see the `true`.

## 7. Point a client at B

```shell
export ERLBUTT_SECRET=/tmp/ssb-nodeb/.ssberl/secret
export ERLBUTT_ADDR=127.0.0.1:8009
export ERLBUTT_SHS=1KHLiKZvAvjbY1ziZEHMXawbCEIM6qwjCDm3VYnaR/s=
export ssb_appname=silkpurse-local-nodeb
npm start
```

A distinct `ssb_appname` keeps this whole data directory apart from your
everyday client, so nothing here can disturb it.

Navigate to A's feed id. There is no timeline — B holds none of A's
messages — and the panel is the page: A's name and picture, A's
statement, a **Replicate this feed** button, and a provenance footer
saying *self-described, relayed by a peer*, with the caveat that nobody
has vouched for it.

That footer is load-bearing. A glimpse is unanchored: nothing proves it
is the newest one A ever wrote, and the panel has to look like a claim
rather than a verified profile card.

## 8. Promote, and undo it

Click **Replicate this feed**. That pins A locally (`feed_pins`), drops
the staged glimpse, and the panel switches to *Replicating at your
request* with a **Stop replicating** button.

```erlang
feed_pins:list(),                      %% [#{feed => <<"@...">>, source => <<"glimpse">>, ...}]
true = ebt:replicate_feed(AId),
none = ssb_glimpses:for_feed(AId).     %% the stand-in has done its job
```

**A's messages will not start arriving on the open connection.**
`ebt:recompute_repl_set/0` rewrites a cached ETS set and nothing else —
no clock is re-sent, and open sessions are not told. A *fresh* connection
sends a full clock built from that set (`ebt:full_clock/0`), so the newly
pinned feed appears in it at sequence 0 and A starts sending. Drop and
redial to see it:

```erlang
gen_server:stop(Peer),
{ok, Peer2} = ssb_peer:start("localhost", 8008, base64:decode(<<"....">>)),
ssb_feed:current_seq(utils:find_or_create_feed_pid(AId)).   %% climbs
```

This is a rough edge rather than a bug — the same thing happens for any
mid-session change to the replication set — but promotion is the first
feature where a *person* makes that change and then watches for a result,
so it is the first place it reads as broken. Worth fixing by nudging the
open peers after a pin.

Then undo it, because the argument that a glimpse is *advisory* rests
entirely on promotion being cheap in both directions:

```erlang
feed_pins:unpin(AId),
false = ebt:replicate_feed(AId).
```

What already arrived stays. Unpinning stops asking; it does not delete.

## When nothing happens

| symptom | first thing to check |
|---|---|
| No panel at all on A's profile | `ebt:replicate_feed(AId)` on B. If `true`, A is inside the replication set and the panel is silent by design — your chain is one identity short. |
| *"Nothing is known about this feed here."* | `ssb_glimpses:offers()` on **A** — is there anything to serve? Then `peer_registry:all()` on B — is the connection actually registered? A round with no peers is a silent no-op. |
| Staged, but stuck on *"Fetching the summary…"* | `blobs:has(Blob)` on B. The want is recorded either way; a blob that has not come from the currently connected peers will not come from anywhere. |
| Handshake fails, looks like a network fault | Network id. Node, client, both. |
| A second look returns the same nothing | `peek/0` is rate-limited to one round per 30 seconds. Use `glimpse_discovery:run_now/0`. |
| `sbutt` cannot reach node B | It never could: port 8008 is compiled into the escript. Use B's shell. |

## What this procedure is for

The CT suite proves the wiring: signatures verify, the boundary set is
computed from a real follow graph, a stranger's glimpse is refused, the
blob transfers, promotion works. It runs in seconds and it should stay
the first thing you run.

This runbook is for everything the suite cannot assert — whether the
panel reads as a claim rather than a fact, whether the two-state
pointer/payload rendering makes sense while you wait, and whether
promoting a stranger from a paragraph about themselves feels like a
reasonable thing to have been asked to do.
