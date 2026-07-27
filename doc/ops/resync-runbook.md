# Runbook: rebuilding a node's data from scratch

*Operational note, July 2026. Written after doing it to EarlButt and
getting it wrong twice, so the traps are the point of the document.*

This covers wiping a node's `.ssberl` while keeping its identity, and
letting it re-replicate from a peer. It is what you do to shake down a
large change to storage or replication, or to recover a node whose
derived state you no longer trust.

Two things about it are genuinely dangerous, and neither is obvious. Read
§1 and §2 before touching anything.

---

## 1. Wiping `.ssberl` deletes the node's OWN feed

`secret` is the identity; `feeds/` is the data — **including this node's
own log**. After a wipe its `last_seq` is 0.

If anything posts before the feed is re-replicated, you write a second
seq 1 and fork your own feed. That is unrecoverable divergence in SSB:
peers that hold the original will reject or ignore the fork, and there is
no merge.

So:

- **Move `.ssberl` aside, do not delete it.** If the own feed does not
  come back you can restore that one `log.offset` and carry on. Deleting
  outright leaves you no move.
- **Do not post anything** — no `sbutt publish`, no client — until the
  own feed is back at its previous sequence.

```
mv ~/.../.ssberl ~/.../.ssberl.bak
mkdir ~/.../.ssberl
cp ~/.../.ssberl.bak/secret ~/.../.ssberl/
```

## 2. Every peer's EBT clock is now wrong about you

This is the one that will actually bite.

The identity survived, so every peer that has ever replicated with this
node still holds a persisted clock saying "it has feed X up to N". ssb-ebt
takes `max(remembered, received)` and **never rewinds**, so those peers
will not re-send what you lost, no matter what clock you advertise.

For a peer you control (silkpurse) the fix is to delete its stored clock
for your id. For a pub you do not control, there is no fix — which is why
**turning the dialer off for the duration is the right call**:

```
# in remsh on the node
peer_dialer:disable().
```

That keeps the resync to the one peer whose clock you can reset.

### Resetting silkpurse's clock — the ordering IS the fix

```
# 1. QUIT silkpurse completely, then verify
pgrep -fl silkpurse           # must be empty

# 2. only NOW delete the clock file
rm ~/.silkpurse/ebt/@<NodeId>.ed25519

# 3. restart silkpurse
```

Step 2 must come after step 1. ssb-ebt holds the clock in memory and
**writes it back on exit**, so deleting the file while silkpurse is
running does nothing at all — the file reappears with the same contents.
This is the mistake that cost an afternoon.

Note the filename uses URL-safe base64: `+` in the feed id is `-` in the
filename, and there is no trailing `=`.

To check what a clock actually claims before deleting it:

```
python3 -c "
import json; d=json.load(open('$HOME/.silkpurse/ebt/@<NodeId>.ed25519'))
print(len(d),'entries'); print(d.get('@<SomeFeed>=.ed25519'))"
```

Values are encoded: `seq * 2` for a normal replicate+receive entry, so
`2400` means sequence 1200. `0` means "has nothing", which is what you
want to see after a reset.

## 3. Deploy

The release contains **platform-specific NIFs** — `enacl` and, since July
2026, `esqlite`. A package built on the Mac will not load on Linux. Build
on the target box:

```
make packages        # on the build machine for that platform
```

`esqlite` needs a C toolchain but no system SQLite (it bundles the
amalgamation). `enacl` needs libsodium, so the box already has a
toolchain.

### If you have an SSH tunnel open, local release commands hit the REMOTE node

`config/vm.args.src` names the node `erlbutt@localhost` and puts Erlang
distribution on port 9100, and the documented way to reach a remote node
is to forward both that and epmd:

```
ssh -L 4369:localhost:4369 -L 9100:localhost:9100 <box>
```

While that tunnel is up, `erlbutt@localhost` **on the laptop resolves to
the node on the box**. Every release command that works over distribution
therefore targets the remote node:

```
./bin/ssb ping             # answers from the REMOTE node
./bin/ssb rpc ...          # runs on the REMOTE node
./bin/ssb remote_console   # attaches to the REMOTE node
./bin/ssb stop             # STOPS THE REMOTE NODE
```

That last one is the reason this section exists. Nothing warns you: the
commands look local, run from a local release directory, and succeed.

The tunnel also makes a local node unstartable — the name is already
taken, and the only symptom is a line in `log/erlang.log.1` saying
`the name erlbutt@localhost seems to be in use by another Erlang node`,
while `bin/ssb ping` cheerfully answers `pong` from the far end.

Both are avoided by naming a local test node differently, which
`vm.args.src` already supports:

```
SSB_NODE=erlbutt_test@localhost SSB_DIST_PORT=9177 ./bin/ssb daemon
```

To check which you are actually talking to before running anything
destructive:

```
lsof -nP -iTCP:9100          # an `ssh` process here means the tunnel is up
./bin/ssb rpc keys pub_key_disp '[]'   # whose identity answers?
```

Note that muxrpc (port 8008) is normally NOT forwarded, so
`sbutt.escript` still talks to a local node and will simply fail to
connect if there is none. It is the distribution port that crosses.

## 4. Start, and do not restart afterwards

Start the node. Then **leave it alone** — restarting it during a resync
gains nothing (its state is correct; nothing on its side persists a wrong
position) and only re-runs view catch-up. Keeping it untouched also makes
any stall you hit a clean signal rather than something you might have
caused.

Before reconnecting the peer, check no stale connection is holding the
duplicate-connection guard:

```
./sbutt.escript health        # == peers == should be empty
```

### Files under `.ssberl` that are no longer read

Derived state moved into `store.db` module by module, and each move left
its old file behind. They are inert — nothing reads them — but they are
easy to mistake for live state when you are deciding what a wipe should
preserve:

| File | Now in |
|---|---|
| `mess_auth.ets` | `msg_author` |
| `friends_graph.tab`, `friends_names.tab` | `social_edges` |
| `views/checkpoints.tab` | `view_checkpoint`, `view_version` |

`views/checkpoints.tab` is the one exception, and only once: on the first
start after the checkpoint port it is imported so the node does not refold
every view over the whole corpus, and it is dead weight from then on. The
import logs `imported N checkpoints`. If you wipe `.ssberl` you lose
nothing by dropping all of these; if you are upgrading in place, keep
`checkpoints.tab` until you have seen that line once.

The remaining `views/*.tab` files ARE live — they belong to the app views
that have not been ported.

## 5. Verify

```
./sbutt.escript health
```

Good output looks like this — core views first, all `ready`, and store
tables filling:

```
== views ==
  ssb_social_graph         core  v2       72 feeds  ready
  ssb_feed_meta            core  v1       72 feeds  ready
  ssb_links                core  v1       72 feeds  ready
  silkpurse_about          app   v1       72 feeds  ready
  ...
== store ==
  link_ids                   192123 rows
  links                      210998 rows
  social_edges                10683 rows
```

Reading it:

- **`feeds on disk` exceeding the per-view feed count is normal.** EBT's
  `full_clock` creates a directory for every feed in the replication set,
  including ones no message has arrived for yet.
- **`== peers ==` does not show self-connections.** A client
  authenticating with the node's own key — `sbutt`, or an owner-mode
  client — is deliberately never registered (`is_self_pk` in `ssb_peer`),
  so "none connected" while `sbutt` runs is normal.
- `health` exits non-zero if a view is still catching up or the store is
  empty, so it works as a post-deploy gate.
- The store section runs `count(*)` per table. On a large store that is
  the slow call, and the most likely one to be interrupted by a dropped
  connection. Sections are printed as they are fetched, so a failure
  there does not discard the earlier output.

The single number that tells you replication is alive:

```
ssb_feed:fetch_last_msg(utils:find_or_create_feed_pid(~"@<SomeFeed>=.ed25519")).
```

Run it twice a minute apart. Climbing means it is working.

**Watch from the node, not the UI.** The failure described in §6 was
completely invisible from silkpurse — the symptom there was only "stuck
scuttling". The diagnosis lived entirely in the node's log.

## 6. When a feed stalls

Symptom in the node's log:

```
feed @YND2...: STALLED at seq 1036 — a peer is offering seq 1038, a gap of
1 message(s).  Its EBT clock is ahead of ours and will not rewind, ...
```

That line is emitted **once per stall**, not once per rejected message,
and a matching line appears when the feed recovers with a count of what
was refused.

**The gap size is the diagnosis.** They mean different things:

| Gap | Meaning |
|---|---|
| exactly 1 | one message was dropped or lost in flight; everything after it is collateral |
| large | the peer resumed from a stale clock position without ever sending the range |

A gap of 1 with no corresponding `rejecting seq N` for the missing
sequence means the node never got the chance to reject it — look for

```
grep 'dropped message' info.log
```

which names the feed and sequence of anything that failed to decode or
store. A message that fails to decode stalls its whole feed, because
every later message then fails the chain check against a tail that never
advanced past the hole.

Either way the remedy is the same: reset the peer's clock per §2. The
node's own state is fine and needs no intervention.

**Do not "fix" this by relaxing the chain check.** Rejecting a message
whose `previous` does not link the tail is what stops a hole being
spliced into a feed and served onward to other peers.

## 7. Recovery

If the own feed does not come back:

```
# restore just that one feed from the backup
cp -r .ssberl.bak/feeds/<2>/<rest>/ .ssberl/feeds/<2>/<rest>/
```

Derived state needs no backup — the store, the views and the ingest
journal are all rebuildable from the logs, and will refold on next start
(`view_manager` does this automatically when a view reports empty).

Once the resync has settled and you have posted nothing that forked,
`.ssberl.bak` can go.
