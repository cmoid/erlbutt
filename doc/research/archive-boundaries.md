# Archive boundaries

*A way for a Scuttlebutt feed to retire its early history without breaking
replication, and for a new peer to start following it partway down.*

**Status: implemented in [erlbutt](https://github.com/cmoid/erlbutt), not
proposed anywhere.** This document exists to be argued with. I would
rather find out here that the design is wrong than after a protocol-guide
PR. Nothing below is a request to change anything you maintain — the first
question is whether the idea is worth anyone's time at all.

---

## The problem

A new peer that wants to follow a feed fetches and validates it from
sequence 1. There is no other option: each message names its predecessor,
so the chain can only be checked from the beginning.

For most feeds that is fine. For old ones it is the dominant cost of
onboarding. A concrete case — the feed this was built for:

| | |
|---|---|
| Messages | 12,527 |
| Span | April 2017 – August 2026 |
| Compressed size | 3,771,786 bytes |

Nine years of history, every message signature-checked, before the first
post anyone actually wants to read appears. Multiply by a follow list and
onboarding a new device is measured in hours.

The history is not junk — it is the reason the feed is trustworthy. But
requiring *every* new reader to take *all* of it is a policy, not a
consequence of the cryptography, and it is worth asking whether the
reader can be allowed to choose.

## What this deliberately does not change

This is the part I would most like checked, because everything else rests
on it.

**No change to message validation.** Not one rule. A message is still
valid iff its signature verifies, its `previous` names its predecessor,
and its sequence is one greater.

**No change to the feed.** An archiving author publishes an ordinary
message — real `previous`, correct sequence, normal signature — that
happens to carry `type: "archive"`. A client that has never heard of
archiving sees an unknown message type and ignores it, exactly as it
ignores every other type it does not implement.

**Existing followers are unaffected.** They are already at the archive
message's predecessor, so it chains for them like any other message. They
notice nothing.

> **This was not true in the first version, and the bug is instructive.**
> The archive message was originally published with `previous: null`, on
> the reading that archiving starts the feed over. That is a chain break
> for everyone already replicating you: their copy stops dead at the
> archive point, permanently. It also destroyed the seam check described
> below, since there was no longer anything to join to. Publishing the
> real predecessor fixes both. If you take one thing from this document,
> take that: **the archive message must be an ordinary in-chain
> successor.**

The only thing that changes is what the *author's node* keeps in the log
it serves from, and what a *new reader* may choose to skip.

## The archive message

When a feed archives, it freezes the current live log into a gzipped
segment, stores that as a blob, and publishes:

```json
{
  "type": "archive",
  "archive": "&EriS6LOlXjueXUNdRBnQzBSWv4/1A2OC+9S6Y61ZkcE=.sha256",
  "from_sequence": 1,
  "to_sequence": 12527,
  "size": 3771786,
  "from_timestamp": 1491735203064,
  "to_timestamp": 1787681358391
}
```

| Field | Meaning |
|---|---|
| `archive` | Blob holding the frozen segment. |
| `from_sequence` / `to_sequence` | Inclusive range of messages inside it. |
| `size` | Byte count of the blob as stored — what fetching costs on the wire. |
| `from_timestamp` / `to_timestamp` | First and last timestamps in the segment. |

`size` is there so a client can offer *"fetch the earlier history — 3.6 MB"*
rather than naming a price only after paying it. The sequence range gives
a message count but not a cost.

The timestamps are author-asserted, like every SSB timestamp. They are fine
for labelling a range in a UI and should not be trusted for ordering or
anything else.

## The seam

This is the whole security argument for the transfer, and it is short.

Call the archive message *G*, at sequence `N`. The segment claims to hold
`1 … N-1`. A receiver checks:

1. Every message in the segment verifies against the feed's public key.
2. Each message's `previous` names the one before it, and sequences run
   consecutively.
3. **`id(last message in segment) == G.previous`**
4. `to_sequence + 1 == N`.

Step 3 is the seam. `G.previous` is a hash the author signed, so a segment
that joins there is that feed's own history and nothing else. A wrong
segment — including a truthful segment from a *different* feed, or an
altered version of this one — fails at a hash comparison.

Nothing here trusts whoever served the blob. Content addressing gets the
bytes; the seam decides whether they are the right bytes.

## Discovery

One RPC, a `source`, callable by anyone:

```
archives.boundaries   ->  [signed archive message, ...]
```

No arguments. The peer volunteers every boundary it knows.

Asking feed-by-feed would be the obvious shape and is the wrong one:
almost no feeds are archived, so it spends a round trip to hear "no"
nearly every time, and feeds × peers is the shape of a want storm. This
is one short stream per connection, usually empty.

**What goes on the wire is the author's own signed message, verbatim.**
The receiver verifies that signature itself, so a peer relaying a boundary
is never trusted for any of it — it is a hint, not an assertion.

## Adopting a floor

Here is the mechanism, and it is smaller than it sounds.

A receiver that accepts boundary *G* at sequence `N` seeds its state for
that feed to:

```
last_sequence = N - 1
last_message  = G.previous
```

It now believes it holds `1 … N-1` without holding it. When *G* arrives
through ordinary replication, the ordinary validation rule passes: the
sequence is one greater, and `previous` matches `last_message`.

**No special case anywhere in the ingest path.** The archive message is
validated by the same code as every other message. Everything after *G*
replicates normally.

That is the entire protocol content of this proposal: *a client may seed a
feed's starting point from a verified archive message it chose to accept.*
Not "relax validation" — the validation rules are untouched. Just a
different, cryptographically justified, starting point.

### Policy is the receiver's

erlbutt's rules, offered as a starting point rather than a
recommendation:

- **Never floor your own feed.** You authored it.
- **Never floor a direct follow.** If you chose this person deliberately,
  keep their history — see witnesses, below.
- **Only floor feeds you are already replicating**, i.e. reached at a
  distance through the follow graph.
- **Prefer the lowest boundary offered**, keeping the most history.

The last one matters and is the opposite of the greedy choice.

## Archives chain

Each archive freezes only the live log of its day, so a feed archived
twice has two segments and two boundaries:

```
segment 1..12527      boundary at 12528
segment 12528..12539  boundary at 12540
```

Fetching the boundary at 12540 returns `12528…12539` and nothing below.
That is correct, and it reads exactly like a bug the first time you see
it.

After importing a segment, the first message recovered *is* the previous
archive message — so a client re-floors to that boundary and can offer the
next layer down. Peel until the feed is whole, at which point the floor
clears for good.

## What it costs

Two real costs. I would rather state them plainly than have someone find
them.

### Witnesses

A peer that floors gives up the ability to detect rewriting of the history
below its floor. It never had those messages, so it cannot notice if the
author later publishes a different segment for that range.

Feeds are still append-only and forks are still detectable *from the floor
forward*. But the deep past is now attested by the seam hash alone, and by
however many peers kept the full history and would notice a mismatch.

**Archiving therefore trades a diffuse property — every replicator is a
witness — for a concentrated one, where only long-standing followers are.**
That is why the policy above never floors a direct follow: the people you
chose deliberately are the ones you should stay a witness for.

I do not think this is fatal, but it is the thing a reviewer should attack
first.

### A floored peer is a partial replica, and EBT cannot say so

This is the sharper problem, and the one I would most like help with.

A vector clock entry is a single integer meaning **"I hold a prefix of
length N."** There is no way to express *"I hold 12528 through 12540."*

So a floored node advertising its current sequence is claiming more than
it has. A peer that asks it for that feed from the beginning gets messages
starting at the floor — orphans, chaining to nothing it holds. A strict
client rejects them, which is merely noisy. A lenient one stores them as
though they were the feed and relays them onward, which is how one
truncated feed becomes several corrupted replicas.

erlbutt's answer is a guard: a node refuses to serve any request that
starts below what it can actually chain to, ending the stream with an
error rather than sending orphans. That is honest and prevents the
damage — but it is a workaround. The floored node simply stops helping
propagate that feed.

Expressing a *suffix* in the availability model is a deeper change than
anything else in this document, and I have no proposal for it. It may be
the strongest argument for treating archiving as an implementation
detail rather than a protocol feature.

## Serving, and why this is safe to deploy today

An archiving node still has the segments on disk. It can decompress and
serve them on request, which makes archiving **invisible to the network**:

- A client that knows nothing about boundaries asks from sequence 1 and
  gets the whole feed, exactly as before.
- A client that adopts a boundary asks from there and never triggers a
  decompression.

The optimisation is entirely the requester's choice. Serving is passive,
so making the history *available* does not make anyone take it.

erlbutt does this by default. The consequence is worth being explicit
about: **archiving as shipped is a local storage change with no effect on
any other client.** The compression saving is real; the onboarding saving
currently only accrues between two erlbutt nodes.

An operator can turn serving off, which is how a node declines to carry
its own history. Then newcomers must get it from some other peer, and the
feed is unfollowable by anyone who cannot floor. That is a deliberate,
reversible choice — not the default.

## Honest wart in the current implementation

The blob is erlbutt's own on-disk log framing:

```
<<Len:32, Message:Len/binary, Len:32, NextOffset:32>>
```

`NextOffset` is an absolute file position in the log the segment came
from. Outside that file it is meaningless. erlbutt ignores it when
verifying, so it is inert — but it is implementation detail leaking into
a wire format, and if this ever became a spec the container should be
defined neutrally instead. I mention it because it is exactly the sort of
thing that quietly becomes permanent.

## What I am asking

Not for anyone to implement this. Specifically:

1. **Is the "no validation change" claim actually true**, or have I missed
   a client where seeding a feed's starting point breaks an assumption I
   cannot see from here?

2. **Is the witness trade acceptable?** It is the real cost, and I may be
   undercounting it.

3. **Is the suffix-availability gap fatal?** If EBT cannot express partial
   holdings, floored peers are permanently second-class relays. Is there a
   shape for this I have not thought of?

4. **Is there any appetite for this at all?** I am aware that metafeeds
   were thoroughly specified, had multiple implementations, and still did
   not land. I would rather learn that this belongs as an erlbutt-local
   feature than repeat that.

If the answer to (4) is no, that is a useful answer and I will keep it
local. The implementation works either way.

---

*Charles Moid — erlbutt. Implementation:
[ssb_feed.erl](https://github.com/cmoid/erlbutt/blob/main/apps/ssb/src/ssb_feed.erl),
[archive_verify.erl](https://github.com/cmoid/erlbutt/blob/main/apps/ssb/src/archive_verify.erl),
[boundary_discovery.erl](https://github.com/cmoid/erlbutt/blob/main/apps/ssb/src/boundary_discovery.erl).
Test procedure:
[doc/ops/archive-testing.md](https://github.com/cmoid/erlbutt/blob/main/doc/ops/archive-testing.md).*
