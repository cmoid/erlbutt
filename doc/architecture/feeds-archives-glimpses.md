Feeds, archives and glimpses
============================

Three things erlbutt does with feeds beyond storing and replicating
them: it refuses to fork its own feed, it lets a feed retire its early
history without breaking anyone's copy, and it lets a node learn about a
feed before deciding to replicate it. The last two are erlbutt's own
extensions; both are designed so that peers that know nothing about
them are unaffected.

Publishing safely: the fork guard
---------------------------------

A feed forks when its key signs two different messages at the same
sequence, and a forked feed stops replicating. The node signs at one past
whatever its local log holds, so publishing forks the feed whenever the
log is *behind* the feed's real head: a secret restored onto an empty or
old store, a store rebuilt and only partly replicated, the same secret
on a second device, an unsynced tail lost to a power cut after peers had
it.

The log cannot tell it is behind. `own_feed` combines two things that
can:

- **The own-head record** (`own_heads`): per identity, the highest
  sequence this store has held and whether a peer has ever confirmed it.
  A log below its record has lost messages.
- **Peers' EBT notes**: each carries the peer's sequence for our feed. A
  peer ahead of us means the feed went on without this store.

From those the node is in one of three states:

| State | Meaning | Publishing |
|---|---|---|
| `confident` | the record is confirmed and the log reaches it | allowed (offline too) |
| `unconfirmed` | no record, or one no peer has confirmed yet — a new identity and a restored secret look the same here | allowed, logged |
| `behind` | a peer is ahead, or the log is below its record | **refused**; the node pulls its own feed from every connected peer until it catches up |

One node, one identity, one feed: the data directory is locked to a
single running node (`store_lock`) precisely because two nodes on one
store would sign with one key.

Archive boundaries
------------------

A new peer following a feed must fetch and validate it from sequence 1,
because each message names its predecessor. For a feed with years of
history that dominates onboarding. Archive boundaries let the reader
choose to start partway down, without changing a single validation rule.

**Archiving** (`ssb_feed`): the author's node gzips its live log into a
frozen segment, stores it as a blob, and publishes an ordinary in-chain
message:

```json
{
  "type": "archive",
  "archive": "&...sha256",
  "from_sequence": 1,
  "to_sequence": 12527,
  "size": 3771786,
  "from_timestamp": 1491735203064,
  "to_timestamp": 1787681358391
}
```

It has a real `previous` and the next sequence, so existing followers
see just another message. Archiving happens automatically every
`archive_length` messages if that is set (it is not, by default), or by
hand with `ssb_feed:archive/1` from a remote shell. The segment stays in
the feed directory with a hint file, so the author's node can still
serve the whole feed.

**Discovery** (`boundary_discovery`): every connected peer is asked, in
rounds, for `archives.boundaries` — a bare source of every signed
archive message it knows. The receiver verifies each signature itself,
so a relaying peer is never trusted. For a feed in its replication set
that it holds nothing of, the node can **adopt a validation floor**: it
records (`feed_floor`) that it accepts the chain from the archive
message onward, and replicates from there. It takes the *lowest* offer
in a round, and decides only after hearing from every peer, since a
floor can only be set once. `{archive_floors, false}` turns adoption
off and replicates every feed whole.

**Paying off a floor later** (`archive_verify`): the segment can be
fetched and checked — every message's signature, the chain within the
segment, and the **seam**: the last message in the segment must hash to
the archive message's `previous`. That one comparison proves the segment
is this feed's own history; nothing about who served the blob is
trusted. Archives chain, so each fetch peels back one layer.

**Serving** (`archive_serving`, on by default): a node that holds a
feed's history keeps serving it. Turning it off makes the node refuse
requests below the boundary — cheaper, but newcomers then need another
peer for that history.

Two limits are worth knowing. A floor accepts the author's claim about
their own past without having witnessed it, so it is safest on a feed
many peers have replicated all along. And a floored node is a partial
replica, which EBT has no way to say: a clock entry is a single latest
sequence, so it cannot express "from K onward". (Exports refuse floored
feeds too, since a bundle must hold a feed from sequence 1.)

Glimpses and pins
-----------------

The follow graph decides which feeds you replicate, so a feed just
beyond it is invisible until you follow it — and following is a public
statement about a stranger. Glimpses let a feed describe itself to
peers at the edge of its graph.

**The glimpse** is a signed message on the author's feed naming a blob:

```json
{ "type": "glimpse", "blob": "&...sha256", "size": 41283, "updated": 1788233201436 }
```

The blob holds the author's own profile and a statement in their own
words — nothing the node infers about them. `sbutt glimpse publish`
writes one.

**Where they live** (`ssb_glimpses`): the `glimpses` core view holds the
glimpse of every feed we replicate, and that is all we serve. A second
table, `glimpse_edge`, holds what peers offered for feeds at our
boundary. That table is *not* derived — nothing in our logs mentions
those feeds — so a view rebuild leaves it alone.

**Discovery** (`glimpse_discovery`): in rounds, every connected peer is
asked for `glimpses.offers`, a bare source of the glimpses it holds. The
receiver keeps only offers for its **boundary set** — feeds one hop
beyond its replication hops — and takes the highest sequence for each.
The graph decides candidacy; the glimpse only informs. `{glimpses,
false}` stops asking.

**Promotion is a pin** (`feed_pins`): replicating a feed you found this
way adds it to the replication set privately, without a follow. Undoing
it deletes a row, and nobody else ever knew. Unpinning stops asking for
the feed but keeps what already arrived. A block still wins over a pin.
`sbutt glimpse edge`, `promote` and `demote` drive this from the
command line; silkpurse has buttons for it.

Further reading
---------------

- [research/archive-boundaries.md](../research/archive-boundaries.md)
  and [research/feed-glimpses.md](../research/feed-glimpses.md) — the
  full designs, the arguments for their safety, and the open problems.
- [Testing archives](../development/testing-archives.md) and
  [testing glimpses](../development/testing-glimpses.md) — two-node
  procedures to watch each one work.
