Replication
===========

Once two peers are [connected](networking.md), they exchange the feeds
and blobs each wants from the other. Feeds go over EBT; blobs over a
separate want/have protocol.

The replication set
-------------------

Which feeds a node stores and serves is computed, not configured feed by
feed:

    {self} ∪ follows(self, hops) ∪ room members ∪ pins − blocks

- `follows(self, hops)` walks the follow graph in `ssb_social_graph` out
  to `replication_hops` (default 2: the feeds you follow, and the feeds
  they follow).
- Room members are included when this node is a room.
- Pins (`feed_pins`) are feeds the operator chose to carry without a
  public follow — see [glimpses](feeds-archives-glimpses.md#glimpses-and-pins).
- Blocks remove a feed even if the graph reaches it, and a blocked feed's
  author is refused.

`ebt` caches the set and recomputes it on a timer and whenever the graph
changes (debounced), so a new follow starts replicating without a
restart.

Epidemic broadcast trees
------------------------

EBT (`ebt`, `ebt_vc`) is the SSB replication protocol, run as a muxrpc
duplex stream, `ebt.replicate`.

1. Each side sends a **vector clock**: for every feed in its replication
   set, the latest sequence it holds and whether it wants to *receive*
   updates for that feed from this peer.
2. Each side then sends the other every message the other is missing,
   for the feeds where the other asked to receive.
3. As new messages arrive — published locally or received from another
   peer — they are pushed along every stream whose peer wants that feed,
   and clocks are re-exchanged periodically (anti-entropy, every 30
   seconds) to catch anything missed.

A received message is validated by its feed's `ssb_feed` process —
signature, `previous` and sequence — before it is stored. With
`require_valid_sigs` (on in every release) a message that fails is
rejected; off, it is stored and counted, which exists only to measure a
network before enforcing.

Opening a connection and asking it to replicate are two separate steps,
and only `peer_dialer` does both. A connection opened any other way
answers RPC but replicates nothing until one side asks; `ebt` logs a
warning once for such a connection so it is not mistaken for a working
one.

Peers remember what they have sent you. Their clock for your feed only
moves forward, so a node whose data was wiped and rebuilt needs care
before it reconnects — see [Rebuilding a node](../guide/rebuilding-a-node.md).

The legacy `createHistoryStream` call (one feed, from a sequence) is
served too, for clients and older peers that replicate that way.

Archive boundaries change where a feed *starts*: a node can adopt a
validation floor and replicate a feed from partway down. That is covered
in [feeds, archives and glimpses](feeds-archives-glimpses.md).

Blobs
-----

Blobs are not part of feeds; messages refer to them by hash
(`&<base64>.sha256`). When a message is stored, `social_msg` scans its
content for blob references and hands each to `blob_fetcher`.

- Blobs we do not hold become **wants**, advertised to every connected
  peer over `blobs.createWants` — two one-way source streams, one each
  direction. A negative number is a want (its magnitude is how far it
  may propagate), a positive one is a *have* with the blob's size.
- When a peer says it has one, the blob is fetched with `blobs.get`,
  hash-verified, and stored in `blobs/`.
- Wants are re-sent to each new connection and re-broadcast every five
  minutes. Fetches are capped per connection (eight in flight), a want
  that keeps failing backs off, and a want nobody answers in 30 days is
  dropped. These limits exist because an uncapped fetcher once turned a
  large backlog into a want storm.
- `{blob_scan, true}` makes the node scan every stored message for blob
  references at startup — a full fold, so off by default.

Owner clients add blobs with `blobs.add` (a sink: the client streams the
bytes and ends with `true`) and can ask the node to want one
(`blobs.want`) or push one to peers (`blobs.push`).

Plumtree
--------

`apps/plumtree` is an alternative epidemic broadcast — the Plumtree
algorithm (Leitão et al., 2007), adapted from the version that came out
of Riak. Peers keep an *eager* set that receives full messages and a
*lazy* set that receives only announcements; a duplicate prunes its
sender to lazy, and an announcement for something unseen grafts it back.

Its transport is Erlang distribution, not SSB connections, so it is for
clusters of erlbutt nodes that trust one another (they share a cookie).
It is started with the node but does nothing until it is given peers
(`initial_peers` in its application environment). It is not used for
ordinary SSB replication.

Further reading
---------------

- [research/papers/ebt.pdf](../research/papers/ebt.pdf) — the epidemic
  broadcast trees paper.
- [Scuttlebutt protocol guide](https://ssbc.github.io/scuttlebutt-protocol-guide/)
  — EBT and blobs on the wire.
