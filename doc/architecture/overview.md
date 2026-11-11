Architecture overview
=====================

Erlbutt is a Scuttlebutt peer written in Erlang/OTP. It implements the
original core of the [protocol][7] — secret handshake, box stream, muxrpc,
signed append-only feeds, epidemic broadcast tree replication, blobs —
and adds pubs, invites and rooms on top. It plays the part
[`ssb-server`][4] plays in the JavaScript world, but it is not built like
it, and the biggest difference is persistence.

It is deliberately not called a server. Peer-to-peer systems should not
have servers and clients: every node is a peer, acting as both, so the
distinction is moot. A UI is a client only in the narrow sense that it
drives the node. Silkpurse and HighWire do it over muxrpc, holding the
node's own key. maxbutt, the Emacs client, does it over Erlang
distribution, running inside the node (see
[networking](networking.md#clients-over-erlang-distribution)).

Protocol extensions
-------------------

erlbutt implements the protocol as other implementations do, and adds two
extensions of its own: **partial feeds** (archive boundaries) and **feed
glimpses**. Both are additive and optional. A peer that knows nothing
about them is still a correct Scuttlebutt implementation and
interoperates with erlbutt unchanged. They are **not part of the core
protocol**: they are proposals, implemented here first.

The wire-level descriptions are in the
[Extensions section][ext] of the protocol guide erlbutt maintains (a
fork of the ssbc guide), so another implementation can be compatible
with them; that section is marked as proposed extensions, separate
from the core protocol the rest of the guide describes. How erlbutt implements them is in
[feeds, archives and glimpses](feeds-archives-glimpses.md), and the
reasoning behind them is in `doc/research`.

This page is the map. The pages beside it go into each part:

| Page | What it covers |
|---|---|
| [storage.md](storage.md) | Per-feed logs, archive segments, the ingest journal, blobs, `store.db` |
| [views.md](views.md) | `view_manager`, core and app views, checkpoints, rebuilds |
| [networking.md](networking.md) | Connections, handshake, muxrpc dispatch, dialing, invites, rooms |
| [replication.md](replication.md) | EBT, the replication set, blob replication, plumtree |
| [feeds-archives-glimpses.md](feeds-archives-glimpses.md) | Own-feed safety, archive boundaries, validation floors, glimpses, pins |
| [plugins.md](plugins.md) | Extending the node: `ssb_plugin`, `ssb_view`, `plugin_loader` |
| [rpc-methods.md](rpc-methods.md) | The muxrpc surface, by namespace and access class |

Why things are the way they are — the alternatives considered and the
measurements behind the choices — is in [`doc/research`](../research/).

Scuttlebutt in brief
--------------------

A feed is an immutable append-only log, identified by a public key, in
which every message is signed by the matching private key and names the
message before it. Only one key can sign a feed, so there is a single
source of truth: two peers holding the same feed can differ only in
length, one copy a prefix of the other. If the same key signs two
different messages at the same sequence — typically because it was used
on a second machine that was behind — the feed *forks*, and replication
of it breaks. Erlbutt guards against that hard (see
[own-feed safety](feeds-archives-glimpses.md#publishing-safely-the-fork-guard)).

Messages are [JSON][8], with a `content` object that has a `type`.
Messages of type `contact` form the follow graph, and the follow graph
drives replication: a node replicates the feeds it follows, out to a
configurable number of hops. Other content types are application data —
posts, likes, profiles (`about`), conversation threads built from `root`
and `branch` references. Because feeds only ever grow, anything derived
from them — indexes, profiles, threads — can be built incrementally and
rebuilt from scratch.

Two kinds of state
------------------

The organising idea is that a node holds exactly two kinds of state, and
they want opposite things from storage.

**Truth** is the signed messages and the blobs. It is immutable,
append-only and self-verifying: its integrity comes from the signature
chain, not from any storage engine. It needs almost nothing from a
database — don't lose bytes, don't reorder within a feed — and it needs
to stay in SSB's own format, because that is what keeps erlbutt
interoperable. So it lives in flat files: one log per feed, rather than
one large log for everything.

**Derivation** is everything else: every index, graph, cache and
checkpoint. All of it is reconstructable by refolding the logs, so
durability is an optimisation rather than a correctness property — but
it is mutable, random-access and query-shaped. So it lives in an
embedded SQLite database.

The payoff: a node can throw away its entire derived state and rebuild
it from the logs. That makes the store safe to change — a schema is a
decision that can be revised, not a migration to be feared — and it means
the only things that genuinely need backing up are `feeds/`, `blobs/` and
`secret`. Details in [storage.md](storage.md).

The per-feed design rests on an assumption worth stating: Dunbar's number
limits how many feeds a person actually cares about, and for most of
those they want the profile and a few recent posts. Each feed is an
Erlang process owning a file or two, started on demand. A node
replicating a few thousand feeds runs a few thousand such processes,
which the BEAM handles comfortably.

Three layers
------------

The code is organised into three layers, and the line between them is
drawn by asking what a *non-social* application built on erlbutt would
need.

**`apps/ssb` — the foundation.** The protocol: connections, handshakes,
replication, feeds, blobs, rooms, the store and the view machinery. It
knows about `contact` and `about` messages, because replication cannot
work without a follow graph, but it knows nothing about posts, threads or
votes.

**`apps/ssb_conv` — the conventions.** SSB application conventions that
more than one client needs but that are not protocol: reading a set of
cross-feed references as a conversation (`tangle`), and the message
types that go with it. Pure functions, no processes.

**Applications.**

- `apps/silkpurse` serves the muxrpc surface a Patchwork-family UI
  expects (`patchwork.*`, `messagesByType`, `friends.*`, …). The node
  never depends on it.
- `apps/admin` is an owner-only control plane — dialer, view status and
  rebuild, peers, store inspection, export and import — reachable by any
  owner client rather than only from an Erlang shell.
- `apps/plumtree` sits beside the others: an alternative epidemic
  broadcast strategy over Erlang distribution.
- **Plugins** such as [ssb_git](https://github.com/cmoid/ssb_git) live
  outside the tree and are loaded at boot. See [plugins.md](plugins.md).

The dependency direction is one way: `ssb_conv` depends on `ssb`,
applications depend on both, and nothing in `apps/ssb` may call upward.
Erlang will happily link such a call, so a test
(`apps/ssb/test/ssb_layering_tests.erl`) enforces it.

One might worry that the social graph, being defined in message content,
is really an application concern. The layering answers that: the graph
is computed from content, but it belongs to the foundation because
replication is what consumes it. The test is not where a fact comes
from, it is who needs it.

Process structure
-----------------

`ssb_sup` starts the foundation's services in dependency order, one for
one. In outline:

1. `config`, then `ssb_store` (everything after declares tables in it),
   `plugin_registry`, `keys`, `blobs`, `mess_auth` and the small
   persistent tables (invites, room members, `conn_db`).
2. `ingest_journal`, `feed_floor`, then `ssb_feed_sup` — the
   `simple_one_for_one` supervisor of per-feed processes — and
   `own_feed`, whose boot check reads our own feed.
3. `view_manager`, then the core views: `ssb_social_graph`,
   `ssb_feed_meta`, `ssb_links`, `ssb_archives`, `ssb_glimpses`.
4. Replication and discovery: `feed_pins`, `ebt`, `peer_registry`,
   `boundary_discovery`, `glimpse_discovery`, the room services,
   `blob_fetcher`, `peer_dialer`.
5. `plugin_loader`, last, because plugins may use everything above.

The TCP listener (ranch) is started by `ssb_app`; each connection is an
`ssb_peer` process with its own `rpc_processor`.

A data directory is locked to one running node (`store_lock`, a `LOCK`
file), and `data_version` refuses to start against data written by a
newer build.

----
[4]: https://github.com/ssbc/ssb-server
[7]: https://cmoid.github.io/scuttlebutt-protocol-guide/
[8]: https://cmoid.github.io/scuttlebutt-protocol-guide/#message-format
[ext]: https://cmoid.github.io/scuttlebutt-protocol-guide/#extensions
