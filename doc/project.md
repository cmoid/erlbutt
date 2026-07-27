Project notes
=============
These notes are random thoughts about design I'm having while prototyping. I've been following the scuttlebutt community for 9 years now, and playing with some of the implementation details. Erlbutt wants to be something like an [`ssb-server`][4]. It implements the original core of the scuttlebutt [protocol][7]. At some point this doc will become an overview of the architecture and high level documentation of the code. I say something like an `ssb-server` because though I'd like to use it that way I have quite a different approach with respect to persistence. `ssb-server` is also in my mind a bad name. Peer to peer systems should not have servers and clients. All nodes are `peers` and should act like both clients and servers, so the distinction becomes moot.

Introduction
-------------

Scuttlebutt is an interesting protocol. A feed is an immutable append-only log file, identified by a public key, where each message is signed by a private key and references the prior message. This ensures a single source of truth since only one peer can sign messages on a feed. At any time, different peers might have different versions of the same feed that only differ by the length. In other words one can be a prefix of the other. If one uses the same private key on a different machine and the feed is not at the latest sequence than the feed becomes forked. This breaks replication. Nostr handles this by not using feeds, each message is just signed. Nostr also is not peer to peer, it's uses relays that act as servers and centralize control. So scuttlebutt makes it hard to use the same private key on multiple machines, though it is doable. One needs to ensure before using the private key for signing posts that the feed has the latest sequence number.

Messages are encoded as [json][8] (newer implementations have attempted to improve this with various binary formats). Messages have a content object that has a type. Messages of type `follow` form the basis for the social graph that is used to drive the replication process. Feeds follow other feeds more or less. This makes for a simple replication, where one node in the network asks a peer for all messages of a feed beyond the latest sequence number they have.

It's unfortunate that the social graph definition is in the content part of the message. It seems to be more an application concern. At the system level of connections, feeds and replication, there shouldn't be any awareness of the content. It's hard to say though. It could be perfectly acceptable for the follow graph to be computed from the content of the messages. Otherwise additional data structures would be need to store and propagate the follow graph, making for a more complicated replication. The social graph drives the replication, once connections to other peers are made. Regardless though, almost anything can be worked around because the feeds are immutable, so a collection of feeds will only ever grow and anything needed can be built and maintained incrementally.

The message content can store other application specific data as well, .eg. conversation threads, sometimes called tangles, can be assembled using fields in the content that track the root, replies and branches of a thread. Because of the immutability, a peer's database only ever grows larger. Data subject to change, like profiles, can be computed incrementally. All sorts of indices and other data can be captured from the feed and stored separately for fast retrieval.

Persistence
-----------

A key difference from the original `SSB` persistence is to store each feed's data separately, rather than one large log file. The idea is something like a log manager rather than a database. In erlbutt each feed maps to an erlang `gen_server`, which is responsible for fetching and storing the messages of that feed. Most OSes now support quite a few file handles open in memory, so erlbutt can support a few thousand `gen_server` objects, each managing a file or two. It would seem such a thing might run pretty slowly but experience shows it is fast enough. The assumption that drives this design is that dunbar's number limits the number of feeds a given user will care about at any point in time. On top of that a user may only wish to see the profile or access a couple of posts in a feed.

### Two kinds of state

The organising idea is that a node holds exactly two kinds of state, and they want opposite things from storage.

**Truth** is the signed messages and the blobs. It is immutable, append-only and self-verifying: its integrity comes from the signature chain, not from any storage engine. It needs almost nothing from a database — don't lose bytes, don't reorder within a feed — and it needs to stay in SSB's own format, because that is what keeps erlbutt interoperable with the rest of the network. So it lives in flat files.

**Derivation** is everything else: every index, graph, cache and checkpoint. All of it is reconstructable by refolding the logs, so durability is an optimisation rather than a correctness property, but it is mutable, random-access and query-shaped. So it lives in an embedded SQL store.

Nearly every awkwardness in an SSB implementation comes from mixing these two up — from storing an index as a log because logs were what the system already had.

### The truth tier

Each feed directory holds its own history:

    feeds/<2>/<rest>/log.offset                  live messages, framed
    feeds/<2>/<rest>/log.offset.<From>-<To>.gz   frozen segments
    feeds/<2>/<rest>/log.offset.<From>-<To>.hint {MsgId, Seq, Offset, Len}

A feed's log is periodically archived: the live log is gzipped into a frozen segment, stored as a blob, and announced in an `archive` message on the owner's own feed, so the archive is itself part of the replicated record. Each segment gets a hint file beside it listing what is inside and where, which is a borrowing from bitcask — a lookup reads small sidecars and decompresses only the segment that actually contains the id, rather than scanning.

Two more files sit alongside the feeds. `ingest.journal` records `{FeedId, Seq}` refs in the order messages arrived, which is genuinely new information — it cannot be recovered from the feeds themselves, since arrival order is local. And `blobs/` is a content-addressed store of opaque bytes.

Within a feed, an in-memory index maps message id to byte offset, populated as messages are written and completed in a single pass the first time a feed is read from. Every read verifies that the record it lands on is the one it expected, so an index that has gone stale — because something rewrote the log underneath it — is detected rather than believed.

### Three layers

The code is organised into three layers, and the line between them is drawn by asking what a *non-social* application built on erlbutt would need.

**`apps/ssb` — the foundation.** The protocol: connections, handshakes, replication, feeds, blobs, rooms. It knows about `contact` and `about` messages, because replication cannot work without a follow graph, but it knows nothing about posts, threads or votes.

**`apps/ssb_conv` — the conventions.** SSB application conventions that more than one client needs but that are not protocol: reading a set of cross-feed references as a conversation, and the message types that go with it. Pure functions, no processes. This layer exists because thread assembly is needed by more than one client, and duplicating it in each would be wrong while promoting it into the foundation would be worse.

**Applications.** `apps/silkpurse` serves the muxrpc surface a patchwork-family UI expects. `apps/admin` is an owner-only control plane — dialer, view status and rebuild, peers, store inspection — reachable by any client rather than only from a shell. `apps/plumtree` sits orthogonal to all three, an alternative replication strategy.

The dependency direction is one-way: `ssb_conv` depends on `ssb`, applications depend on both, and nothing in `apps/ssb` may call upward. Erlang will happily link such a call, so a test enforces it.

The introduction above worries that the social graph being defined in message content is really an application concern. The layering is the answer to that worry: the graph is indeed computed from content, but it belongs to the foundation because replication is what consumes it. The test is not where a fact comes from, it is who needs it.

### Views

A view folds stored messages into queryable state. `view_manager` owns the lifecycle: it keeps a per-feed sequence checkpoint for every view, replays what a view missed when it registers, rebuilds from scratch when a view's version changes, and fans out each newly stored message. It is flume's role.

Views come in two classes. **Core views** belong to the foundation, are always on, and are what an application may assume exists. **App views** are registered by applications on top. Core views are delivered each message first, so an app view folding the same message sees core state already updated.

The catch-up fold runs in chunks inside the manager's message loop rather than inside the registration call, so replication keeps flowing while a large view rebuilds. A view that is still catching up receives no live messages — delivering one would advance its checkpoint past everything the fold had not yet reached, leaving a permanent hole — and the fold sweeps until a whole pass delivers nothing.

There are three core views:

**`ssb_social_graph`** holds follow and block edges. This is protocol machinery: `ebt` asks it who to replicate and who to refuse.

**`ssb_feed_meta`** holds the latest self-asserted metadata for each feed, as `(feed, key, value, seq)`. The mechanism — last-write-wins metadata, folded from the log — is foundational, because anything that renders a feed id needs it. The schema is not: `name`, `image` and `description` are patchwork conventions, so no field is privileged and an application with entirely different self-description gets the same machinery.

**`ssb_links`** holds every cross-feed reference as an edge. A message carrying the id of another message, feed or blob anywhere in its content produces a row. Extraction is deliberately lexical — the content is walked and anything shaped like a reference is recorded along with the field it was found at — so the foundation indexes a message type it has never heard of.

That last one is worth dwelling on, because it is the most interesting thing in the store. A cross-feed reference is a cryptographically enforced happens-before: if message M in feed A carries the id of message N in feed B, then A held N when it wrote M, because you cannot reference a hash you have not seen. So the per-feed sequence edges together with the cross-feed reference edges form a DAG whose transitive closure is a *causal partial order* over the whole database — Lamport happens-before derived from content rather than from protocol bookkeeping. SSB gives a total order within a feed and none at all across feeds, and its timestamps are self-asserted and routinely absurd, so this is the only trustworthy cross-feed ordering signal there is.

It has real limits. It is partial and sparse: it says "A knew B", not when. And it is not the arrival order in `ingest.journal`, which is total but purely local and meaningless to any other peer. Two different orderings, for two different jobs.

Reading those edges as a *conversation* — roots, branches, forks, depth — is a separate thing, and lives in the convention layer. The index is the graph; the tangle is one interpretation of it.

### The store

Derived state lives in a single SQLite database (`store.db`) through `esqlite`, a NIF that bundles the amalgamation so there is no system dependency.

Reads run in the calling process on a shared connection, so a view query is a plain function call rather than a round-trip through a server — without that, replacing an ETS lookup with a store lookup would be a serious regression on every read path. Writes go through one server, which matches SQLite's single-writer model and gives one place to batch them. Batching matters more than it looks: every call into the NIF occupies a dirty IO scheduler for its duration, and that pool is small, so a transaction per batch runs at hundreds of thousands of rows per second where a transaction per row does not.

A view declares its tables with a version; the schema is applied when the version changes. Ids are interned to integers — SQLite's own rowid does the work, via an upsert with `RETURNING` — which matters because the alternative is storing 53-byte identifiers several times per row, and on a large corpus that is the difference between a store the size of the logs and a fraction of it.

The payoff is not only durability that costs O(change) instead of rewriting whole tables on a timer. It is that queries become queries: "who follows this person" is an indexed lookup rather than a fold over every feed, and transitive follows out to N hops is one recursive CTE rather than a hand-rolled breadth-first walk.

Some views remain on ETS with periodic snapshots. That is a matter of which shapes justified moving, not a plan half-executed — a table that is genuinely an edge set or a graph earns SQL, while a small rollup may not.

### What this buys

Because the truth tier is immutable and everything else is derived, a node can throw away its entire derived state and rebuild it from the logs. That makes the store safe to change: a schema is a decision that can be revised, not a migration to be feared. It also means the only thing that genuinely needs backing up is `feeds/`, `blobs/` and `secret`.

The historical design note, including the survey that led here and the measurements behind the engine choice, is kept separately as research.


----
[0]: https://github.com/rebar/rebar3
[1]: https://viewer.scuttlebot.io/%25pYmFr6d0QwLP%2BYG0VNoo75PP7eYNZ1Y8C2MC9IjF5aw%3D.sha256
[2]: http://localhost:8989/blobs/get/&4DUnrqwI7xxUpP6omK1wiPSco5uLrNa6Ey7lNrXCzCU=.sha256
[3]: https://github.com/cn-uofbasel/ssbdrv/blob/master/doc/tangle.md
[4]: https://github.com/ssbc/ssb-server
[5]: https://cloud.google.com/healthcare/
[6]: https://github.com/flumedb/flumedb
[7]: https://ssbc.github.io/scuttlebutt-protocol-guide/
[8]: https://ssbc.github.io/scuttlebutt-protocol-guide/#message-format
