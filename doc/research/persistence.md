# Persistence in erlbutt

*Design note, July 2026. A survey of everything the node writes to disk,
what each piece actually is in database terms, where the current model
strains, what a more general storage model would look like, and how the code
should layer — SSB foundation, shared conventions, applications.*

The SSB log format was chosen deliberately: byte-compatibility with the
JS ecosystem, an obvious migration path, and the property that matters
most — **logs are immutable, information only ever increases**. That
choice is still right for the messages themselves. This note argues that
it has been over-applied: several things that are not logs are stored as
logs, and several things that are not databases are being asked to
behave like one.

## 1. What we persist today

### Tier 1 — truth (immutable, append-only, self-verifying)

| What | Where | Format |
|---|---|---|
| Signed messages, per feed | `feeds/<2>/<rest>/log.offset` | `<<Len:32, Msg:Len, Len:32, NextOff:32>>` frames |
| Frozen history | `feeds/…/log.offset.<From>-<To>.gz` | gzipped frames; also stored as a blob and announced in a `type: archive` message |
| Arrival order across feeds | `ingest.journal` | same framing; payload `term_to_binary({FeedId, Seq})` |
| Blobs | `blobs/<2>/<rest>` | opaque bytes, content-addressed |
| Identity | `secret` | JSON, write-once |
| Node config | `ssb.cfg` | Erlang terms via `file:consult`, read-only, cached in a protected ETS table |

Modules: `ssb_feed`, `feed_store`, `ingest_journal`, `blobs`, `keys`,
`config`.

### Tier 1½ — secondary copies of messages, written as logs

Written by `ssb_feed:store/2` alongside the main log:

| File | Contents |
|---|---|
| `feeds/…/profile` | every `about` message of that feed, duplicated |
| `feeds/…/contacts` | every `contact` (follow/block) message, duplicated |
| `feeds/…/references` | tangle arc records `{root, src, tar}` — written into the **target author's** directory, not the author's own |

These are indexes wearing a log's clothes. See §3.

### Tier 2 — derived state (ETS in RAM, `ets:tab2file` snapshots)

| File | Owner | Shape |
|---|---|---|
| `mess_auth.ets` | `mess_auth` | `msgid -> author`, set; 60 s flush; `rebuild/0` refolds every feed |
| `views/checkpoints.tab` | `view_manager` | `{Mod, feed, FeedId} -> Seq`, `{Mod, version} -> V` |
| `views/friends_graph.tab`, `_blocks.tab`, `_names.tab` | `friends` | follow graph, block graph, profile names |
| `views/silkpurse_about.tab` | `silkpurse_about` | set |
| `views/silkpurse_backlinks.tab` | `silkpurse_backlinks` | bag |
| `views/silkpurse_by_type.tab` | `silkpurse_by_type` | duplicate_bag |
| `views/silkpurse_channels.tab`, `_likes.tab`, `_private.tab`, `_threads.tab` | respective views | set |

Every `view_save/0` is a full `ets:tab2file`, run every 60 s and at
shutdown. Recovery on mismatch is a full refold driven by
`view_version/0`.

### Tier 3 — operational

| File | Owner | Notes |
|---|---|---|
| `conn.json` | `conn_db` | peer address book, JS-SSB-compatible JSON, whole-file read-modify-write |
| `room_members.tab` | `room_store` | `ets:tab2file` on every mutation |
| `invites.tab` | `invite_store` | `ets:tab2file` on every mutation |

### Tier 4 — deliberately not persisted

`peer_registry`, `network_id_cache`, `heartbeat`'s peer table, **`ebt`'s
`?REPL_SET`**, `plugin_registry`, `rpc_processor`'s call table,
`ssb_feed_sup`'s registry, and `ssb_feed`'s per-feed `msg_cache`.

Not persisting EBT vector clocks is a **feature**, not an omission: it is
precisely why erlbutt does not have ssb-ebt's monotonic-clock reseed bug,
where a wiped peer can never be re-sent its own feed because the stored
clock only ever moves forward. Clocks are re-derived from the store on
each connect. Keep it that way.

`invite_store` was also unpersisted, which was unintentional — outstanding
invites died with the node. It now snapshots to `invites.tab` on every
mutation, the same pattern as `room_store`.

## 2. What this is, in database terms

We have built CQRS / event sourcing, and every piece has a standard name:

| erlbutt | Standard name |
|---|---|
| feed | partition; author is the partition key |
| `sequence` | per-partition offset |
| `ingest.journal` | the global commit log (refs only, bodies by reference) |
| `view_manager` checkpoints | consumer-group offsets |
| `view_version/0` bump | replay from earliest |
| `.gz` segments | cold segments — frozen, but never merged |
| `ets:tab2file` | full-state checkpointing, rather than incremental durability |
| `blobs/` | content-addressed object store |

The important structural fact is that there are exactly **two** kinds of
state, with opposite requirements, and both are currently stored the same
way:

1. **Truth** — signed messages and blobs. Immutable, self-verifying,
   replicated by the protocol. Its integrity guarantee comes from
   cryptography, not from a storage engine. Requirements: don't lose
   bytes, don't reorder within a feed. That is the entire contract.
2. **Derivation** — every index, graph, cache, checkpoint. Mutable,
   random-access, query-shaped, and 100% reconstructable from (1).
   Requirements: fast query, fast rebuild. Durability is an
   *optimisation*, not a correctness property.

Nearly all of the accumulated storage complexity lives at the places
where those two got mixed up.

## 3. Where the current model strains

**`profile` / `contacts` / `references` are views implemented as logs.**
They duplicate message bodies into append-only files that are never
archived and never compacted, costing roughly 2–3× write amplification on
exactly the message types we write most. They predate `view_manager` and
should become views.

Two of the three are already dead weight: `ssb_feed:fold_contacts/3` and
`ssb_feed:profile_name/1` are exported and called by **nobody** — the
lazy-load path they served died in the `view_manager` port, and
`friends:view_entry/1` now folds follows, blocks and names directly. The
`references` file is still live; `tangle.erl` reads it in four places.

An earlier draft of this note concluded that all three were "redundant
with silkpurse" and should be deleted. That conflated two questions. The
mechanism criticism stands; the layering conclusion did not follow.
*silkpurse has an equivalent* does not mean *the capability belongs to
silkpurse* — the follow graph drives EBT replication and the reference
graph induces a causal order, and both are foundational to SSB rather than
to any application built on it. §5 draws that line properly.

**There is no point-read index.** `get(msgid)` goes to `mess_auth` for
the author, then `ssb_feed:feed_get/3` linear-scans that feed's live log
from offset 0. Nothing on disk maps a msgid or a `{FeedId, Seq}` to a
byte offset; `msg_cache` is volatile and starts empty on every boot.

The missing index used to be a crash as well as a cost: `feed_get/3` only
ever opened `log.offset`, never the archived segments, so an archived
msgid returned `not_found` and badmatched `{Pos, Msg} = feed_get(...)`,
taking down a gen_server shared by every caller of that feed. Fixed in
`ssb_feed:do_fetch/3`, which now falls back to a cursor over the archived
segments and returns `not_found` instead of crashing. The linear scan
remains — that is what the `loc:` index in §4 is for.

**Snapshot-only durability is the scaling ceiling.** Every 60 s we rewrite
each view's entire table plus the checkpoint table. Cost is O(state), not
O(change). `friends_graph.tab` is 171 KB and `checkpoints.tab` 219 KB
today; at a million messages this is tens of MB rewritten per minute. And
because the state lives in ETS, every view's full state must fit in RAM,
with rebuilds that are all-or-nothing.

**No atomicity.** One `ssb_feed:store/2` touches up to five files,
several ETS tables, the ingest journal, and *other feeds' processes* (via
`utils:update_refs/1`). There is no transaction. Recovery is by idempotent
replay plus tail scanning — which mostly works (`feed_store:last_frame/1`
returning `unknown` on a torn tail, `recover_from_archives/1`) — but the
`do_archive` crash window was exactly this bill coming due.

**No fsync on the hot path.** Deliberate and correct: `open_file/1`
carries a comment explaining that `sync` cost ~60 ms per write on Linux
and throttled EBT to ~4 msg/s. The consequence is that the durability
contract is "the OS will get to it," and a power cut can tear the tail.

**No GC anywhere.** Blobs are never collected, blocked feeds are never
dropped, archives only accumulate. Archiving gzips; it does not reclaim.

**Whole-file rewrite for `conn.json`,** with no atomic rename — a crash
mid-write leaves a truncated JSON file.

## 4. A general storage model

The recommendation is *not* "move everything into a database." It is to
cut along the seam that already exists.

**Keep tier 1 as flat immutable files.** Signed append-only logs are the
right storage for signed append-only logs: ecosystem byte-compatibility,
trivial backup, archivable to blobs, parallel-foldable per feed, and no
storage engine can offer a stronger integrity guarantee than the
signature chain already provides. Putting messages into an LSM tree buys
nothing and costs interop.

**Collapse tiers 1½, 2 and 3 into one embedded store,** namespaced by key
prefix, one instance per node:

```
msg:<msgid>              -> {FeedId, Seq}       % replaces mess_auth.ets
loc:<FeedId>:<Seq>       -> {Segment, Offset}   % the missing point-read index
ckpt:<View>:<FeedId>     -> Seq                 % replaces checkpoints.tab
view:<View>:<key…>       -> row                 % ordered ⇒ range scans replace ets:match
conn:<peerid>            -> peer record         % replaces conn.json RMW
```

One atomic write batch per stored message. That buys three things at
once:

- **Atomicity for the whole derived tier.** The index can never be
  internally inconsistent again. If it disagrees with the log, the log
  wins and we replay — which is already the recovery model, just now with
  a well-defined starting point.
- **Incremental durability.** O(change) writes instead of O(state)
  snapshots every 60 s, and no RAM ceiling on view size.
- **Real point reads,** including into archived segments, via `loc:`.

**Then retire the `profile`, `contacts` and `references` files,** and let
what they were reaching for be views like everything else — see §5 for
which layer those views belong to.

`ingest.journal` stays where it is: arrival order is genuinely new
information not recoverable from the feeds (backfill only approximates
it), so it belongs in the truth tier, not the index.

## 5. Layering: foundation, conventions, applications

The store answers *where derived state lives*. It does not answer *whose
derived state it is* — and erlbutt currently has no way to express that
distinction. `ssb_view` treats every registered view identically, so
`friends` (which drives replication) and `silkpurse_channels` (which
renders a sidebar) are the same kind of thing to the system.

They are not. erlbutt is meant to be an SSB foundation that different
kinds of applications run on; silkpurse is one such application, and a
socially-shaped one — the patchwork/manyverse lineage. Some derived state
is protocol infrastructure that any application needs, and some is that
application's own concern. The line matters because it decides which
capabilities a *non-social* application inherits for free.

It turns out there are three layers, not two, because some things are
shared between applications without being protocol:

| Layer | Holds | Knows about |
|---|---|---|
| **`apps/ssb`** — foundation | protocol, replication, storage, core views | feeds, sequences, contacts, abouts — because replication needs them |
| **`apps/ssb_conv`** — conventions | tangle/thread assembly, message-type helpers | `post`, `vote`, `root`/`branch` — SSB application conventions |
| **applications** | silkpurse, admin, out-of-BEAM clients | their own UI and control surfaces |

(`apps/plumtree` sits orthogonal to all three — an alternative replication
strategy, foundation-adjacent.)

### Core views

**Core views** are always on, owned by the ssb app, part of its contract;
**app views** are registered by applications on top.

| Core view | Is | Subsumes |
|---|---|---|
| `ssb_social_graph` | follow and block edges, hops | today's `friends`, the `contacts` file |
| `ssb_feed_meta` | latest self-asserted `(feed, key, value, seq)` | the `profile` file, `friends`' name table |
| `ssb_links` | every cross-feed edge, ids interned to integers (§8) | the `references` file |

### Why these three are foundational

**The social graph drives replication.** `ebt` already asks
`friends:follows/2` and `friends:blocks/1` who to replicate and who to
refuse. That is protocol machinery, not a social feature — a sensor
network or a records store on SSB still has to decide whose feeds it
carries. The view is already correctly placed in `apps/ssb`; what is
wrong is only its name, which is a patchwork-ism sitting in the
foundation.

**Feed metadata is a mechanism, not a schema.** "Latest self-asserted
metadata per feed, last-write-wins over the log" is foundational: anything
that renders a feed id needs it, including invites, rooms and connection
UIs. The *schema* — `name`, `image`, `description` — is patchwork
convention. Storing arbitrary keys as `(feed, key, value, seq)` costs
nothing and spares a non-social application from pretending it has an
avatar. `about` becomes one registered producer rather than the definition.

**Cross-feed links are a causal order, and that is the interesting one.**
SSB gives a total order *within* a feed (the sequence chain) and no order
at all *across* feeds; timestamps are self-asserted and routinely absurd.
But a cross-feed reference is a cryptographically enforced happens-before:
if message M in feed A carries the id of message N in feed B, then A held
N when it wrote M, because you cannot reference a hash you have not seen.
Therefore

> (per-feed sequence edges) ∪ (cross-feed reference edges) = a DAG whose
> transitive closure is a causal partial order over the whole database.

That is Lamport happens-before derived from *content* rather than from
protocol bookkeeping, and it is something most P2P systems simply do not
have. It is also not what `references` is today:
`utils:update_refs/1` fires only on `is_branch`, so it captures tangle
arcs and nothing else.

The generalisation is to index every cross-feed link as
`(from_msg, from_feed, to_msg, to_feed, field, kind)`. Extraction can be
schema-agnostic — scan content for sigil-shaped values (`%…sha256`,
`@…ed25519`, `&…sha256`) and record the field path each was found at.
That catches tangles, votes, mentions, abouts, forks and blob refs without
the foundation knowing a single message type, which is exactly the
property a foundation wants. Blob refs are a different kind of edge and
earn the `kind` column rather than exclusion.

Two honest limits, so this does not get oversold:

1. **It is a partial order, and a sparse one.** Most message pairs are
   incomparable. It yields "A knew B," not a timeline. Excellent for
   deterministic thread ordering, fork detection, and rejecting
   impossible timestamps; useless as a global sort.
2. **It is not arrival order.** `ingest.journal` is a *total* order that
   is *local* and meaningless to any other peer; the causal order is
   *partial*, *global* and trustworthy. Different jobs — naming both
   keeps them from being conflated.

### The convention layer

`ssb_links` is deliberately only the *edge set* — "M references N, in
field F." Reading those edges as a **conversation** is a different thing:
root/branch semantics, thread assembly, fork handling, depth ordering.
That is one interpretation of the edges, specific to the SSB thread
convention, and it is not protocol.

`tangle.erl` is that interpretation, and it currently lives in `apps/ssb`
— the same misplacement as `references`, mirrored. The edges belong down;
the reading belongs up. What forces the issue is that **two** applications
need thread assembly: silkpurse and maxbutt (an Emacs client, see below).
Two consumers means it must not be duplicated in either, but it does not
promote it to protocol. It means there is a middle.

`social_msg.erl` is already in that middle, filed under the foundation,
and the split runs *through* it rather than around it: `contact` and
`about` are de-facto protocol — `ebt` cannot replicate without the follow
graph — while `post`, `vote` and `root`/`branch` are application
convention. Only the first half has to stay in `apps/ssb`.

The convention layer is pure functions, no processes. It earns being its
own lib app precisely because the alternative — moving `tangle.erl` into
`apps/silkpurse` — would strand maxbutt.

### Applications, and an admin app

silkpurse's views stop being parallel implementations and become thin
adapters: `silkpurse_about` wraps `ssb_feed_meta` with patchwork's
socialValue semantics, and `silkpurse_backlinks` becomes a **stateless**
plugin — no view, no table, no process — that queries `ssb_links` and
renders the answers in the shape the JS client expects.

Not every app view collapses, and it is worth being precise about which
do. An earlier draft of this section claimed `silkpurse_threads` would
too. It does not: its table is keyed by thread root and holds an
*aggregate* — reply count, recent replies with timestamps, last-activity
time, participants, mentions, channel. `ssb_links` supplies the reply
*set* and nothing else, so the rollup would have to be recomputed by
fetching every reply of every thread on each query. The test for whether
an app view dissolves into `ssb_links` is not "does it involve links" but
"is its table an edge set": `silkpurse_backlinks`' was, and
`silkpurse_threads`' is not.

erlbutt already has three clients, and only one of them bypasses the
protocol. `sbutt.escript` (`whoami`, `publish`, `get`, `invite create`,
`log`, `hist`) and silkpurse both speak muxrpc. **maxbutt** — the Emacs
client, both a text-only reader and an ad-hoc admin console — instead
calls internal modules directly over Erlang distribution, includes
`../erlbutt/apps/ssb/include/ssb.hrl` by relative path, and has already
paid for it: `maxbutt:log/0` reads `<repo>/log.offset`, the global log
retired when `ingest_journal` landed, and has silently returned nothing
ever since.

So the admin question is not "should there be an admin app" but "should
control-plane operations be RPC methods like everything else?" — and yes,
because then every client gets them rather than only the Emacs one. That
gives **`apps/admin`**: an owner-only RPC namespace holding what is today
either an exported one-off or a remsh incantation.

- dialer on/off/status (`peer_dialer`, the one that exists today)
- peers: list, connect, forget, connection status
- views: rebuild one, list with checkpoints and versions
- feeds: archive now, last sequence, counts
- config: `replication_hops`, `archive_length`, network ids
- invites: create, list, revoke
- blob store: counts and size

The line to hold: this is the **control plane**, not BEAM introspection.
Process inspection, memory, code reload and tracing genuinely need
same-node access, that is what distel and `remsh` already provide, and
they should not go behind muxrpc. No app required — that is the shell.

Every method in this namespace is `owner`. It is the first surface where a
wrong permission class is a security problem rather than a bug: `rebuild`
from an arbitrary peer is a free denial of service at roughly 28 s of
blocked `view_manager` per call (§8). The `plugin_registry` lattice
already enforces this; the discipline is simply that nothing here is ever
`peer`.

maxbutt then becomes a client of that namespace rather than a caller of
`peer_dialer:enable/0` — it stops including erlbutt's header, stops
knowing module names, and the next `log/0`-style breakage fails loudly at
the RPC boundary instead of silently returning an empty list.

### What this does to the engine argument

The three core views are precisely relational shapes: a graph edge set, a
key-value-per-feed table, and an edge list queried by reachability. So
§6's case gets stronger. `tangle.erl`'s `parents`/`ancestors`/`find_paths`
walk — recursive gen_server calls, one round trip per hop — becomes a
single `WITH RECURSIVE` CTE over `ssb_links`. Reachability over a causal
DAG is what recursive CTEs exist for.

## 6. Engine choice

The requirement, stated plainly: an embedded store with ordered keys,
atomic batches, range/prefix iteration, bounded memory, and no separate
server process.

| Option | Verdict |
|---|---|
| **SQLite** (`esqlite`) | Recommended — see below |
| **RocksDB** (`erlang-rocksdb`) | Right shape if pure KV + prefix scan is all we need; column families map cleanly onto the prefixes above. Heavier build (C++), and we'd hand-roll every query. |
| **LMDB** (`elmdb`) | Simpler than RocksDB, superb reads, single-writer MVCC, no compaction. Map size must be fixed up front, which is awkward for an ever-growing index. |
| **Mnesia** | The no-NIF option. `disc_copies` keeps the RAM ceiling we're trying to escape; `disc_only_copies` is DETS, with its 2 GB limit and slow repair. Gets us transactions cheaply, and little else. |
| **Bitcask** | Architecturally the closest relative we have — and rejected, for reasons worth writing down. See below. |
| **CouchDB-style document store** | The closest philosophical match — append-only, MVCC, map/reduce views, replication built in; it is essentially the same architecture as flume. But nothing embeddable exists for the BEAM, and running a server alongside the node defeats the point. |

### Why SQLite, and what the Erlang integration looks like

Look at what the views actually do. `backlinks`, `by_type`, `threads`,
`likes`, `about` are all "select rows where X, order by Y, limit N" —
hand-rolled as ETS bag folds, one view at a time, ~200 lines each. That
is a relational workload being served by a hash table. `silkpurse_by_type`
already hit a quadratic `file2tab` restore that a real index would never
have produced. Silkpurse ships SQLite in Electron for search, so the
dependency is already conceptually committed.

SQLite gives us: one file, real transactions, secondary indexes we
declare rather than fold, FTS5 for the search path, `ORDER BY … LIMIT`
without materialising the world, and views that are ~20 lines of SQL.

On the OTP integration — the ecosystem is in decent shape, with two
serious NIF bindings:

**[`esqlite`](https://github.com/mmzeeman/esqlite)** (mmzeeman) is the
established choice. It is a NIF that embeds the SQLite amalgamation
(3.45.2 as of v0.9.0, or use the system library via `ESQLITE_USE_SYSTEM`),
compiled with FTS3/4/5, JSON1 and RTREE enabled. It runs all blocking
work on dirty schedulers, which removed the old thread-per-connection
design and makes many concurrent connections practical. ~28k recent
downloads on Hex, v0.9.0 published within the last year, 335 commits and
live CI. Note one gotcha: it compiles with `SQLITE_DQS=0`, so double-quoted
string literals are a syntax error — single quotes only.

**[`sqlite`](https://hex.pm/packages/sqlite)** (max-au) is the other
credible option — BSD-3, v2.0.0, explicitly designed so that tens of
thousands of connections can live in one node without extra OS threads,
with dirty-scheduler variants of the blocking calls
(`sqlite_dirty_close_nif` and friends). Cleaner OTP idioms, but last
published February 2023, so it is the more conservative bet only if its
API suits us better.

The known failure mode is worth designing around from the start: the
[dirty IO scheduler pool defaults to 10 threads](https://www.erlang.org/doc/apps/erts/erl_cmd.html),
and *every* SQLite call queues through it. Under high-concurrency reads
that pool serialises,
[negating the concurrency SQLite itself would allow](https://erlangforums.com/t/high-concurrency-sqlite-reads-with-esqlite3-nif-performance-bottleneck/5012).

Note this is not a quirk of esqlite: since OTP 21 `file:pread` runs on the
same dirty schedulers, so *any* embedded store pays into the same pool.
What differs is the **granularity of occupancy**, and that is where SQLite
is genuinely exposed. A plain `file:pread` holds a slot for one syscall; a
single `esqlite3:q/step` holds one for the whole statement — parse, plan,
B-tree descent, every page read, any sort or join. The practical ceiling
is therefore ~10 concurrently *executing* statements node-wide, no matter
how many connections are open, and SQLite's own WAL-mode reader
concurrency cannot be expressed through it.

Three mitigations, in order of leverage. Keep statement occupancy short —
indexed, `LIMIT`ed queries, never a full scan on the request path. Batch
writes: buffer in Erlang and commit one transaction per ingest batch
rather than per message. Keep a hot-read ETS cache (`mess_auth`-style) in
front of the point-read path so common lookups never reach the NIF at all.
Raising `+SDio` (32–64) is a legitimate fourth knob rather than a fig leaf
— the ERTS docs note the dirty IO count is deliberately not tied to the
normal scheduler count, since only I/O-bound work runs there — but it
cannot rescue long-running statements.

The pattern that follows naturally: **ETS stays as the read cache, SQLite
becomes the durability layer** — which inverts today's arrangement, where
ETS is the store and the file is a 60-second snapshot of it.

### Bitcask, and why not

Bitcask deserves more than a table row, because it is the closest thing
in the Erlang world to what erlbutt has already half-built, and looking at
it sharpens the §4 design whether or not we adopt it.

Bitcask is: append-only data files, plus an in-memory **keydir** mapping
every key to `{file_id, offset, size}`, plus **hint files** written beside
each data file so the keydir can be rebuilt at startup without reading
values. The correspondence is uncanny:

| Bitcask | erlbutt |
|---|---|
| append-only data files | `log.offset` + `.gz` segments |
| keydir | `msg_cache`, `mess_auth`, and the `loc:` index of §4 |
| hint files | **nothing** |

We have independently arrived at most of bitcask's architecture. The
missing third is the interesting part: hint files are exactly why
`msg_cache` starts empty on every boot and why `mess_auth:rebuild/0` has
to brute-force a fold over every feed.

**The decisive objection is that bitcask owns its file format.** It cannot
be pointed at `log.offset` and told to index it. Adopting it means copying
every message into bitcask's own data files — duplicating the entire log.
That is precisely the write amplification §3 convicts `profile`,
`contacts` and `references` of, at 100% rather than 30%. For a store whose
ground truth must stay in SSB format for interop, that alone disqualifies
it.

The data model rules it out for the view tier independently:

- **No ordered keys and [no range queries](https://tech-lessons.in/en/blog/bitcask/)**
  — keys are not stored in order, so the API is `bitcask:fold/3` over
  everything and nothing else. The `view:<View>:<key…>` prefix-iteration
  design of §4 dies immediately, and every view query stays a full fold,
  which is *worse* than the ETS bags we have now.
- **Strict key-to-one-value.** `backlinks` is a bag and `by_type` a
  duplicate_bag; we would read-modify-write a whole list per key, i.e.
  O(fan-out) per append. Bad exactly where it hurts, on a popular
  message's backlinks.
- **Keys must fit in RAM** — [the sole limitation Riak's own documentation lists](https://docs.riak.com/riak/kv/2.2.3/setup/planning/backend/bitcask/index.html).
  That is the ceiling §3 criticises ETS for; bitcask formalises it rather
  than removing it. At roughly 40–50 bytes of overhead plus a ~53-byte
  msgid, a million messages is ~100 MB of keydir and ten million ~1 GB —
  survivable at SSB scale, but not an improvement.

There is an irony in the merge story too. Compaction exists to reclaim
space from overwritten keys; our truth tier never overwrites, so merge is
pure overhead there — while the view tier that genuinely needs GC is the
one whose shape bitcask cannot represent.

On provenance: `basho/bitcask` is dead with Basho, and the forks of it
scattered around GitHub mostly stop in 2013–2014. The live line is
[OpenRiak/bitcask](https://github.com/OpenRiak/bitcask), maintained in
conjunction with the Erlang Ecosystem Foundation —
[OpenRiak 3.4 targets OTP 26, 3.6 targets OTP 28, and the openriak-4.0 branch trials OTP 28.3](https://github.com/orgs/OpenRiak/discussions/19).
So it is alive, but alive as a Riak component rather than as a
general-purpose library.

OpenRiak's advice to keep `bitcask.io_mode = erlang` rather than `nif` is
worth understanding but does *not* mean what it once did. The knob selects
how bitcask reads and writes its data files — through `file:pread` or
through its own C NIF — and it never covered the keydir, which is always a
NIF. The advice dates from the era when `file` was a linked-in port driver
using the async thread pool, so "Erlang I/O" and "NIF I/O" were genuinely
different scheduling worlds and a third-party NIF doing a blocking read on
a normal scheduler could stall the VM. That distinction largely went away
in [OTP 21, which rewrote the efile driver as NIFs on dirty schedulers](https://www.erlang.org/downloads/21):
on OTP 28 `file:pread` is itself a dirty-scheduler NIF. What `io_mode =
erlang` buys today is OTP's own well-tested file I/O rather than a
third-party C implementation — a correctness argument, not an escape from
the dirty pool.

**What to take from it: hint files.** Whatever the index tier lands on,
persist the id-to-location map beside each archived segment so it loads
without re-reading the segment. That kills the cold-start fold §3 flags,
and it is a few dozen lines rather than a dependency.

Bitcask's existence mildly *strengthens* the SQLite recommendation: SQLite
serves the `loc:` index perfectly well and gives us ordered iteration on
top, which bitcask denies outright. Two engines for one problem would be
the worse outcome.

## 7. On immutability

"Information only ever increases" is doing real architectural work here,
and it is worth being precise about where it holds.

- **True at the message level.** This is why we never need a transaction
  spanning the log and the index — only idempotent replay, plus atomicity
  *within* an index batch. Checkpoints alone are a complete recovery
  story, and any index can be rebuilt per-feed in parallel.
- **False at the view level.** "Current display name," follow/unfollow,
  likes — these are last-write-wins registers over a monotone log, i.e.
  projections that overwrite. Views do need updates, and the index tier
  does accumulate compaction pressure. Far less than a general-purpose
  store, but not zero.
- **Leaks entirely** at blob GC, blocked-feed dropping, and legal erasure
  — the same wall the EHR-export sketch ran into. Better to keep that
  seam explicit than to assume append-only all the way down.

There is a pleasing extension of something we already do: `do_archive`
writes each frozen segment into the blob store. Push that further and the
truth tier becomes purely content-addressed segments, with the local
store as a cache plus an index over them. At that point a
sneakerweb-style drop bundle is nearly free, because the export format
and the storage format are the same thing.

## 8. Concrete next steps

Ordered by cost/benefit, smallest first:

1. ~~Fix the archived-msgid crash in `ssb_feed`~~ — **done**, July 2026.
   `fetch_msg/2` now falls back to the archived segments and returns
   `not_found` on a miss; callers that assumed a `#message{}` were
   updated, and `do_archive/1` clears `msg_cache` so no stale live-log
   offset survives the log being replaced.
2. ~~Persist `invite_store`~~ — **done**, July 2026.
3. ~~Write `conn.json` via write-temp-then-rename~~ — **done**, July
   2026. It mattered more than "no atomic rename" suggested: `load/1`
   treats undecodable JSON as an empty map, so a torn write did not fail
   loudly — it silently forgot every peer the node knew how to reach.
4. ~~**Hint files** (the bitcask borrow, §6)~~ — **done**, July 2026.
   `do_archive/1` writes `log.offset.<From>-<To>.hint` beside each
   segment, listing `{MsgId, Seq, Offset, Len}` against the uncompressed
   frames, so a lookup decompresses only the segment whose hint names the
   id. Missing hints are built on demand, so an existing store heals one
   segment at a time.
5. ~~Add the `loc:` index~~ — **done in part**, July 2026, and the part
   that was dropped is worth recording. The live-log half landed:
   `ssb_feed`'s `msg_cache` went from a read-through cache in front of a
   linear scan to a real index — populated on write from the offset
   `write_msg/2` now returns, completed in one pass on the first miss,
   and verified on read so a stale offset is detected rather than
   answered. `feed_get/3` and its scan are gone.
   The *durable cross-feed* table was NOT built, for two reasons. Its
   archived half is redundant now that item 4 exists — hints already
   answer that, at a fraction of the memory. And it is not naturally an
   `ssb_view`: `view_entry/1` receives a decoded message and never sees
   the byte offset it was written at, so making it one would mean bending
   the view contract for a single consumer. It belongs in item 11, in the
   store, not in ETS.
6. ~~**Drop the `profile` and `contacts` files.**~~ — **done**, July 2026.
   Both were already readerless (§3); pure deletion of write
   amplification.
7. ~~**Introduce the core/app view split**~~ — **done**, July 2026. Teach
   `ssb_view` (or `view_manager`) to mark a view as core, so the ssb app
   always registers its own and applications register on top. Then rename
   `friends` to `ssb_social_graph` and split its name table out as
   `ssb_feed_meta` keyed `(feed, key, value, seq)`. `view_class/0` is an
   optional callback defaulting to `app`, so no existing view changed.
8. ~~**Create `apps/ssb_conv`, the convention layer**~~ — **done**, July
   2026. Moved
   `tangle.erl` there, and split `social_msg.erl`: the `contact`/`about`
   half stays in `apps/ssb` because replication needs it, the
   `post`/`vote`/`root`/`branch` half moves up. Pure functions, no
   supervision tree. `ssb_layering_tests` enforces the dependency
   direction, since Erlang links a call from the foundation up into the
   convention layer without complaint.
9. ~~**Create `apps/admin`**~~ — **done**, July 2026 (except the maxbutt
   port, deferred by choice): an owner-only RPC namespace, starting
   with the dialer toggle that already exists plus whatever is currently
   reached for via remsh. Then port maxbutt onto it, dropping its
   relative-path include of `ssb.hrl` and its direct calls into
   `peer_dialer`, `friends`, `config` and `ssb_feed`. Fix `maxbutt:log/0`
   in the same pass — it still reads the retired global `log.offset` and
   returns nothing. Every method `owner`; nothing in this namespace is
   ever `peer`. **maxbutt is still unported** and still calls
   `friends:name/1` and `ssb_social_graph:direct_follows/1` by their old
   names; `maxbutt:log/0` still reads the retired global `log.offset`.
10. ~~**Generalise `references` into `ssb_links`**~~ — **done**, July
   2026, in four steps, because it was several commits' worth:
   - **10a** `view_manager` catch-up chunked and moved out of the
     registration call (the prerequisite below). A catching-up view takes
     no ingests — delivering one would advance its checkpoint past
     everything the fold had not reached — and the fold sweeps until a
     whole pass delivers nothing.
   - **10b** the `ssb_links` core view: schema-agnostic extraction, ids
     interned to integers, incoming edges only (outgoing are derivable
     from the message, and `links_of/1` is exported for that).
   - **10c** `tangle` ported onto it — a step of the walk is now the
     intersection of two `ssb_links` queries and reads no message bodies.
     `references`, `utils:update_refs/1`, `ssb_feed:store_ref/2` and
     `references/3` retired; `is_branch/1` finally moved to
     `ssb_conv_msg`. `converter:build_refs/1` went with them: it
     `-import`ed the deleted function, which Erlang does not check, so it
     compiled clean and would have failed at runtime.
   - **10d** `silkpurse_backlinks` reduced to a stateless plugin over
     `ssb_links`. `silkpurse_threads` deliberately NOT repointed — see §5.
11. ~~Introduce the store behind `ssb_view`~~ — **done**, July 2026, in
   three steps:
   - **11a** `{esqlite, "0.9.0"}` plus `ssb_store`. Reads run in the
     CALLER's process on a shared connection (verified safe for 20
     concurrent readers), so a store-backed view read stays a function
     call rather than a gen_server round-trip; writes go through the
     server. Measured 509k rows/s batched through the NIF — essentially
     the raw SQLite rate, confirming §6's claim that batching amortises
     the dirty-scheduler cost.
   - **11b** `ssb_social_graph` ported. `reverse_edges/1` went from an
     `ets:foldl` over both whole graphs to an indexed lookup, and
     `follows/2` from a hand-rolled BFS with a visited set to one
     `WITH RECURSIVE` CTE — the payoff §6 predicted, arriving a view
     earlier than expected.
   - **11c** `ssb_links` ported. Interning is now SQLite's own rowid
     (`num INTEGER PRIMARY KEY` plus `RETURNING`), so no counter is kept
     anywhere; the edge table's primary key `(to_id, from_id, field)`
     both deduplicates and indexes the only query shape there is.

   Still on ETS with `tab2file`: `ssb_feed_meta` and the six silkpurse
   views. They work, and the two ported views were the ones with a
   relational shape worth the move — `mess_auth` and `view_manager`'s
   checkpoints are the more interesting remaining candidates, since both
   are O(state) snapshots on a timer.

### What an `ssb_links` rebuild actually costs

Measured July 2026 against `~/.silkpurse/flume/log.offset` — the corpus
erlbutt would replicate, and framing-compatible with our own store. (Our
own feed store is empty at the time of writing, so this is a
"when-you-get-there" number, not a current one.)

| Corpus | |
|---|---|
| messages | 2,482,208 |
| distinct feeds | 43,683 |
| payload | 1.79 GB |
| content links | 3,672,528 (avg 1.48/msg) — 2.06M msg→msg, 1.36M msg→feed, 251k msg→blob |
| messages with no links | 390,211 (15.7%) |

| Rebuild phase | Rate | At 2.48M messages |
|---|---|---|
| frame read + `message:decode/2` | 248k msg/s | ~10 s |
| link extraction, raw record bytes | 306k msg/s | ~8 s |
| SQLite insert, 3.67M rows, one txn | 570k rows/s | 6.4 s |
| two indexes (`to_ref`, `from_msg`) | — | 3.1 s |
| **total** | | **~28 s** |

Backlinks lookups against the result: ~3 µs. Caveats in both directions —
the measurement had a warm page cache and esqlite is slower than raw
SQLite, but the fold is per-feed across 43k feeds and parallelises, which
the measurement does not.

**The cost worth worrying about is disk, not time.** `ssb_links` lands at
656 MB unindexed and **1.33 GB with both indexes** — most of the size of
the log it derives from, for a single view. The cause is storing 53-byte
ids as TEXT three times per row; ~584 MB of the table is repeated
identifiers.

**So intern the ids.** Map message and feed ids to integer rowids and
store edges as integers: ~88 MB of edges plus ~130 MB of intern table,
call it ~220 MB before indexes — four to five times smaller. The
convergence is the pleasing part: the msgid intern table *is* the `msg:`
index of §4. We need `msgid -> {feed, seq}` for point reads anyway, so
interning buys the point-read index rather than storing ids twice in two
different places. Treat it as a precondition of item 8, not an
optimisation to retrofit.

**One prerequisite in `view_manager`.** The 28 seconds are not the risk;
where they are spent is. `register_view/1` is a
`gen_server:call(?SERVER, ..., infinity)` and `catch_up/1` runs *inside*
it, so the manager is blocked for the whole fold — and because
`ssb_feed:store/2` calls `view_manager:ingest/1` synchronously,
replication stalls with it. At an empty store that is invisible; at 2.5M
messages it is a half-minute freeze at boot with every other view's
registration queued behind it. Catch-up should be chunked or run outside
the registration call before `ssb_links` becomes always-on. This is worth
doing on its own account: `silkpurse_backlinks` hits the identical wall on
the identical corpus.
