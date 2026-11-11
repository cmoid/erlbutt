Storage
=======

Everything a node keeps lives in one data directory, `.ssberl`. Where
that directory is depends on how the node was built and started — see
[configuration](../guide/configuration.md#where-the-data-lives). This
page is about what is inside it and why.

The split described in the [overview](overview.md#two-kinds-of-state)
runs through the directory: some files are **truth** and can never be
recreated, the rest are **derived** and can be thrown away and rebuilt.

```
.ssberl/
  secret              the identity keypair                       truth
  feeds/              one directory per feed                     truth
  blobs/              content-addressed blob files               truth
  ingest.journal      arrival order of every stored message      truth (local)
  own_heads           highest sequence ever published, per key   truth (local)
  data_version        on-disk format version                     marker
  store.db            SQLite: views, indexes, checkpoints        derived
                      (and validation floors)                    (floors: local truth)
  conn.json           known peer addresses                       operational
  invites.tab         outstanding pub invites                    operational
  room_members.tab    room membership                            operational
  ssb.cfg             the operator's settings (optional)         config
  overrides.cfg       settings changed at runtime                config
  plugins/            operator-installed plugins (optional)      code
  LOCK                held by the running node                   runtime
```

Back up `secret`, `feeds/` and `blobs/`. Everything else is either
rebuilt from those or is local bookkeeping you can live without (the
ingest journal and the operational tables are worth keeping, but losing
them loses order and convenience, not data).

The truth tier
--------------

### Feeds

Each feed directory holds its own history, sharded by the first two
characters of the encoded key:

    feeds/<2>/<rest>/log.offset                  live messages, framed
    feeds/<2>/<rest>/log.offset.<From>-<To>.gz   frozen (archived) segments
    feeds/<2>/<rest>/log.offset.<From>-<To>.hint {MsgId, Seq, Offset, Len}

Records are framed `<<Len:32, Msg:Len/binary, Len:32, NextOffset:32>>`,
the message bytes stored as the signed JSON. The live log and the
segments together are the feed's full history in sequence order
(`feed_store` reads them; `utils:fold_log_file` understands the framing).

A feed is owned by one `ssb_feed` process, started on demand under
`ssb_feed_sup` and found through a named ETS registry. The process
validates and appends (signature, `previous`, sequence), and keeps an
in-memory index from message id to byte offset, filled as messages are
written and completed in one pass the first time the feed is read. Every
read checks that the record it lands on is the one it expected, so an
index made stale by something rewriting the log underneath it is
detected rather than believed.

A feed's log can be **archived**: the live log is gzipped into a frozen
segment, stored as a blob, and announced in an `archive` message on the
owner's own feed, so the archive is part of the replicated record. The
`.hint` file beside each segment lists what is inside and where — a
borrowing from bitcask — so a lookup reads small sidecars and decompresses
only the segment that holds the id. See
[feeds, archives and glimpses](feeds-archives-glimpses.md).

### Blobs

`blobs/` is a content-addressed store of opaque bytes, one file per blob,
at `blobs/<2>/<rest>` of the decoded hash. A blob is named by the sha256
of its contents, so whatever stores it is verified on the way in.

### The ingest journal

`ingest.journal` records a `{FeedId, Seq}` reference for every stored
message, in the order messages arrived. That is genuinely new
information — arrival order is local and cannot be recovered from the
feeds — and it is what `createLogStream` serves. It holds references,
not bodies; bodies are read from the feeds. It replaced an older global
`log.offset` that duplicated every message.

### Two local records that are not derived

- `own_heads` remembers the highest sequence this node has published per
  identity. It is half of the fork guard (see
  [feeds, archives and glimpses](feeds-archives-glimpses.md#publishing-safely-the-fork-guard)).
- **Validation floors** (`feed_floor`, a table in `store.db`) record that
  we chose to hold a feed only from some sequence onward. A floor is a
  local decision nothing in the logs implies, so it is not a view and a
  rebuild does not touch it.

The store: `store.db`
---------------------

Derived state lives in a single SQLite database through `esqlite`, a NIF
that bundles the SQLite amalgamation, so there is no system dependency.
`ssb_store` owns it.

- **Reads run in the calling process** on a shared connection, so a view
  query is a plain function call rather than a round trip through a
  server.
- **Writes go through one server**, which matches SQLite's single-writer
  model and gives one place to batch. Every NIF call occupies a dirty IO
  scheduler, and that pool is small, so batching rows into one
  transaction is the difference between hundreds of thousands of rows per
  second and very few.
- **Ids are interned** to integers (an upsert with `RETURNING` on
  SQLite's own rowid), so 53-byte identifiers are not stored several
  times per row.
- **Schemas are versioned per view.** A view declares its tables with a
  version; when the version changes the tables are dropped and the view
  rebuilt. See [views.md](views.md).

What lives here: the core views (follow graph, feed metadata, cross-feed
links, archive boundaries, glimpses), the message-id → author index
(`mess_auth`), view checkpoints, validation floors, and every app view
(silkpurse's threads, likes, channels; ssb_git's repositories). Queries
become queries: "who follows this person" is an indexed lookup, and
transitive follows out to N hops is one recursive CTE.

esqlite is compiled with `SQLITE_DQS=0`: string literals take single
quotes; double quotes are identifiers only.

What this buys
--------------

Because the truth tier is immutable and everything else is derived, any
view can be rebuilt from the logs at any time (`admin.views.rebuild`),
and a schema change is a version bump, not a migration.

One caveat: `store.db` is mostly, not entirely, derived. The validation
floors live in it too, so deleting the whole file with the node stopped
rebuilds every view but forgets which feeds were held from a floor.
Prefer rebuilding views to deleting the database.

`data_version` guards the part that cannot be rebuilt: it refuses to start
a node against a data directory written by a newer build, because a
rollback that silently misread the logs, segments or floors would cost
far more than a refusal.

Further reading
---------------

- [research/persistence.md](../research/persistence.md) — the survey and
  measurements behind this design, and how it compares to flumedb and
  ssb-db2.
- [Rebuilding a node](../guide/rebuilding-a-node.md) — what to do, and not
  do, when the data has to be recreated.
