erlbutt documentation
=====================

Running erlbutt
---------------

For anyone running a node, on a laptop or as a pub. No Erlang needed.

- [Getting started](guide/getting-started.md) — what erlbutt is,
  installing a release, your identity, connecting a client, meeting the
  network.
- [Configuration](guide/configuration.md) — every setting, where settings
  live, environment variables, where the data is.
- [The sbutt command](guide/sbutt.md) — the command line tool.
- [Moving from SSB](guide/moving-from-ssb.md) — bringing an existing feed
  over from a JavaScript client (Patchwork, ssb-server).
- [Running a pub](guide/running-a-pub.md) — installing erlbutt as a
  service on a VPS, upgrades, rollback, backups.
- [Backup, export and import](guide/backup-export-import.md) — feed
  bundles: moving feeds between nodes, keeping a portable copy.
- [Rebuilding a node](guide/rebuilding-a-node.md) — recreating a node's
  data from scratch without forking your own feed.

Working on erlbutt
------------------

For programmers: how it works, and how to change it.

**Architecture** — what the code does now.

- [Overview](architecture/overview.md) — the state model, the three
  layers, the process structure. Start here.
- [Storage](architecture/storage.md) — per-feed logs, archive segments,
  the ingest journal, blobs, `store.db`.
- [Views](architecture/views.md) — the view manager, core and app views,
  checkpoints, rebuilds.
- [Networking](architecture/networking.md) — handshake, box stream,
  muxrpc, permission classes, dialing, invites, rooms.
- [Replication](architecture/replication.md) — the replication set, EBT,
  blobs, plumtree.
- [Feeds, archives and glimpses](architecture/feeds-archives-glimpses.md)
  — the fork guard, archive boundaries and floors, glimpses and pins.
- [Plugins](architecture/plugins.md) — writing methods and views,
  packaging and installing a plugin.
- [muxrpc methods](architecture/rpc-methods.md) — the full RPC surface.

**Development** — building, testing, releasing.

- [Building and testing](development/building-and-testing.md)
- [Releasing](development/releasing.md)
- [Testing archives](development/testing-archives.md) and
  [testing glimpses](development/testing-glimpses.md) — two-node
  procedures for watching those features work.

**Protocol** — the wire.

- [Scuttlebutt protocol guide](https://cmoid.github.io/scuttlebutt-protocol-guide/)
  — the protocol erlbutt implements, maintained by this project. Its
  [Extensions section](https://cmoid.github.io/scuttlebutt-protocol-guide/#extensions)
  specifies erlbutt's two additions, partial feeds and feed glimpses.
  These are proposals, not yet part of the core protocol.

**Research** — why it is built this way.

- [Design notes](research/README.md) — persistence, plugins, archive
  boundaries, glimpses, git over SSB; plus papers and links.

Dev diary
---------

[dev-diary/](dev-diary/) — the project's history as it happened, posted
to SSB as it was written. Links to SSB messages in it resolve only from
an SSB client.
