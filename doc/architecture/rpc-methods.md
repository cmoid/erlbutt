muxrpc methods
==============

Every method the node answers, grouped by who implements it. The access
column is the method's permission: `anyone` who completed the handshake,
a room `member`, `room` (open on an open room, members only otherwise),
or the node's `owner` only. How a caller's class is decided is in
[networking](networking.md#a-connection-end-to-end).

The authoritative list is what the node itself serves: `manifest`
returns the methods visible to the caller's class, so an owner client
sees everything below and a stranger sees only the `anyone` rows.

Foundation (`apps/ssb`)
-----------------------

Implemented in `rpc_processor` itself and always present.

| Method | Kind | Access | Purpose |
|---|---|---|---|
| `manifest` | sync | anyone | the methods this caller may use |
| `whoami` | sync | anyone | the node's feed id |
| `get` | async | anyone | one message by key |
| `createHistoryStream` | source | anyone | one feed from a sequence (legacy replication) |
| `createUserStream` | source | owner | one feed, client form |
| `createLogStream` | source | owner | every message, in arrival order |
| `createFeedStream` | source | owner | every message |
| `publish` | async | owner | sign and append a message to the node's own feed |
| `gossip.ping` | duplex | anyone | keepalive |
| `ebt.replicate` | duplex | anyone | EBT replication |
| `blobs.has` | async | anyone | which of these blobs do you hold |
| `blobs.get` | source | anyone | a blob's bytes |
| `blobs.createWants` | source | anyone | the blob want/have stream |
| `blobs.want` | async | owner | fetch a blob from the network |
| `blobs.add` | sink | owner | store a blob (stream the bytes, end with `true`) |
| `blobs.push` | async | owner | offer a blob to peers |
| `archives.boundaries` | source | anyone | every archive boundary this node knows (extension†) |
| `glimpses.offers` | source | anyone | every glimpse this node holds (extension†) |
| `invite.create` | async | owner | mint a pub invite |
| `invite.use` | async | anyone | redeem an invite |
| `tunnel.isRoom` | sync | anyone | whether this node is a room |
| `tunnel.connect` | duplex | room | open a tunnel to another attendant |
| `tunnel.endpoints` | source | room | attendants (legacy rooms form) |
| `room.metadata` | async | anyone | room name and privacy mode |
| `room.attendants` | source | room | who is connected, then join/leave events |

† A protocol extension, not part of core SSB: see the
[Extensions section](https://cmoid.github.io/scuttlebutt-protocol-guide/#extensions)
of the protocol guide. Peers that do not implement these answer them as
unknown methods, and erlbutt treats that as "nothing to offer".

Admin (`apps/admin`)
--------------------

The node's control plane. All owner only.

| Method | Purpose |
|---|---|
| `admin.status` | identity, uptime, feed and view counts, dialer, hops, room, signature checks |
| `admin.dialer.status` / `.enable` / `.disable` / `.trigger` | the peer dialer; enable and disable persist |
| `admin.views.list` | every view: class, version, feeds folded, caught up |
| `admin.views.rebuild` | reset one view and refold it |
| `admin.peers.known` / `.connected` | the address book; live connections |
| `admin.store.tables` | tables in `store.db` and their row counts |
| `admin.export` / `admin.import` | [feed bundles](../guide/backup-export-import.md) |

Client surface (`apps/silkpurse`)
---------------------------------

What a Patchwork-family UI needs: silkpurse and HighWire use these. All
owner only — they are for the node's own UI, not for peers. Some are
general SSB client methods any client expects; most are
Patchwork-specific.

**General client methods**

| Method | Kind |
|---|---|
| `messagesByType` | source |
| `getLatest`, `latestSequence` | async |
| `friends.get` | async |
| `blobs.ls` | source |
| `backlinks.read` | source |
| `about.socialValue` | async |
| `about.socialValueStream`, `about.socialValuesStream`, `about.latestValueStream` | source |
| `conn.peers`, `conn.stagedPeers` | source |
| `conn.connect`, `conn.remember`, `conn.forget` | async |
| `replicate.request` | sync |
| `archives.history`, `archives.fetch` | async |
| `glimpses.get`, `.edge`, `.publish`, `.promote`, `.demote` | async |

**Patchwork**

| Method | Kind |
|---|---|
| `patchwork.<feed>.roots`, `patchwork.<feed>.latest` for `publicFeed`, `networkFeed`, `participatingFeed`, `mentionsFeed`, `profile`, `channelFeed` | source |
| `patchwork.recentFeeds` | source |
| `patchwork.privateFeed.roots`, `.latest` | source |
| `patchwork.thread.sorted` | source |
| `patchwork.backlinks.referencesStream`, `.forksStream` | source |
| `patchwork.liveBacklinks.stream` | source |
| `patchwork.liveBacklinks.subscribe`, `.unsubscribe` | async |
| `patchwork.contacts.stateStream`, `.ignoreStream` | source |
| `patchwork.contacts.isFollowing`, `.isBlocking` | async |
| `patchwork.likes.get` | async |
| `patchwork.likes.countStream`, `.feedLikesMsgStream` | source |
| `patchwork.channels.get`, `.suggest` | async |
| `patchwork.channels.stream`, `.recentStream`, `patchwork.subscriptions` | source |
| `patchwork.profile.avatar`, `patchwork.suggest.profile` | async |
| `patchwork.progress`, `patchwork.heartbeat` | source |
| `patchwork.disconnect` | async |

A few Patchwork methods (`channels.get`, `channels.stream`,
`liveBacklinks.subscribe`/`unsubscribe`, `progress`, `disconnect`) are
stubs that answer empty or `true`, so the UI works; the rest are real.

Plugins
-------

Plugins add their own namespaces. ssb_git, for example, adds `git.repos`,
`git.refs`, `git.packs`, `git.updates`, `git.issues` and `git.issue`,
all owner only. Writing your own is covered in [plugins.md](plugins.md).
