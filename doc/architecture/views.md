Views
=====

A view folds stored messages into queryable state: a follow graph, a
name index, the replies to a thread. Views are the derived half of the
[storage model](storage.md). This page covers the machinery; writing
one of your own is covered in [plugins.md](plugins.md#writing-a-view).

The view manager
----------------

`view_manager` owns the lifecycle of every view — it plays the role
flumedb plays in the JavaScript stack. For each registered view it:

- keeps a **per-feed sequence checkpoint**: how far into each feed this
  view has folded;
- **catches up** at registration, folding whatever the view missed since
  its checkpoints were last saved;
- **rebuilds from scratch** when the view's version changes;
- **fans out** each newly stored message to every view, synchronously,
  on the store path;
- **publishes events** a view returns, as `{view_event, ViewMod, Event}`,
  to processes that called `view_manager:subscribe/1`. Live muxrpc
  streams (`{live: true}`) are built on these events.

Replay and rebuild fold the per-feed logs — archived segments in
sequence order, then each feed's live log. Feeds are folded
independently, so **per-author order is the only order a view may rely
on**. That is sufficient for SSB content semantics, since only my
messages assert my follows or my profile. A view that needs arrival
order across feeds reads the [ingest journal](storage.md#the-ingest-journal).

### Folding without stalling the node

Catch-up runs in the manager's own message loop, not inside the
registration call, in turns bounded by a **count of messages read**. The
bound is per message rather than per feed because feed sizes vary by
orders of magnitude: a fold that yielded only between feeds would sit on
one busy feed for minutes while everything sharing the process waited,
including ingest. So replication keeps flowing while a large view
rebuilds.

A view that is still catching up receives no live messages. Delivering
one would advance its checkpoint past everything the fold had not yet
reached, leaving a permanent hole. The fold sweeps until a whole pass
delivers nothing, and then the view goes live.

### Checkpoints

Checkpoints live in `store.db`, but an ETS table is the working copy.
Delivering a message consults and advances one checkpoint per view, so
writing through would put a store round trip per view on every ingested
message. Instead the manager tracks which `{view, feed}` pairs moved and
flushes only those, periodically — the cost is proportional to what
changed.

A crash can therefore lose the last few checkpoints, leaving view rows
*ahead* of their checkpoint, never behind. The view then refolds a few
messages it already folded, which is harmless because folds are
idempotent per `(feed, seq)`. Because checkpoints and rows live in the
same file, they live or die together.

Core views and app views
------------------------

**Core views** belong to the foundation (`apps/ssb`), are always on, and
are what an application may assume exists. They are registered before
any app view and are delivered each message first, so an app view folding
the same message sees core state already updated.

| View | Holds | Consumed by |
|---|---|---|
| `ssb_social_graph` | follow and block edges | EBT (who to replicate, who to refuse), every social client |
| `ssb_feed_meta` | latest self-asserted metadata per feed, `(feed, key, value, seq)` | anything that renders a feed id |
| `ssb_links` | every cross-feed reference as an edge | threads, backlinks, mentions |
| `ssb_archives` | archive boundaries published by the feeds we hold | boundary discovery, `archives.boundaries` |
| `ssb_glimpses` | the glimpse each replicated feed publishes, plus the edge table of offers | glimpse discovery, `glimpses.offers` |

`ssb_feed_meta` privileges no field: `name`, `image` and `description`
are Patchwork conventions, and an application with a different idea of
self-description gets the same machinery.

`ssb_links` extracts references lexically — anything shaped like a
message, feed or blob id anywhere in the content becomes a row, along
with the field it was found at — so the foundation indexes message types
it has never heard of. Those edges are worth a moment: if message M in
feed A carries the id of message N in feed B, then A held N when it
wrote M, because a hash cannot be referenced before it is seen. Together
with each feed's own sequence, the reference edges form a DAG whose
transitive closure is a causal partial order over the whole database —
the only trustworthy cross-feed ordering SSB has, since its timestamps
are self-asserted. Reading those edges as a *conversation* (roots,
branches, forks) is a separate interpretation and lives in `ssb_conv`'s
`tangle`.

**App views** are registered by applications and plugins: silkpurse's
threads, likes, channels and about rollups; ssb_git's repositories and
issues. The class is declared by the optional `view_class/0` callback and
defaults to `app`.

Not everything derived is a view. `mess_auth` (message id → author) is
an index written on the store path itself, because every read by message
id needs it before it can open a feed. It has its own `rebuild/0`.

Versions and rebuilds
---------------------

Every view declares `view_version/0`. When the stored version differs
from the code's — in either direction, so a downgrade rebuilds just as an
upgrade does — the manager resets the view and refolds everything.

The rule for anyone changing a view: **if the fold logic changes what a
message produces, bump the version.** Otherwise old rows stay in the
shape the old code wrote and only new messages get the new treatment.

Rebuilding by hand:

- `admin.views.list` shows every view's class, version, feed count and
  whether it has caught up;
- `admin.views.rebuild <name>` resets one view and schedules a refold.
  The reply comes back at once; the fold runs in turns as above.

Further reading
---------------

- [research/plugin-architecture.md](../research/plugin-architecture.md)
  — why views and plugins are two behaviours, and the choice of per-feed
  logs over a global one.
- [research/persistence.md](../research/persistence.md) §5 — the core
  views and the layering argument in full.
