# A plugin architecture for erlbutt

*Design note, July 2026. Motivated by the idea of replacing silkpurse's
embedded ssb-server with erlbutt, but deliberately general: plugin support
is a first-class capability, and the social-media stack is just one
consumer of it.*

## What a secret-stack plugin actually is

"Support patchwork's plugins" sounds like ~23 bespoke features, but a JS
secret-stack plugin bundles three separable capabilities, and almost all of
the patchwork plugins are instances of the first two:

1. **An RPC namespace** — a manifest fragment (`friends.hops: async`,
   `patchwork.publicFeed.roots: source`) plus handlers, exposed over muxrpc
   with per-method permissions.
2. **A derived index over the log** (flume's role in JS) — a fold over the
   append-only log with a checkpoint (the log sequence it has consumed),
   rebuildable when the fold logic changes version, and live-tailing so
   open streams get updates as new messages arrive.
3. **Hooks into other plugins** — auth hooks, connection hooks, wrapping
   other methods.

The hard part is not any single plugin. JS got (2) for free from flume;
erlbutt hand-rolls it every time it needs an index.

## What erlbutt already has, in embryo

- **A dispatch point:** the per-connection `rpc_processor`. Every new
  namespace so far (rooms, invites, publish/get/log) was added by editing
  it directly — exactly the pain a plugin registry removes.
- **An ingest fan-out hook:** `social_msg:dispatch` is already the
  "message arrived → notify interested subsystems" point, hardcoded today.
- **Hand-rolled views with divergent persistence:** `friends` (follow and
  block graphs, name cache; ETS + tab2file), `mess_auth` (ETS + tab2file),
  `tangle`, `conn_db` (JSON file). Four views, four storage stories.
- **A plugin seam that already exists:** the plumtree app's
  `replication_strategy` behaviour is this pattern applied to replication.

## Proposed architecture: two behaviours

### `ssb_plugin` — RPC extensibility

`manifest/0` (method → sync/async/source + permission class) plus
handlers. A registry ETS table maps `<<"namespace.method">>` to the
implementing module; `rpc_processor` falls through to the registry for
anything it doesn't handle itself, so existing methods migrate
strangler-style.

Two things fall out for free:

- **`manifest.json`** is just the union of registered manifests (one of
  the gaps blocking JS clients like silkpurse from talking to erlbutt).
- **An auth model.** Tag each connection (`#ssb_conn{}`) with a class at
  handshake time — owner client / room member / peer / stranger — then
  filter the manifest and gate dispatch by each method's permission class.
  This one mechanism is both the equivalent of ssb-master/ssb-no-auth
  (privileged local clients) and a general capability system.

### `ssb_view` — derived indexes over the log

`version/0`, `init/1`, `handle_msg/2` (the fold), plus query callbacks.
A view-manager gen_server owns the lifecycle:

- per-view sequence checkpoint, persisted;
- replay from checkpoint at startup;
- full rebuild when a view's `version/0` bumps;
- ingest fan-out (generalizing `social_msg:dispatch`);
- change events (pg or gen_event) so muxrpc source streams can tail live.

Port `friends`, `mess_auth`, and `tangle` first to prove the abstraction
on code with existing tests. After that, patchwork's `publicFeed.roots`
(the front page) becomes "just another view" rather than a mountain.

**The view manager is the load-bearing piece.** It is also what every
non-social use case needs — any application on a replicated log reduces to
"fold the log into queryable materialized state and tail it live." The
social plugins become one consumer of the substrate rather than the
substrate itself.

## Erlang-specific opportunities JS doesn't have

- **Plugins as OTP apps.** plumtree already lives as a separate app in the
  same release. A plugin = an OTP app that registers its `ssb_plugin` /
  `ssb_view` modules at startup. You get supervision, crash isolation (a
  broken view doesn't take down replication), and hot code reload of a
  single plugin on a live node — none of which secret-stack offers.
- **Out-of-process plugins.** Since the surface is muxrpc anyway, a plugin
  can be a *remote* handler: "forward `foo.*` to this authenticated
  client." That lets a Python or JS process extend an erlbutt node without
  writing Erlang. Cheap to add once the registry exists.

## Suggested sequencing

1. **Registry + manifest + connection classes.** Small, and immediately
   unblocks JS clients (silkpurse phase 1).
2. **View manager**, with `friends` as the first ported view — it has
   existing tests and exercises checkpoint, rebuild, and live-tail.
3. **Feed views one at a time**, starting with `publicFeed.roots`.

## The settled design question (July 2026)

Is a view a fold over **SSB messages**, or over **log entries generally**?

**Decision: opaque entries, decode-once at ingest.** All use cases ride
signed SSB feeds, so every entry has a `(feed, seq)` identity and passes
through verification (which forces a decode) exactly once, at ingest.
Views receive `{FeedId, Seq, Raw, Decoded}` — content-agnostic views
ignore `Decoded`; content-aware views (friends, abouts, tangles) use it
without re-parsing.

Content-aware replication is not a counterexample to opacity: as in the
JS stack (where ssb-friends is itself a flumeview that ssb-ebt consults),
"replication looks inside messages" means **one view** (friends) decodes
contact messages, and the replication machinery consumes that view's
*output* and change events. The substrate stays dumb; EBT's repl-set
becomes a subscriber of the friends view, which also removes the
refresh-timer lag class of bugs by construction.

### Storage substrate: per-feed logs, not a global log

The per-feed `ssb_feed` + `log.offset` design is the flume substitute.
Almost every SSB view needs only **per-author order** (only my messages
assert my follows/abouts/sequence), so folds interleave feeds freely and
a view checkpoint is a small **vector clock** (`feed → seq` consumed) —
the same shape as an EBT clock. This buys partial rebuilds (re-fold one
feed) and parallel replay.

Two things want a global order before the legacy global `log.offset`
(now just a converter convenience) can be retired: `createLogStream`
and cheap "everything since I last looked" live-tail resume. Replace it
with a tiny **ingest journal** — append-only `{feed_id, seq}` refs, no
bodies — giving a stable arrival order and single-integer checkpoints
to the views that want them, without duplicating the data.

### Ingest pipeline

entry arrives (EBT / plumtree / CHS / publish) → verify + decode once →
append to per-feed log → append ref to ingest journal → view manager
fans out `{FeedId, Seq, Raw, Decoded}` to subscribed views → views
update state and emit change events (replication, muxrpc live streams,
and the UI are all just subscribers).
