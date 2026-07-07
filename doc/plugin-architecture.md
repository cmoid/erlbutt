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

## The open design question

Is a view a fold over **SSB messages**, or over **log entries generally**?

If the other intended use cases still ride on signed SSB feeds (sensor
data, package metadata, wiki edits as message types), the message-level
abstraction is right, and content-type dispatch belongs in the view API.
If non-SSB logs are in scope (e.g. plumtree as a transport for arbitrary
payloads), the view manager should sit below message semantics and take
opaque entries, with SSB decoding as a layer on top.

This choice is cheap now and expensive later, so it should be settled
before the view manager is built.
