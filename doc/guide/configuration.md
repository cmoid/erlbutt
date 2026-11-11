Configuration
=============

Most nodes need no configuration at all. This page lists what can be
changed, and where.

Where settings come from
------------------------

Settings are read in layers, each overriding the one before:

1. **Built-in defaults.**
2. **The release's `ssb.cfg`**, at the top of the release directory. It
   is generated when the release is built, and **replaced on every
   upgrade** — don't edit it.
3. **Your `ssb.cfg`**, in the data directory (`~/.ssberl/ssb.cfg`). This
   is where your settings go: an upgrade never touches it. If it cannot
   be read, the node refuses to start rather than quietly falling back to
   defaults (which could put it on a different network).
4. **`overrides.cfg`**, in the data directory, written by the node itself
   when a setting is changed at runtime — for example
   `admin.dialer.disable`. A corrupt one is ignored with a warning.

Both files hold Erlang terms, one per line, each ending in a full stop:

```erlang
{replication_hops, 3}.
{peer_dialer, true}.
{plugins, [ssb_git]}.
```

An unrecognised setting, or a known one with a value of the wrong type
(`{peer_dialer, "false"}` — a string, not the atom `false`), is logged
and ignored.

Settings
--------

| Setting | Default | What it does |
|---|---|---|
| `network_id` | the main SSB network (prod builds) | Which SSB network to join. Peers on a different network id cannot connect at all. |
| `extra_network_ids` | `[]` | More network ids to accept, as a list of base64 strings. |
| `replication_hops` | `2` | How far out the follow graph to replicate: 1 is the feeds you follow, 2 adds the feeds they follow. Each hop can multiply the data stored many times over. |
| `peer_dialer` | `true` (releases); `false` (development builds) | Whether the node dials out to known peers by itself. A node that never dials only gets what others bring it. Changed at runtime with `admin.dialer.enable` / `disable`, which persists. |
| `require_valid_sigs` | `true` | Reject messages whose signature does not verify. Leave it on. |
| `blob_scan` | `false` | At startup, scan every stored message for blobs and fetch any that are missing. A full pass over the store. |
| `archive_length` | unset | Archive your own feed automatically every N messages. Leave it unset on a pub. See [archives](../architecture/feeds-archives-glimpses.md#archive-boundaries). |
| `archive_floors` | `true` | Accept a peer's offer to start a feed partway down, at an archive boundary, instead of from the beginning. |
| `archive_serving` | `true` | Keep serving archived history to peers who ask for it. |
| `pin_archives` | `false` | Fetch and keep the archive blob behind every boundary, for feeds you replicate. Off on a personal node (it downloads the history archiving lets you skip); worth turning on for a pub with the disk, so somebody keeps that history reachable. |
| `glimpses` | `true` | Ask peers about feeds just beyond your follow graph. See [glimpses](../architecture/feeds-archives-glimpses.md#glimpses-and-pins). |
| `plugins` | `all` | Which plugins to start: `all`, a list such as `[ssb_git]`, or `[]` for none. |
| `room` | `false` | Act as an SSB room (a connection relay). |
| `room_name` | `<<"erlbutt room">>` | The room's name, as an Erlang binary. |
| `room_privacy` | `open` | `open`, `community` or `restricted`; the last two admit only members, who join with a room invite. |

**The dialer.** Releases ship with it on; development builds (`make
rel`, `devpkg`) ship with it off, so a test node makes only the
connections you make for it. To run a released node inbound-only — a
pub that waits for others, or a converted feed you have not verified
yet — add `{peer_dialer, false}.` to `~/.ssberl/ssb.cfg`. Because
`overrides.cfg` is read last, the runtime switch wins over both:
a dialer turned off with `admin.dialer.disable` stays off whatever
`ssb.cfg` says, until `admin.dialer.enable`, and the other way round.

Environment variables
---------------------

| Variable | Default | What it does |
|---|---|---|
| `SSB_DATA` | see below | Where the data directory is. |
| `SSB_PORT` | `8008` | The port the node listens on for peers and clients. `sbutt` reads it too, so a CLI started in the same environment talks to the same node. |
| `SSB_LOG_LEVEL` | `error` | `error`, `info` or `debug`. |
| `SSB_NODE` | `ssb@127.0.0.1` (prod) | The Erlang node name, for remote shells and tools like maxbutt. |
| `SSB_DIST_PORT` | `9100` | The Erlang distribution port. Bound to loopback in prod; never open it on a firewall. |
| `SSB_COOKIE` | unset | Pin the Erlang cookie. Otherwise prod uses `~/.erlang.cookie`, created on first start. |

With `install-service`, pass these as `--env K=V` so the service gets
them.

Where the data lives
--------------------

The data directory — identity, feeds, blobs, store — is chosen in this
order, first match wins:

1. `SSB_DATA`, if set.
2. `SSB_HOME`, if set (older form: the data is `$SSB_HOME/.ssberl`).
3. A `.ssberl` inside the release directory, if one exists — an older
   install that kept its data there goes on using it, with a warning.
4. The build's default: `~/.ssberl` for a production release; a
   `.ssberl` inside the release directory for development builds.

Development builds (`make rel`) deliberately keep their own identity
inside the release and use a test network id, so a node you build to
experiment with can never post as you or reach the real network by
accident.

Only one node may run on a data directory at a time; a second refuses to
start (the `LOCK` file).

Logs
----

Logs are written under `log/` in the node's working directory (the
release directory, for an installed service): `error.log`, `info.log`
and `debug.log`. Each rotates at 50 MB, keeping five compressed
generations, so logs never fill a disk.
