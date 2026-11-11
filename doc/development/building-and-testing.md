Building and testing
====================

Prerequisites
-------------

- **Erlang/OTP 28.** The code uses the `~"..."` binary sigil, so older
  releases will not compile it.
- **A C compiler, and libsodium** (headers included). The `enacl` NIF
  links libsodium; on macOS `brew install libsodium`, on Debian/Ubuntu
  `apt install libsodium-dev`. SQLite needs nothing: `esqlite` bundles it.
- **git** — dependencies are fetched from git, and release versions come
  from `git describe`.

rebar3 is vendored as `./rebar3`; the Makefile uses it.

Building
--------

```sh
make compile     # compile everything
make rel         # build a development release in _build/default/rel/ssb
```

Run the release in the foreground with a shell:

```sh
_build/default/rel/ssb/bin/ssb console
SSB_LOG_LEVEL=debug _build/default/rel/ssb/bin/ssb console
```

A development release joins a **test network** (an id one transposition
away from the main SSB network's), and keeps its identity and data in
`.ssberl` *inside the release directory*. So a node you build to
experiment with cannot post as you or reach the real network, and `make
rel` again does not touch its data. Building the `prod` profile is the
deliberate act that targets the main network; see
[releasing.md](releasing.md).

To run a second node beside it — the only way to exercise anything that
needs a peer — copy the release and start the copy with a different
`SSB_PORT` (and `SSB_NODE`). The [archive](testing-archives.md) and
[glimpse](testing-glimpses.md) test procedures walk through this step by
step.

Plugins
-------

`make rel` also builds the plugins named in `PLUGINS` into the release's
`plugins/` directory. It defaults to `../ssb_git` when that directory
exists; `make rel PLUGINS=` builds without. See
[plugins](../architecture/plugins.md#packaging-a-plugin).

Tests
-----

```sh
make test    # eunit: unit tests, in the modules themselves
make ct      # Common Test: suites that start real nodes and connect them
```

**eunit** tests live at the bottom of the module they test, inside
`-ifdef(TEST)`. `make test` unpacks `test/testdata.zip` (sample feeds, blobs
and vector clocks) into `testdata/` first and removes it afterwards.
Run `make test` after every change.

**Common Test** suites live in `apps/*/test/`:

| Suite | Covers |
|---|---|
| `erlbutt_basic_SUITE` | a handshake against a node |
| `erlbutt_peer_SUITE` | loopback peer communication |
| `erlbutt_conn_SUITE` | `pub` messages into `conn_db` and `conn.json` |
| `erlbutt_two_node_SUITE` | replication and blobs between two nodes |
| `erlbutt_identity_SUITE` | one store, several identities, a node stopped by a signal |
| `erlbutt_archive_SUITE` | feed archiving |
| `erlbutt_invite_SUITE`, `erlbutt_room_invite_SUITE` | pub and room invites |
| `erlbutt_rooms_SUITE` | room presence, tunnels, replication through a room |
| `erlbutt_multi_netid_SUITE` | several network ids |
| `silkpurse_SUITE` | the client surface, end to end over a real connection |
| `plumtree_SUITE` | plumtree broadcast |

Multi-node suites start extra nodes with OTP's `peer` module, which needs
Erlang distribution; `make ct` passes `--sname erlbutt_ct`, which starts
`epmd` if it is not running.

Conventions worth knowing:

- **Layering is tested.** `ssb_layering_tests` fails if anything in
  `apps/ssb` calls into `ssb_conv`, silkpurse or any other application.
- **Blob tests use unique payloads** (`crypto:strong_rand_bytes/1`), since
  a blob is named by its content and a fixed payload collides with one
  stored by an earlier test.
- **Test fixtures** for the plugin and view machinery are small modules
  in `apps/ssb/test/` (`test_echo_plugin`, `test_counter_view`, …).
- A view whose fold logic changes gets its `view_version/0` bumped
  ([views](../architecture/views.md#versions-and-rebuilds)), and a
  change to the on-disk format of the truth tier needs a
  `data_version` bump.

Working on a live node
----------------------

The development release includes the compiler and tools, so you can
attach and reload code without restarting:

```sh
_build/default/rel/ssb/bin/ssb remote_console
```

The release boots in **embedded mode**, which loads modules up front and
never on demand. A module that was not part of the boot — a freshly
built one, or one from another project such as maxbutt — must be loaded
by hand (`code:load_file(Mod)`) after each start.

Where to start reading
----------------------

- [Architecture overview](../architecture/overview.md), then whichever
  page covers the part you are changing.
- The module headers: most modules open with a comment explaining what
  they own and why, and that comment is kept current.
- [research/](../research/) for the reasoning behind a design.
