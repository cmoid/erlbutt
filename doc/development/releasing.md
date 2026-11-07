Releasing
=========

Profiles
--------

Three release profiles, built with rebar3 and wrapped by the Makefile:

| Target | Profile | Network | Data directory | Contents |
|---|---|---|---|---|
| `make rel` | default | test network | inside the release | system ERTS; compiler and tools for live development |
| `make devpkg` | `devpkg` | test network | inside the release | bundled ERTS, debug info, source, and the GUI tools (debugger, observer) for maxbutt |
| `make prodpkg` | `prod` | **main SSB network** | `~/.ssberl` | bundled ERTS, stripped beams, no source, no compiler; warnings are errors |

Only `prod` reaches the real network. Every other build — `make rel`,
`devpkg`, eunit, CT — uses the test network id, so reaching the main
network is something you ask for by name. The difference lives in
`config/prod.vars`, layered over `config/default.vars` when `ssb.cfg` is
rendered.

A profile's `{release, ...}` app list **replaces** the base list in
`rebar.config` rather than adding to it, so a new application must be
added to all three.

`make devpkg` and `make prodpkg` write
`dist/erlbutt-{dev,prod}-<version>-<os>-<arch>.tar.gz` and a `.sha256`
beside it. The version is `git describe`. Packages are native builds:
they carry the build machine's ERTS, so build each platform on that
platform. On macOS, `scripts/vendor-macos-dylibs.sh` copies the
Homebrew libraries the NIFs link (OpenSSL, libsodium, wx) into the
tarball and repoints them, so a package runs on a Mac without Homebrew.

What a release contains
-----------------------

```
bin/ssb                  the relx start script (foreground, console, remote_console, stop)
bin/sbutt                the CLI launcher
bin/install-service      service installer (launchd, systemd --user, systemd system)
sbutt.escript            the prebuilt CLI
ssb.cfg                  rendered release config
share/ops/               the pub's sbutt and sbutt-console wrappers (from config/pub/)
plugins/                 plugins built from $PLUGINS
lib/, releases/, erts-*  the OTP release
```

`sbutt.escript` is built by `scripts/build-sbutt.escript` and plugins by
`scripts/stage-plugins.sh`, both run by rebar3 `pre_hooks` before every
`release` and `tar`, so a release cannot ship a stale CLI.

Cutting a release
-----------------

1. Make sure `make test` and `make ct` pass.
2. Tag and push the tag:

   ```sh
   git tag -a v0.2.0 -m 'erlbutt v0.2.0'
   git push gh v0.2.0
   ```

3. On each build machine, with that tag checked out:

   ```sh
   make release
   ```

   This builds both packages and attaches them to the GitHub release for
   the tag, creating it on the first machine and adding to it on the
   others (`gh` must be installed and authenticated). Re-running replaces
   that platform's assets.

Compatibility rules
-------------------

Releases are upgraded in place — unpack the new one, run its
`install-service` — and the data directory carries over. These rules keep
that safe:

- **Views:** bump `view_version/0` when a fold's output changes. The
  view rebuilds on first start, in either direction, so an upgrade and a
  rollback both work. See [views](../architecture/views.md#versions-and-rebuilds).
- **Store columns** added to an existing table need a `DEFAULT`, or rows
  written by the old build break the new one.
- **The truth tier:** bump `DATA_VERSION` in `data_version.erl` when a
  change makes data written by the new build unreadable, or readable but
  wrong, to the old one. Not for additive changes (a new table, config
  key or message type). A node refuses to start on data newer than
  itself, rather than misreading it.
- **Plugins:** bump `SSB_PLUGIN_API` in `ssb.hrl` when the `#message{}`
  record or anything else a plugin compiles in changes shape, so plugins
  built for the old layout are refused instead of crashing. See
  [plugins](../architecture/plugins.md#the-api-version).
