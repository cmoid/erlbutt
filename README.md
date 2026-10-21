erlbutt
======

An implementation of the [`SSB`][1] protocol.

## [Project notes][2]

## Installing a release

Download (or `make prodpkg`) an `erlbutt-prod-*.tar.gz` for your platform
and unpack it anywhere. It carries its own Erlang runtime, so nothing else
needs installing. Then:

```
tar -xzf erlbutt-prod-*.tar.gz -C ~/erlbutt
~/erlbutt/bin/install-service --user --init        # macOS (launchd) or Linux (systemd --user)
ln -s ~/erlbutt/bin/sbutt ~/.local/bin/sbutt       # the CLI, on PATH
sbutt whoami
```

`--init` creates a new identity. To bring one you already have, use
`--from ~/old/.ssberl` instead. The data lives in `~/.ssberl`, outside
the release, so upgrading is: unpack the new release and run its
`install-service` again. Settings that should survive upgrades go in
`~/.ssberl/ssb.cfg`. The distribution cookie is `~/.erlang.cookie`.

For a pub on a Linux server: `sudo bin/install-service --system --init`
(or `--from DIR`). That creates the `erlbutt` service account, installs
the release into `/opt/erlbutt`, the systemd unit, and the `sbutt` /
`sbutt-console` wrappers. See [the VPS guide][5] for the background.

## Building

```
make test

make ct

make rel

```
To run:
```
./_build/default/rel/ssb/bin/ssb-0.0.1 console

```
An environment variable can be used to control logging, .eg.:

```
SSB_LOG_LEVEL=debug _build/default/rel/ssb/bin/ssb-0.0.1 console
```

See [ssb conversion][4] for details on converting an existing `SSB` store to erlbutt.

There is also a [dev-diary][3] that are posts made on `SSB`, so many of the linked items in those entries are not visible in github.

----
[1]: https://ssbc.github.io/scuttlebutt-protocol-guide/
[2]: https://github.com/cmoid/erlbutt/blob/main/doc/project.md
[3]: https://github.com/cmoid/erlbutt/tree/main/doc/dev-diary
[4]: https://github.com/cmoid/erlbutt/blob/main/doc/ops/ssb-conversion.md
[5]: https://github.com/cmoid/erlbutt/blob/main/doc/ops/vps-install.md
