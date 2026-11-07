erlbutt
=======

A [Scuttlebutt][1] peer, written in Erlang/OTP.

Scuttlebutt (SSB) is a peer-to-peer social network with no servers and no
accounts. Each person's posts go into their own signed, append-only log —
a *feed* — kept on their own machine, and friends' machines keep copies of
each other's feeds, passing updates along whenever they connect.

erlbutt is the part that does the work: it holds your identity and your
copies of everyone's feeds, connects to other peers and replicates. It has
no user interface of its own; you read and post through a client that
connects to it, such as [Silkpurse][6] or [HighWire][7], or with the
`sbutt` command line tool. The same node can run on your laptop or, with
a public address, as a pub or room that others connect through.

What it implements:

- the SSB protocol core: secret handshake, box stream, muxrpc, signed
  feeds, EBT replication, blobs, private messages;
- pubs and invites, and SSB rooms (connection relays, without the HTTP
  parts);
- the client surface a Patchwork-style UI expects;
- erlbutt's own extensions, designed to be invisible to peers that do not
  know them: **archive boundaries** (a feed can retire old history, and a
  newcomer can start partway down) and **feed glimpses** (a feed can
  describe itself to people just beyond its follow graph);
- **plugins**: extra RPC methods and indexes, loaded at boot — for
  example [ssb_git][8], git over SSB;
- feed **export and import** as portable, verifiable bundles.

Under the hood it stores each feed as its own log file and keeps every
index in an embedded SQLite database that can be rebuilt from the logs at
any time. The [architecture overview][2] explains why.

Install a release
-----------------

Download an `erlbutt-prod-…` tarball for your platform from the
[releases page][9]. It carries its own Erlang runtime, so nothing else is
needed:

```sh
mkdir -p ~/erlbutt && tar -xzf erlbutt-prod-*.tar.gz -C ~/erlbutt
~/erlbutt/bin/install-service --user --init     # macOS (launchd) or Linux (systemd --user)
ln -s ~/erlbutt/bin/sbutt ~/.local/bin/sbutt    # the CLI, on your PATH
sbutt whoami
```

`--init` creates a new identity in `~/.ssberl`; `--from DIR` brings an
existing erlbutt data directory instead. Upgrading is unpacking the new
release and running its `install-service` again — your data is never
touched.

Releases ship with outgoing dialing off. To let your node connect to
peers by itself, put `{peer_dialer, true}.` in `~/.ssberl/ssb.cfg` and
restart it.

[Getting started][3] walks through all of this, including connecting a
client and joining the network. Coming from a JavaScript client? See
[Moving from SSB][4]. Running a pub? See [Running a pub][5].

Build from source
-----------------

Needs Erlang/OTP 28, a C compiler, libsodium and git. rebar3 is included.

```sh
make rel                                      # build a development release
_build/default/rel/ssb/bin/ssb console        # run it, with a shell
make test                                     # unit tests
make ct                                       # multi-node Common Test suites
```

A development build joins a test network and keeps its identity inside
the release directory, so it can never post as you on the real network.
`make prodpkg` builds a production package. See
[Building and testing][10] and [Releasing][11].

Documentation
-------------

- [Getting started][3] and [configuration][12] — for running a node
- [Architecture overview][2] — for understanding the code
- [Writing plugins][13]
- [Everything else][14] — the full documentation index

There is also a [dev diary][15], posted to SSB as it was written, so
links to SSB messages in it resolve only from an SSB client.

License
-------

GPL-2.0-only; see [LICENSES/COPYING](LICENSES/COPYING). Test data is MIT
licensed (`testdata.zip.license`). Contributors are listed in
[BUTTS](BUTTS).

----
[1]: https://scuttlebutt.nz
[2]: doc/architecture/overview.md
[3]: doc/guide/getting-started.md
[4]: doc/guide/moving-from-ssb.md
[5]: doc/guide/running-a-pub.md
[6]: https://github.com/cmoid/silkpurse
[7]: https://github.com/mwmiller/highwire
[8]: https://github.com/cmoid/ssb_git
[9]: https://github.com/cmoid/erlbutt/releases
[10]: doc/development/building-and-testing.md
[11]: doc/development/releasing.md
[12]: doc/guide/configuration.md
[13]: doc/architecture/plugins.md
[14]: doc/README.md
[15]: doc/dev-diary/
