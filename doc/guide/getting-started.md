Getting started
===============

This page takes you from nothing to a running erlbutt node with a client
attached. No Erlang knowledge needed.

What you are installing
-----------------------

[Scuttlebutt](https://scuttlebutt.nz) (SSB) is a peer-to-peer social
network with no servers. Everything you post goes into your own signed,
append-only log — your *feed* — which lives on your own machine. Your
friends' machines keep copies of your feed, and yours keeps copies of
theirs, passing updates along whenever two of them connect. There is no
company in the middle and no account to sign up for: your identity is a
keypair, created on your machine, and it never leaves it.

**erlbutt is the part that does the work**: it holds your identity and
your copies of everyone's feeds, connects to other peers, and replicates.
It has no window of its own. To read and post you use a **client** that
connects to it:

- [Silkpurse](https://github.com/cmoid/silkpurse) — a desktop app in the
  Patchwork tradition.
- [HighWire](https://github.com/mwmiller/highwire) — a browser-based
  client.
- [maxbutt](https://github.com/cmoid/maxbutt) — an Emacs client. It works
  differently from the others: rather than connecting the way a peer
  would, it joins the running node as an Erlang node, so it suits people
  who run their own node and are at home in Emacs. Its own README covers
  setting it up.
- `sbutt`, the command line tool that comes with erlbutt.

You can run erlbutt on your own computer, or on a server that is always
on (a *pub*) and point clients at it from anywhere.

1. Install a release
--------------------

Releases are on the [GitHub releases page](https://github.com/cmoid/erlbutt/releases).
Download the `erlbutt-prod-…` tarball for your platform. If there isn't
one for your platform, [build from source](../development/building-and-testing.md).

A release carries its own Erlang runtime, so there is nothing else to
install. Unpack it anywhere you like; `~/erlbutt` is used below.

```sh
mkdir -p ~/erlbutt
tar -xzf erlbutt-prod-*.tar.gz -C ~/erlbutt
```

Put the command line tool on your `PATH`:

```sh
ln -s ~/erlbutt/bin/sbutt ~/.local/bin/sbutt
```

2. Give it an identity, and start it
------------------------------------

Your identity is a file called `secret` in erlbutt's data directory,
`~/.ssberl`. **That file is you.** Anyone who has it can post as you, and
if you lose it there is no way to recover the identity. Back it up
somewhere safe and private.

**If you are new to Scuttlebutt**, let erlbutt create one and install
itself as a service that starts when you log in:

```sh
~/erlbutt/bin/install-service --user --init
```

**If you already have an erlbutt data directory** (from another
machine, say), bring it instead:

```sh
~/erlbutt/bin/install-service --user --from /path/to/old/.ssberl
```

**If you are coming from a JavaScript client** (Patchwork, ssb-server,
anything with a `~/.ssb` directory), follow
[Moving from SSB](moving-from-ssb.md) instead. Do not just copy the
`secret` across: see the warning below.

`--user` works on macOS (a launchd agent) and Linux (a systemd user
unit). To run it by hand instead, in a terminal:

```sh
sbutt init                      # create the identity, once
~/erlbutt/bin/ssb foreground    # run until you press Ctrl-C
```

Check that it is up:

```sh
sbutt whoami      # your feed id: @….ed25519
sbutt health      # node, views and store at a glance
```

> **One identity, one running node.** Never run two nodes — on one
> machine or two — with the same `secret`, and never start a fresh node
> with an old `secret` while your old feed is somewhere else. Each would
> sign different messages at the same position in your feed, and peers
> treat a feed like that as broken. erlbutt refuses to publish when it
> can tell it is behind, but it cannot always tell.

3. Connect a client
-------------------

A client authenticates *as* your node, using the same `secret`, so
anything you post through it goes on your feed.

For Silkpurse, point it at the node's address and secret in
`~/.silkpurse/config`:

```json
{
  "erlbutt": {
    "secret": "/Users/you/.ssberl/secret",
    "addr": "127.0.0.1:8008"
  }
}
```

Silkpurse's own [install notes](https://github.com/cmoid/silkpurse/blob/main/docs/INSTALL.md)
cover the rest, including pointing it at a node on another machine.

4. Meet the network
-------------------

A new node knows nobody, so it has nothing to replicate. You need
someone to follow and someone to follow you back. The usual way in is a
**pub invite**: ask someone who runs a pub for an invite code and redeem
it in a client that supports invites (HighWire does). The pub follows you, you follow it, and replication
starts from there. Follow people you meet, and their feeds — and the
feeds they follow — start arriving.

The node dials out by itself — to peers it learns about from the feeds
it replicates, and to peers on the same local network, which find each
other automatically. `sbutt health` shows the dialer's state.

Day to day
----------

- **Upgrading**: unpack the new release and run *its*
  `install-service --user` again. Your data in `~/.ssberl` is never
  touched by an upgrade.
- **Settings**: put them in `~/.ssberl/ssb.cfg` — see
  [Configuration](configuration.md).
- **Logs**: in the release's `log/` directory. Set `SSB_LOG_LEVEL=info`
  for more detail.
- **Backups**: `~/.ssberl/secret`, `feeds/` and `blobs/`. See
  [Backup, export and import](backup-export-import.md).
- **Commands**: see the [sbutt reference](sbutt.md).

Running a pub for other people is a step further: see
[Running a pub](running-a-pub.md).
