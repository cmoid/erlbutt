sbutt — the command line tool
=============================

`sbutt` comes with every release, as `bin/sbutt`. Symlink it onto your
`PATH`; it follows the link back to its release, so it always matches the
node it ships with.

```sh
ln -s ~/erlbutt/bin/sbutt ~/.local/bin/sbutt
```

Most commands connect to the node running on this machine, on port 8008
(or `SSB_PORT`), using the node's own identity — the same way a client
does. A few read the data directory directly and work with the node
stopped; those are marked.

On a pub installed with `install-service --system`, use the `sbutt`
wrapper in `/usr/local/bin` instead: it runs the command as the service
account, with the service's environment, which is what the node's files
require.

Identity and service
--------------------

| Command | What it does |
|---|---|
| `sbutt init` | Create this node's identity, if it has none. *No node needed.* |
| `sbutt init --from DIR` | Adopt an existing erlbutt data directory (copied, not moved). *No node needed.* |
| `sbutt init --check` | Exit 0 if there is a usable identity. *No node needed.* |
| `sbutt install-service --user\|--system [...]` | Install the release as a service; the same as `bin/install-service`. `--help` lists its options. |
| `sbutt whoami` (or `id`) | Print the node's feed id. |
| `sbutt ping` | Check the node answers. |
| `sbutt health` | A report on the node, its views and its store: feeds, hops, dialer, invalid signatures, each view's progress, store tables, connected peers. Start here when something seems wrong. |

Publishing
----------

| Command | What it does |
|---|---|
| `sbutt publish --type T --text TEXT` | Publish a message on your feed. |
| `sbutt about [--name N] [--description D] [--image PATH]` | Set your profile name, description or avatar. |
| `sbutt invite create HOST PORT` | Mint a pub invite code for someone, naming the address they should connect to. |

Reading
-------

| Command | What it does |
|---|---|
| `sbutt get MSGKEY` | Print one message by its `%….sha256` key. |
| `sbutt hist --id FEEDID [--limit N]` | Print one feed's history. |
| `sbutt log` (or `feed`) | Print every message, in the order they arrived. |

Output is the messages as JSON, which pipes into `jq`.

Glimpses
--------

See [glimpses](../architecture/feeds-archives-glimpses.md#glimpses-and-pins).

| Command | What it does |
|---|---|
| `sbutt glimpse publish [--statement S]` | Publish what your feed says about itself to people at the edge of your graph. |
| `sbutt glimpse show [FEEDID]` | Show a glimpse (your own by default). |
| `sbutt glimpse edge` | List the glimpses peers have offered from just beyond your graph. |
| `sbutt glimpse promote FEEDID` | Start replicating a feed, privately, without following it. |
| `sbutt glimpse demote FEEDID` | Stop replicating it (keeps what already arrived). |

Data
----

| Command | What it does |
|---|---|
| `sbutt export DIR [FEEDID ...]` | Write feeds (your own by default) and their blobs to a new directory. *Reads the store directly; safe beside a running node.* |
| `sbutt import DIR` | Verify a bundle and load whatever is new into the running node. |
| `sbutt census encoding [--limit N]` | Count stored messages that cannot be encoded for a client. *No node needed.* |

Export and import are described in
[Backup, export and import](backup-export-import.md).

Going further
-------------

Everything `sbutt` does goes through the node's muxrpc interface, and an
owner client can do more — see the [method list](../architecture/rpc-methods.md),
especially `admin.*`. For an Erlang shell on the running node, use
`bin/ssb remote_console` (or `sbutt-console` on a pub).
