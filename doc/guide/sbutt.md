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
| `sbutt gc plan [--grace DAYS] [--top N\|all] [--no-record]` | List the stored feeds that have fallen out of the replication set and what removing them would touch. *Removes nothing.* |
| `sbutt gc run [--grace DAYS] [--keep-views] [--yes]` | Move the eligible feeds into a quarantine. Without `--yes` it only says what it would do. *Node stopped.* |
| `sbutt gc restore STAMP` | Put a quarantine back. *Node stopped.* |
| `sbutt gc list` | List quarantines. |
| `sbutt gc purge STAMP` | Delete a quarantine for good. |
| `sbutt gc blobs plan [--grace DAYS] [--top N\|all]` | List the stored blobs nothing references any more, and how much space they take. *Removes nothing.* |

Export and import are described in
[Backup, export and import](backup-export-import.md).

### Feeds the graph no longer reaches

A node keeps every feed that was ever in its replication set (yourself,
follows out to `replication_hops`, room members and pins, minus blocks).
The set moves when you or the people you follow unfollow someone, or when
you lower the hop count. Nothing removes the feeds that fall out of it.
`sbutt gc plan` lists them.

It reports, for the feeds outside the set and for the subset that is
*eligible*:

- their size on disk and how many messages they hold;
- their refs in the ingest journal;
- their view checkpoints, validation floors and `feed_meta` rows.

It also lists the largest of them, with names where known. Other view
rows are rebuilt by a run rather than counted.

A feed becomes eligible only after it has been outside the set for the
grace period (`--grace`, 7 days by default). The node records when each
one was first seen outside the set in `<data>/gc/candidates`, and that
ledger is the only thing a plan writes. A feed that comes back into the
set leaves the ledger, and its grace period starts over if it falls out
again. Use `--no-record` to look without updating the ledger.

The plan refuses to answer rather than guess when:

- the store can't be read;
- the follow graph is still catching up;
- the keep set holds nobody but you while `replication_hops` is above 0.

It reads every feed directory and the whole journal, so expect it to take
seconds to a minute.

#### Removing them

`sbutt gc run` works on a **stopped** node. It takes the node's LOCK, so
the node can't start while it runs. It removes a feed only when both of
these are true:

- a plan recorded by the running node named the feed at least the grace
  period ago;
- the feed is still outside the replication set, worked out afresh from
  the same store.

If you re-follow someone after the plan, their feed is kept.

What it does, in order:

1. Moves each feed's directory into `<data>/gc/<STAMP>/feeds/`, beside a
   `MANIFEST`.
2. Rewrites the ingest journal without those feeds' refs. The surviving
   refs keep their order, and the old journal stays in the quarantine.
3. Deletes the feeds' validation floors, view checkpoints, `feed_meta`
   rows and message-id index rows from `store.db`, in one transaction.
4. Marks every view to rebuild when the node next starts, because other
   view rows (links, threads, contacts) would otherwise still point at
   messages that are gone. The node serves while the views refold, but
   the views are incomplete until the refold finishes. `--keep-views`
   skips the rebuild and leaves those rows stale.

On a store of 2.5 million messages, removing 42,000 feeds (1.25 million
messages) took about 25 seconds.

`sbutt gc restore STAMP` puts a quarantine back:

- the feed directories;
- the journal, merged so that refs the node added since the run are kept
  after the restored history;
- the validation floors;
- the message-id index.

The views fold the restored feeds in at the next start. If a feed was
fetched afresh after the run, the store's copy is kept and the
quarantined one stays where it is. Once you are sure,
`sbutt gc purge STAMP` frees the space.

If a run is interrupted, the manifest records how far it got. A later
`gc run` refuses until you `restore` that quarantine, and restoring is
safe to repeat.

Removed feeds can't always be fetched again: peers drop feeds too, and
some peers remember what they think you hold and won't resend it. That
is why removal goes through a quarantine.

Blobs are not touched. Archive segments and images belonging to removed
feeds stay in `blobs/` (see the next section).

### Blobs nothing references

A blob is fetched because a stored message named it, and nothing ever
removes it. `sbutt gc blobs plan` runs on the live node and reports which
blobs nothing references any more. It writes nothing.

A blob is kept if any of these hold:

- a message in any stored feed names it. The plan checks the links index,
  which can read private messages addressed to you, and also scans every
  message's raw bytes, so a `![img](&….sha256)` written inside post text
  counts.
- a message in a feed held in an unpurged `gc run` quarantine names it,
  so that `gc restore` brings feeds back with their images. Those blobs
  become candidates once you purge the quarantine.
- a table names it: archive segments, validation floors, glimpses, or
  outstanding wants.
- it changed within the grace period (`--grace`, 7 days by default),
  which covers an upload you haven't posted yet.

Everything else is a candidate. The plan refuses to answer while the
links view is still catching up. It reads every stored message, so on a
large store expect it to take about a minute.

Going further
-------------

Everything `sbutt` does goes through the node's muxrpc interface, and an
owner client can do more — see the [method list](../architecture/rpc-methods.md),
especially `admin.*`. For an Erlang shell on the running node, use
`bin/ssb remote_console` (or `sbutt-console` on a pub).
