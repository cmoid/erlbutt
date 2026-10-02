# Exporting and importing feeds

Your data is not sovereign if it is not portable. `export` writes one or
more feeds and every blob they reference into a self-contained directory
(a *bundle*); `import` reads a bundle into a node, checking every
message and blob before storing anything new.

Typical uses:

- take your own feed off a node, to keep or to hand to someone;
- move a feed between nodes without replicating it over the network
  (USB stick, `scp`, an air-gapped machine);
- restore a feed onto a fresh node.

## The bundle

```
<dir>/manifest.json            what is in the bundle
<dir>/feeds/<hex>.jsonl.gz     one feed: a {"key","value","timestamp"}
                               record per line, sequence order, from 1
<dir>/blobs/<hex>              the blobs those messages reference
<dir>/index.html               a plain preview, readable without SSB
```

`<hex>` is the 64-char lowercase hex of the feed's public key or the
blob's sha256.

The feed lines are the signed messages exactly as stored, in the same
shape `createHistoryStream({keys: true})` returns, so any SSB tool can read
them. Nothing erlbutt-specific is in the bundle: the on-disk log framing
is dropped, and the archive `.hint` files are a local cache the importing
node rebuilds for itself.

`manifest.json` lists each feed (`id`, `from`, `to`, `latest` message id,
`sha256` of its file), each blob (`id`, `size`), the blobs that were
referenced but not held (`missingBlobs`), and any feed that could not be
exported (`refused`, with a reason).

The manifest deliberately does **not** list network keys. A private
network's key is an access credential, and a bundle is made to be handed
to someone.

## Export

```sh
sbutt export DIR                  # your own feed
sbutt export DIR @a... @b...      # these feeds instead
```

On a release install, run it as `bin/ssb escript sbutt.escript export ...`.
`DIR` must not exist yet. The bundle is built in `DIR.partial` and renamed
into place at the end, so `DIR` only ever appears complete.

`sbutt export` reads the store's files directly and does not talk to the
node. The logs are append-only and blobs are immutable, so it is safe to
run beside a live node, and a large export never blocks it. Set
`SSB_HOME` if you are not in the release directory (see
[ssb-conversion.md](ssb-conversion.md)).

**What is exported.** A feed is exported only if this node holds it
**from sequence 1, as one unbroken chain**. Archived segments are part
of the feed and are included. A feed held from a validation floor (a
suffix, see [archive-testing.md](archive-testing.md)) is refused and
listed under `refused`, and so is a feed this node does not hold. The
other feeds still export.

**Blobs.** Every blob referenced by an exported message that this node
holds is copied into the bundle, including blobs referenced from your
own private messages (found by decrypting them with your key). Referenced
blobs the node does not hold are listed under `missingBlobs`, not
fetched.

**Private messages** are exported as they sit in the feed: still
encrypted.

## Import

```sh
sbutt import DIR
```

Unlike export, import goes **through the running node**: new messages are
stored by the feed processes exactly like replicated ones (indexed,
journalled, offered onward by EBT). `DIR` must be readable by the node.
`sbutt` sends the absolute path, and the node and `sbutt` normally share a
machine.

A bundle is untrusted input. For every message:

- the signature must verify;
- the message id is **recomputed** from its value, and the bundle's
  `key` must match it;
- the author must be the feed the manifest names;
- sequence numbers must run from 1 without gaps;
- each `previous` must be the id of the message before it.

The feed file must also match the manifest's `sha256`. Where the bundle
overlaps what the node already holds, it must be the **same chain**: a
different message at a sequence we hold is reported as a **FORK**, and
nothing is stored.

File paths inside the bundle are derived from the ids. The manifest's
`file` fields are ignored, so a bundle cannot point the importer outside
its own directory.

Each blob must hash to its id.

### Reading the result

```
Imported from /media/usb/moid-2026-10-02
  @Sur8...ed25519  imported: +12603 (held 0)
  blobs: 266 stored, 0 already held, 0 missing, 0 BAD
```

| status       | meaning                                                   |
|--------------|-----------------------------------------------------------|
| `imported`   | new messages stored                                       |
| `up_to_date` | nothing in the bundle that the node did not already have  |
| `stopped`    | stored the messages that checked out, then hit a bad line |
| `refused`    | nothing stored; the reason says why                       |

**Stopping halfway is safe.** Each message is checked and stored in
order, so a bundle that goes bad at sequence 500 leaves the node holding
a valid 1..499: the same state an interrupted replication leaves. A
better bundle, or a peer, simply continues from there.

`sbutt import` exits **2** if any feed was refused or stopped or any blob
was bad, so it can gate a script.

## From the admin namespace

Both are owner-only muxrpc methods, for clients other than `sbutt`:

| method         | args                         | answers         |
|----------------|------------------------------|-----------------|
| `admin.export` | `[dir]` or `[dir, [feedId]]` | the manifest    |
| `admin.import` | `[dir]`                      | the report      |

`dir` is an **absolute path on the node's disk**. Both block the calling
connection until they finish. For a big export, prefer `sbutt export`,
which never goes through the node.

maxbutt has `M-x ssb-export` and `M-x ssb-import`. They call the same
handlers in-process, so the directory is likewise on the node's machine.

## Example: from the VPS to a laptop

```sh
# on the VPS
cd /opt/erlbutt && bin/ssb escript sbutt.escript export /tmp/moid-export
tar czf /tmp/moid-export.tgz -C /tmp moid-export

# on the laptop
scp pub.cmoid.org:/tmp/moid-export.tgz . && tar xzf moid-export.tgz
open moid-export/index.html          # look before you load
sbutt import moid-export
```

## Not yet

- **Floored feeds** can be neither exported nor imported. A full bundle
  is exactly what would fill in a floored feed's missing history, but
  that belongs to `archive_verify`'s seam check, not an append.
- **Imported feeds are not pinned or followed.** EBT replicates an
  imported feed onward only if it is already within the node's follow
  range.
- **The outer `timestamp`** on each line is the exporting node's
  receive time. It is unsigned, and import ignores it: imported messages
  are stamped with the time they were imported.
