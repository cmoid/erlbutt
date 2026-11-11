# Git over SSB

*Peer-to-peer code collaboration that lives in the same social fabric as
everything else on the network — repos from people you follow, pushes as
signed messages, review as ordinary threads — without a forge in the
middle.*

This is a revival, not an invention. cel's git-ssb did all of this in
2016–17, and the network still carries its history. This document
describes `ssb_git`, an erlbutt plugin that rebuilds it: what git-ssb got
right and keeps, why the original stopped working, the message formats
both speak, and how the pieces fit, with git doing the git work and Emacs
as the forge.

The plugin lives in its own repository,
[`ssb_git`](https://github.com/cmoid/ssb_git); its README is the user's
guide. This is the design.

---

## The problem

Collaborating on code currently means an account on a commercial forge.
The forge holds the canonical copy of the refs, decides who may push,
owns the issue tracker, and can change its terms, its pricing, or its
mind. git itself is already distributed — every clone is a full replica —
but everything *around* git (who has what, what changed, what we are
discussing) is centralised, because git has no answer to "how do I find
your repo and hear about your pushes?"

SSB answers exactly that question, for everything else. A feed is a
signed append-only log; replication follows the social graph; content
too large for a message goes in a content-addressed blob. Commit history
is a signed append-only log of content-addressed objects. The fit is not
a coincidence, and cel saw it in 2016.

## git-ssb

Read from the source: `git-ssb` 2.2.1, `git-remote-ssb` 2.0.4,
`ssb-git-repo` 2.8.3 and `ssb-git` 0.5.0. These are the ideas that make
it work, and ssb_git keeps every one of them.

**git meets SSB through a remote helper.** For a URL with scheme `ssb`,
git runs a program named `git-remote-ssb` and speaks a line protocol to it
over stdin/stdout: *list your refs*, *fetch these objects*, *push these
refs*. The helper never touches the network; it asks the local node.
git is not patched or wrapped, so every tool built on git works
unchanged.

**The data is results, not commands.** Nothing is replayed. A push
publishes a statement of what it produced: the objects in a packfile
stored as a content-addressed blob, the new ref values in a signed
message. A git repository is objects plus refs, and this is that, spread
across signed feeds. The state of a repo is a **fold** over every update
that links to it.

**Updates form a DAG.** Each update names the updates its pusher had seen
(`repoBranch`, `refsBranch`), a tangle in later SSB vocabulary. Within one
feed the higher sequence wins, because a feed is totally ordered. Across
feeds, two updates to the same ref that do not see each other are
concurrent, and git-ssb does not pick a winner: both survive, the other
renamed to a **conflict branch** such as `refs/heads/%<update id>/master`,
and the next push on top of both resolves it.

**Anyone can push.** The owner check in `uploadPack` is commented out.
Combined with conflict branches, this means there is no upstream
authority at all: every pusher's view of the repo is published, and
divergence is visible rather than rejected. This is the most genuinely
peer-to-peer idea in the design.

**Issues and pull requests are messages that point at the repo**, their
discussion ordinary threads. A push whose commit says `fixes %<issue>`
closes the issue.

**A commit list in every push**, so a client can show what was pushed —
titles, parents, tags — without fetching a single pack.

### Why it stopped working

- **The JavaScript aged out.** Last release June 2017: `ssb-client@4`,
  `ssb-keys@7`, `new Buffer`, native modules of that era. It will not
  install on a current Node without pinning one from years ago.
- **It depended on server APIs that moved.** `sbot.links` was ssb-db's;
  ssb-db2 stacks dropped it.
- **It carried its own pack reader and writer** (`pull-git-pack` and
  friends), a large surface to keep alive.
- **Packs were never split.** One push, one pack, and ssb-blobs refuses a
  blob over 5 MB, so a real repository's first push could fail.
- **Blob availability.** An update replicates with the feed; its pack only
  when somebody wants it. With the pusher offline and nobody else holding
  the pack, a clone stopped at "Timed out waiting for blob".
- **The ecosystem went quiet.** SSB's own developers self-hosted on
  git-ssb early and later moved to GitHub.

None of these is a flaw in the model, and the first three disappear by
not writing it in JavaScript and by letting git handle packs.

## What is on the network

Counted on one well-connected node, October 2026:

| | |
|---|---|
| `git-repo` messages (repos created) | **1,728** |
| repos with at least one push | **1,472** |
| `git-update` messages (pushes) | **10,429** |
| `issue` / `pull-request` / `issue-edit` | 594 / 354 / 81 |
| distinct blobs referenced by pushes | **20,043** |
| of those, already on that node's disk | **19,908 (99.3%)**, 637 MB |
| largest pack | 5,052,284 bytes |

**The packs are already here.** Updates name their packs as ordinary blob
links, so a node's blob fetcher has been pulling them all along. Nearly
every public git-ssb repo in a node's replication range can be cloned from
its own disk, with no peer online.

**The largest pack is just under ssb-blobs' 5 MB cap.** Nobody has pushed
a pack over the limit, which is not the same as nobody needing to; the
cap shaped what got pushed.

**This history is the compatibility test.** ssb_git's fold, run over all
of it, produces the same refs, in the same order, with the same `HEAD`,
as git-ssb's own merge for every one of the 1,728 repos.

## Principles

- **No change to git.** It meets SSB through its own extension point.
- **No new message types.** `git-repo`, `git-update`, `issue`,
  `pull-request`, `issue-edit`: git-ssb's, read and written as they are.
  The one extension, chunked packs, is additive.
- **No change to replication.** Repos replicate because feeds and blobs
  replicate. Nothing here touches EBT.
- **No pack format of our own.** git produces and verifies packs; the
  helper calls it.
- **Not part of the node.** Most nodes will never host a repo, and a
  forge is exactly what a community should be able to fork, replace or
  leave out. It is a plugin.

## Message formats

All of these are public messages; the ids are ordinary SSB message (`%…`)
and blob (`&…`) ids.

### `git-repo`: a repository

```json
{ "type": "git-repo", "name": "ssb_git", "upstream": "%<repo it forks>.sha256" }
```

The message id **is** the repo, and its URL is `ssb://%<id>`. `name` and
`upstream` are optional. A fork inherits its upstream's refs under any
name it does not set itself.

A repo is named, or renamed, by an ordinary `about` message:

```json
{ "type": "about", "about": "%<repo>.sha256", "name": "ssb_git" }
```

Older `git ssb create` put the name only here, so it matters: 370 repos
on the network have one, 217 of them repos whose `git-repo` message has
no name.

### `git-update`: a push

```json
{
  "type": "git-update",
  "repo": "%<git-repo id>.sha256",
  "refsBranch": ["%<head update>.sha256"],
  "repoBranch": ["%<head update this push builds on>.sha256"],
  "packs":   [{ "link": "&<pack blob>.sha256", "size": 41283 }],
  "indexes": [{ "link": "&<idx blob>.sha256",  "size": 1156 }],
  "refs": { "refs/heads/master": "a1b2c3…", "refs/heads/old": null },
  "issues": [{ "link": "%<issue>.sha256", "open": false,
               "merged": true, "object": "a1b2c3…", "label": "…" }],
  "num_objects": 12,
  "commits": [{ "parents": ["…"], "title": "…", "body": "…", "sha1": "a1b2c3…" }],
  "commits_more": 132,
  "tags": [{ "object": "…", "type": "commit", "tag": "v1.0", "title": "…", "sha1": "…" }],
  "object_ids": ["…"]
}
```

- `refs` holds the new value of each ref the push moved; `null` deletes it.
- `packs` and `indexes` are parallel: the objects the repo lacked, and the
  index git-ssb readers use. A push that moves refs to objects the repo
  already has carries neither.
- `refsBranch` and `repoBranch` together name every head of the repo's
  update DAG the pusher had seen. `repoBranch` is the part the pushed
  objects depend on.
- `issues` closes what the push fixes or merges; see
  [State and authority](#state-and-authority).
- `commits`, `tags` and `object_ids` summarise the pack (commits newest
  first) so readers can show it without fetching. A message is at most
  8192 characters, so a large push lists only its newest commits and
  counts the rest in `commits_more` (`tags_more` likewise), and drops
  `object_ids` first.

The field order above is the one current git-ssb wrote; ssb_git keeps it.

**Chunked packs** are the one extension. A pack or index too big for one
blob is split into pieces, and the pack is their concatenation:

```json
"packs": [{ "link": "&<piece 1>", "size": 4194304,
            "chunks": ["&<piece 2>", "&<piece 3>"] }]
```

`link` and `size` describe the first piece, as a link's size always
describes the blob it links.

**Older shapes** a reader still meets: one update on the network writes
`packs: [{pack: {link, size}, idx: {link}}]` instead of parallel arrays,
and 22 of the earliest store each object as its own blob under
`objects.<sha1>`.

### `issue` and `pull-request`

```json
{ "type": "issue", "project": "%<repo>.sha256", "title": "…", "text": "…" }

{ "type": "pull-request", "repo": "%<repo>.sha256", "project": "%<repo>.sha256",
  "branch": "master", "head_repo": "%<repo with the branch>.sha256",
  "head_branch": "feature", "title": "…", "text": "…" }
```

`title` is optional, and most of the network's issues have none; a reader
uses the first line of `text`. ssb_git writes both: the title in `title`,
and again as the first line of `text`, because clients that render an
issue (silkpurse among them) show only its text. A pull request asks for `head_repo`'s
`head_branch` to be merged into `repo`'s `branch`.

### `issue-edit`: a change of state

```json
{ "type": "issue-edit", "root": "%<issue>.sha256",
  "issues": [{ "link": "%<issue>.sha256", "open": false, "title": "…" }] }
```

`open` closes or reopens, and `title` renames. The earliest clients wrote
a rename as `{type: issue-edit, issue: %<issue>, title}`.

### Comments

Ordinary posts in the issue's thread:

```json
{ "type": "post", "root": "%<issue>.sha256", "branch": ["…"], "text": "…",
  "issue": "%<issue>.sha256", "repo": "%<repo>.sha256" }
```

`issue` and `repo` let git-ssb-web file a comment without walking the
thread.

## Architecture

```
 working repo                                    erlbutt node
 ┌───────────────┐  stdin/stdout  ┌─────────────────┐  SHS + muxrpc  ┌───────────────────────┐
 │ git, or any   │ ─────────────► │ git-remote-ssb  │ ─────────────► │ ssb_git plugin        │
 │ git front end │  remote-helper │ (git starts it) │    as owner    │   views: repos, refs, │
 └───────────────┘    protocol    │ git pack-objects│                │   packs, issues       │
                                  │ git index-pack  │                │ + blobs, publish      │
                                  └─────────────────┘                └───────────────────────┘
                                                                         ▲
 Emacs (ssb-git.el on maxbutt) ── repos, issues, pull requests ──────────┘
                                  (Distel)
```

- **The node owns state and the network.** It folds updates into views,
  answers questions about them, stores and replicates the blobs, and
  publishes. It never needs git installed.
- **The helper owns git's object format**, because it is the process
  standing next to the working repository. It turns packs into objects
  and objects into packs by calling git.
- **Emacs is the forge.** Repos, issues and pull requests, which git has
  no commands for. Git itself is used however one likes: the command
  line, Emacs's built-in VC, or anything else.

### The node

An OTP application of `ssb_view`s over the node's store, each also an
`ssb_plugin` serving RPCs. Every table is derived and rebuildable from the
messages:

| table | holds | view |
|---|---|---|
| `git_repo` | repo id, author, name, upstream, time | `ssb_git_repos` |
| `git_object` | each `git-repo`/`git-update` as the fold's object | `ssb_git_refs` |
| `git_pack`, `git_pack_chunk` | each update's packs, indexes and pieces | `ssb_git_refs` |
| `git_issue`, `git_issue_edit` | issues and pull requests, and every change to them | `ssb_git_issues` |

**The fold** is a port of `ssb-git/merge.js` run the way `ssb-git-repo`
runs it, quirks included, because 10,429 real updates mean whatever that
code made of them. An object links to every other object whose id appears
anywhere in it, so `repo`, `repoBranch`, `refsBranch` and even `previous`
are all edges. A state summarises everything above a message: per ref,
per author, the highest sequence wins; one author's value is the ref;
several authors' values become conflict branches. At the top, a conflict
whose plain name ended up empty has one branch promoted, preferring an
update that set `repoBranch` and then the latest time. Order matters
(JavaScript object key order decided ties in the original), so objects
are kept in ordered lists, not maps.

The fold runs per request rather than being stored. A repo's updates are
tens or hundreds of rows, and folding on read means there is no derived
ref state to go stale when an old update arrives late, which on SSB they
do.

The fold orders updates by their claimed time, as git-ssb did, since
agreeing with it depends on that. Everything else that sorts by time
(repos, issues, their changes) uses `min(claimed, received)`: an author's
clock is unvalidated, the node's receipt time is not.

RPCs, all owner-only:

| method | returns |
|---|---|
| `git.repos({author?})` | repos, oldest first |
| `git.refs({repo})` | the folded refs and `HEAD`, a fork inheriting its upstream's |
| `git.packs({repo})` | every pack the repo's (and its upstreams') updates name, oldest first |
| `git.updates({repo})` | the repo's update graph: each object's links and published refs |
| `git.issues({repo, kind?, state?})` | issues and/or pull requests, newest first |
| `git.issue({id})` | one, with every change to it, each marked counted or not |

Emacs reaches the node over Erlang distribution and calls the same reads
directly, through `ssb_git_emacs`, which returns maps Distel carries well.

### The remote helper

One program, `bin/git-ssb`, run under the two names git looks for:
`git-remote-ssb` for `ssb://` URLs, and `git-ssb`, which git runs for
`git ssb …`. It picks its command by the name it was run as, the same
trick git-ssb's `bin.js` used. It is a shell launcher, not an escript: it
runs the erlbutt release's own `erl` (booted with relx's
`no_dot_erlang.boot`, a plain VM, not the node) with the release's beams,
so the client is always the node's own version and finds the node's
identity and data exactly as the node does. It connects as the node's
owner over SHS.

The helper's capabilities are `list`, `list for-push`, `fetch`, `push` and
`option`. One protocol detail is easy to get wrong: an `option` reply is a
single line with no blank line after it, unlike every other reply.

**Fetch.** `list` answers from `git.refs`. A fetch batch asks `git.packs`,
skips the packs this clone has already absorbed (recorded by blob id in
`.git/ssb/packs`), and checks the rest with `blobs.has`. Packs the node
lacks are asked of its peers with `blobs.want` and waited for. Each pack
is fetched, checked against its blob id (an error frame from `blobs.get`
arrives looking like data), and fed to `git index-pack --stdin
--fix-thin`. index-pack recomputes every object hash, so a pack is trusted
exactly as far as git trusts any pack, and the published `.idx` is never
read. A pack index-pack refuses is retried after the others, in case it
is thin and its bases came later. If the record of absorbed packs leaves
a wanted object missing (a gc can remove objects), every pack is absorbed
again before the fetch gives up.

**Push.** One update for the whole batch, so the refs land or fail
together:

1. resolve each source here; an empty source deletes the ref;
2. pack `rev-list --objects <new> --not <every hash the repo has
   published that this clone holds>` with `git pack-objects`, non-thin so
   a reader never needs a base it lacks, and index it with
   `git index-pack` for the readers that use `.idx`;
3. store the blobs (`blobs.add`, skipped for any the node has) and
   announce them (`blobs.push`);
4. link the update into the graph, and publish.

The helper, not the node, builds the links. `git.updates` reports the
repo's objects and their links; the heads are the objects nothing links
to, and `refsBranch` + `repoBranch` name all of them, so the new update
sits on top of everything this node has seen. `repoBranch` is the subset
that published, as a ref value, a commit this push builds on (a parent of
a pushed commit, a pushed tag's target, or a ref moved to an existing
commit), reduced to those no other such update links to. git-ssb found
dependencies by walking every object's parents, trees and blobs; this
approximation needs no extra data and changes nothing the fold decides,
since the fold counts every link alike.

**Conflict branches** that a push would make disappear are written into
the update with their current values, as git-ssb did, so no commit is
lost; one the push deletes along with its plain name is left out.

**Non-fast-forward pushes are not refused.** They are published, and if
they conflict with someone else's view, the fold shows both. A `--force`
from git means "I know"; the message is the same either way.

**Size.** A message is at most 8192 characters (ssb-validate's
`JSON.stringify(value, null, 2).length`). The node refuses to sign a
longer one, since every other implementation would reject it and every
later message in the feed, and answers with ssb-validate's wording. On
that, the helper drops `object_ids`, then halves the commit list until
the update fits. git-ssb did it the other way round, moving each cut
commit's id into `object_ids`, which refilled the room the cut had made.

### Large packs

The cap is a blob cap, not a pack cap, so the bytes are split, not the
objects. A pack or index over 5 MB is stored in 4 MB pieces, published as
[chunks](#git-update-a-push), and joined by the reader before index-pack,
whose trailing pack checksum catches a wrong order or a missing piece.
Each piece is a blob like any other: a node's blob fetcher wants every
blob id anywhere in a message, and goes by a peer's reported size, never
the message's, so the pieces replicate with no change to anything.

The alternative, splitting the object set into several self-contained
packs, needs a planner nobody should have to own. The cost is that an old
git-ssb reader takes the first piece for a short pack and fails, loudly,
at index-pack. Only initial imports of long histories should ever produce
one; ordinary pushes are kilobytes.

### Pack availability

The pusher's node holds its own packs and serves them, for free, since
blobs it added are local. The harder case is the pusher being offline.
A node that holds a repo's updates already wants their packs, because the
blob fetcher wants every blob a message names, so a repo is as available
as its most-connected follower. Pubs and rooms should go further and keep
`git-update` blobs whatever their size policy for other blobs: a pack is
the clearest case there is of a blob that is useless alone.

### Emacs

`ssb-git.el` ships with the plugin and runs on maxbutt: the same Distel
connection to the same node, maxbutt's feed and thread views, and
`ssb_git_emacs` inside the node. It does only what git has no word for:

- a list of every repo the node knows, most recently active first, with
  open issue and pull request counts;
- a repo: its branches, conflict branches flagged with the name they
  compete for, and its recent pushes summarised from their `commits`
  lists without fetching anything;
- a repo's issues or pull requests, open, closed or all;
- an issue or pull request: its text, every change to its state (marked
  when it does not count), and its comments inline; comment, close or
  reopen, and check out a pull request (its head branch fetched as
  `pr/<id>`);
- filing an issue or a pull request.

It runs three git commands, all for convenience: `clone`, and the `fetch`
and `checkout` of a pull request. Everything else about the repository is
git's, used however one prefers.

## State and authority

**Whose change counts is the reader's policy.** Anyone can publish a close
of anyone's issue. ssb_git counts state changes from the issue's author
and from the owner of the repo it is filed against (the `git-repo`
message's author); among those, the latest wins. Others' changes are kept
and shown, marked as not counted. On one node in October 2026, 278 of the
306 state changes naming a known issue counted; the other 28 were third
parties'.

A repo's name follows the same rule with one authority, its author: the
latest `about` name in the author's feed, by sequence rather than
timestamp, else the `git-repo` message's. Of 389 names given to repos,
331 were their authors'. The other 58 are kept and shown as someone
else's, standing in for a name only for a repo its author never named;
31 repos have no other.

A push closes what it fixes, as in git-ssb: a commit it adds to the repo's
`HEAD` branch whose message says `close`, `closes`, `closed`, `closing`,
`fix`, `fixes`, `fixed` or `fixing`, optionally `issue`, then an issue's
id, closes that issue. A pull request whose head branch tip is among
those commits is closed as merged. Whether readers take the close follows
the same rule as any other.

**Refs have no authority at all.** Anyone can push, and git-ssb chose the
plain branch among concurrent values by a timestamp heuristic, which is a
guess. The natural refinement is for a `git-repo` message, or a later
update by its author, to name **maintainers**. A reader that honours it
presents the newest maintainer-reachable value of a ref as the plain
branch and every other pusher's divergent value as a conflict branch,
which is what they already are. Nobody is refused; the default is chosen
by the repo's author rather than by a clock, and a reader that ignores
the field sees git-ssb's behaviour. As with issue state and archive
floors, the field is a signed statement of intent, not an access
control.

## Packaging as a plugin

ssb_git is built in its own repository against erlbutt's `ssb`
application, the way an sbot plugin was built against ssb-server, and
finding every place the plugin boundary was not really a boundary was
part of the point.

**What a plugin is.** An ordinary OTP application depending only on
`ssb`. Its views register with the view manager, which treats a late
arrival as the normal case: a view with no checkpoint is reset and folded
from the whole store, in chunks, while ingest carries on. Its RPC modules
register with the plugin registry, which serves their methods in the
manifest with their permission, so a muxrpc client cannot tell a plugin
method from a built-in one.

**How the node finds one.** `plugin_loader`, the last child of the `ssb`
supervisor, looks in `<data>/plugins/` and then `<release>/plugins/` (the
first wins, so a copy dropped into the data directory replaces one the
package shipped and survives upgrades), and starts what
`{plugins, all | [App]}` in `ssb.cfg` names: `all` by default, `[]` for
none. For each: the API check on the unloaded beams, the `ebin` on the
code path, every module the `.app` lists loaded by hand (an erlbutt
release boots in embedded mode, which loads nothing on demand), then the
application started as temporary. A plugin that fails at any step is
logged and skipped; nothing a plugin does can keep the node from coming
up, or take it down later. erlbutt's packages ship ssb_git this way,
built from a checkout beside the erlbutt tree, so leaving it out is one
line of configuration rather than a different build.

**The API version.** A plugin compiles erlbutt's record layouts into its
own beams, so a changed `#message{}` would fail at runtime far from the
cause. `ssb.hrl` stamps every module that includes it with the plugin API
version it was compiled against, and both registries, and the plugin's own
start, refuse a mismatch with a message naming both versions. Only
`#message{}` is pinned; the header's other records are connection
internals.

**What a plugin may not do.** Bring its own NIF: SQLite comes through
`ssb_store`, sharing the node's database and its transaction discipline.
Serve duplex or sink methods: the registry's one-call-one-result dispatch
cannot host them (ssb_git needs neither). And it cannot be unloaded from a
running node: the view manager has no way to forget a view, so removing a
plugin takes a restart, and its tables stay in the store.

**Trust.** A plugin runs inside the node's VM. It can read the secret
key, publish as you, and stop the node. Installing one is installing
code, exactly as an sbot plugin from npm was, and nothing here is a
sandbox. For that reason, and because a beam is tied to an OTP release,
plugins are distributed as source built against a tagged erlbutt rather
than as prebuilt beams.

## Compatibility

ssb_git reads every git-ssb repo on the network and writes what git-ssb
clients read, with four differences:

- **chunked packs**, which old readers cannot reassemble;
- **`repoBranch`** computed from published ref values rather than a walk
  over every object (the union with `refsBranch`, which is what the fold
  uses, is the same);
- **shrinking** keeps the newest commits and drops `object_ids`, where
  git-ssb kept `object_ids` and could end up with neither;
- **whose close counts**, which is a reader's choice in either.

A stock `git-remote-ssb` pointed at erlbutt would also need `links` and
`blobs.size`, which the core does not serve; both would be shims
registered by ssb_git over `ssb_links` and `blobs:size_of/1`, keeping
anything git-ssb-specific out of the core.

## What it costs

**A push is forever.** It is a message in your feed. A force-push does not
remove the old commits; it adds a statement that the ref moved. A
committed secret is published for as long as anyone holds the pack, and
nothing here should make that look like GitHub, where deletion is merely
expensive.

**Repos are visible only within your replication range.** You see the
repos of people you replicate, and nobody else's. For collaboration
inside a community this is the feature: a repo travels the same paths as
the conversation about it. For publishing software to strangers it is a
limitation, and [feed glimpses](feed-glimpses.md) are the natural place
for "this feed has repos" to show up at the boundary.

**Feed weight.** Each push is one message plus blobs that only the
interested fetch. 10,429 pushes across the network is a rounding error
against millions of messages.

## Prior art

- **git-ssb.** Everything above. The model is cel's; this design mostly
  argues for keeping it.
- **Radicle** (Rust, "heartwood"). The active project in this space: P2P
  gossip of git repos, with issues and patches stored as signed objects
  inside git itself. If the goal were only P2P git, it is the pragmatic
  answer. The case for SSB is that collaboration then shares a social
  graph, an identity and a client with everything else.
- **git-bug.** Issues stored in git refs; proof that the tracker can
  travel with the repo.
- **Email patches** (sourcehut, the kernel). The original decentralised
  forge, and the reason Emacs is already good at reviewing code it did
  not fetch from a website.

## Future work

- **Maintainers**, as described under [State and authority](#state-and-authority).
- **Deliberate pack retention** on pubs and rooms.
- **From a commit to its push.** A command that takes a commit id, from
  any log, and opens the `git-update` that introduced it, so a push is a
  message one can discuss.
- **Managing plugins at runtime:** `sbutt plugin add | remove | list` as
  owner-only admin methods, and a view manager that can unregister a view.
- **Private repos.** git-ssb supported `recps` and blobs encrypted by
  their plaintext hash. erlbutt can decrypt private messages; whether it
  should carry convergently encrypted blobs, which leak equality, is a
  separate question.

Not planned: a backend for Magit's forge package. Its model assumes a
central API, issue numbers and permissions, none of which SSB has, and
the Emacs side is meant to stay small.

## Open questions

1. **Is the fold right?** It agrees with git-ssb on every repo on the
   network, which proves agreement, not correctness. git-ssb's merge has a
   timestamp tiebreak and a special case for a conflict branch with no
   base; whether its output on real history is what those repos' authors
   meant is not something a test can say.
2. **Is chunking the right fix for the blob cap,** or should nodes accept
   larger blobs for packs? The latter is a one-line change in one
   implementation, and breaks every other implementation's replication of
   them.
3. **How much of the plugin API to promise.** Today it is two behaviours,
   two registration functions, `ssb_store`, a handful of `ssb_feed` and
   `blobs` functions, and `#message{}`. Whether records belong in it at
   all, or should give way to accessors and maps so the API version rarely
   moves, is open.

---

*Charles Moid — erlbutt, October 2026. Sources read: `git-ssb` 2.2.1, `git-remote-ssb`
2.0.4, `ssb-git-repo` 2.8.3 and `ssb-git` 0.5.0 as published on npm.
Counts are from one node's store in October 2026.*
