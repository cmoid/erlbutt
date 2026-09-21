# Feed glimpses

*A way to know enough about a feed at the edge of your follow graph to
decide whether you want it, without replicating it first.*

**Status: implemented, September 2026.** Built in a day against this
document, which is the case for writing the thing down first. What
follows is the design as written, with `Built:` notes where the
implementation went somewhere the design did not, and a revised set of
open questions at the end — three of the five were answered by building
it, and not always in the direction I expected.

Running on two nodes; not yet on the public pub. The modules are
[`ssb_glimpses`](../../apps/ssb/src/ssb_glimpses.erl),
[`glimpse_discovery`](../../apps/ssb/src/glimpse_discovery.erl),
[`feed_pins`](../../apps/ssb/src/feed_pins.erl) and
[`silkpurse_glimpses`](../../apps/silkpurse/src/silkpurse_glimpses.erl);
the two-node procedure is [glimpse
testing](../ops/glimpse-testing.md).

---

## The problem

Replication is bounded by a hop count. Mine is 2. The set is

```
({self} ∪ follows(self, hops) ∪ room_members) − blocks(self)
```

which is a reasonable rule and produces an unreasonable result, because
social graphs are densely connected locally. On my own node that has meant
**over 43,000 feeds and around 2 million messages**, the overwhelming
majority of which I will never read. Dunbar's number is somewhere around
150. The gap between 150 and 30,000 is not a tuning problem, it is a
category error: hop count is a proxy for interest, and it is a bad one.

Lowering the hop count does not fix it either. At hops=1 you see only
people you already chose, and the network stops being a way to discover
anyone. The hop count is doing two jobs at once — deciding *what to
replicate* and deciding *what exists* — and it is only any good at the
second.

So: keep the graph wide, make replication narrow, and put something in
between. At the boundary of the replicated region, instead of a whole feed
or nothing at all, fetch a small signed artifact that says who this is.
Enough to decide. The user promotes a feed to full replication when the
glimpse earns it.

This is the idea from the dev diary entries of
[2026-08-16](../dev-diary/08-16-2026.md) and
[2026-08-24](../dev-diary/08-24-2026.md), written up properly.

## What this deliberately does not change

Same discipline as archive boundaries, and for the same reason — the value
of the idea is inversely proportional to how much it disturbs.

- **No change to message format.** A glimpse is an ordinary signed message
  in the author's own feed, of a new `type`. Clients that do not know the
  type ignore it, as they already ignore dozens of types.
- **No change to validation.** Nothing here asks anyone to accept a
  message they could not already verify.
- **No change to EBT.** Glimpses do not ride the replication protocol at
  all; see [Why this cannot ride EBT](#why-this-cannot-ride-ebt).
- **No new trust primitive.** Nothing is asserted by anyone about anyone
  else. See the next section, which is the part I most want checked.

## Author-curated, not peer-attested

The question I sat on longest was whether a glimpse should be written by
the author about themselves, or assembled by peers about the author. The
second is more powerful and I am not going to build it.

Peer attestation means a peer telling me something *about* a third party,
which means I need a reason to believe that peer, which means a trust
metric, which means propagating trust through a social graph. Declaring
that I trust an institution is a normal thing to do. Declaring that I
trust Sally more than Bob is a different kind of statement, and building a
protocol that requires people to make it is a large social commitment
dressed up as an engineering decision. I would rather not.

Author-curated has an obvious objection — the author can lie — and a
sufficient answer: **so can their feed.** A glimpse is not evidence about
a person, it is a self-description, exactly like the `about` messages
every client already renders as a profile. Nobody believes a profile is
attested. The glimpse only has to be as trustworthy as the thing it
summarises.

### The dichotomy is looser than it looks

Splitting two things dissolves most of the worry:

- **Content** — what the glimpse says. Author-curated, signed by the
  author.
- **Carriage** — how it reaches a node that does not replicate the feed.
  Necessarily peer-mediated.

The trust question only arises when a peer *asserts something about* the
author. Relaying a signed artifact is not that. It is handing over a
sealed envelope: the courier can decline to carry it, or carry an old one,
but cannot write it.

erlbutt already relies on exactly this split, and says so in
[`boundary_discovery.erl`](../../apps/ssb/src/boundary_discovery.erl):

> NOTHING HERE TRUSTS A PEER. Every offer is a message signed by the
> feed's own author, verified before it is looked at. A hostile peer can
> withhold boundaries (we replicate from the beginning, as we would have
> anyway) or offer an older one (we skip less) — neither is an attack.

Glimpses are the same pattern pointed at a different question. If that
paragraph is sound for boundaries, it is sound here.

## The glimpse message

Structurally identical to an archive genesis: a signed message that names
a blob.

```json
{
  "type": "glimpse",
  "blob": "&....sha256",
  "size": 41283,
  "updated": 1788233201436
}
```

The blob holds whatever the author chooses to be represented by — the
current profile (name, description, image), a handful of posts they
consider representative, a channel list, a statement of what the feed is
for. **The contents are deliberately unspecified here.** That is the
author's editorial decision, and a protocol that dictates it is a protocol
that will be wrong for somebody.

> **Built: the profile and a statement, and nothing the node worked out
> for itself.**
>
> The first implementation did assemble a summary — five recent thread
> roots and twelve subscribed channels, on top of the profile — and it
> was wrong in two different ways, which is how I know the second way is
> the one that matters.
>
> The lists were bad. On a real feed the channels *subscribed to* and the
> channels *posted in* had **no overlap at all**, and the subscriptions
> were years stale; a stranger was being told about reading habits from
> 2019 and shown writing from last week, with nothing to distinguish
> them. That is fixable by picking better queries.
>
> The mechanism was worse than either list, and is not. A summary
> assembled out of somebody's behaviour and printed in their voice is not
> a self-description — and a reader cannot tell which parts of it the
> author chose. My own renderer put the heading *"From this feed"* above
> three posts that the *node* had selected, which reads as curation and
> was not. The paragraph above says a protocol that dictates the contents
> will be wrong for somebody; I had written a client that dictated them
> and got it wrong for the first author who looked.
>
> So: profile (from `about`, which the author wrote) plus a statement
> (which the author typed), and an author who wants to feature a post can
> quote it in the statement, where everything is theirs. A reader ignores
> payload fields it does not know, so another implementation is free to
> disagree.
>
> The size bound is a convention on the **reading** side —
> `?GLIMPSE_MAX_SIZE`, 256KB, above which a receiver declines the offer.
> The publisher warns and publishes anyway rather than refusing or
> truncating: those are the author's words, and a node deciding what a
> person may say about themselves is the thing this section just stopped
> doing.

The message is small and the blob is bounded by convention rather than by
rule. If glimpses become large enough to matter, the design has failed and
the correct response is to replicate the feed.

Why a blob rather than inline content: the same reason archives use one.
The message stays small enough to relay cheaply in bulk, the payload is
content-addressed and deduplicated, and a node can decide to take the
pointer without taking the payload.

## Why fetching one is not a chicken-and-egg

This looked like the hard part and I think it is not, because of a
property SSB already has:

**A feed id is an ed25519 public key.**

So a glimpse needs no context whatsoever to verify. A peer hands over the
whole signed message; the name of the thing it claims to be from *is* the
key that validates it. No feed, no chain, no predecessor, no prior state.

The flow is therefore not "fetch a message out of a feed I do not have" —
which would be genuinely awkward — but "accept some bytes and check them
against a key I already hold, because the key is the identifier I was
asking about in the first place."

### Why `ssb-ooo` does not apply

I assumed this would need something like
[`ssb-ooo`](https://github.com/ssbc/ssb-ooo). Having read it, I think it
solves a genuinely different problem, and the difference is worth stating
because it looks like the obvious tool right up until you check the key
it is indexed by.

`ssb-ooo` answers **"fetch me this specific message, by id."** Its
manifest is small but it is real — `{stream: 'duplex', get: 'async', help:
'sync'}` — a gossip-query stream between peers, plus two checks on
anything received: the computed hash must equal the requested id, and
`checkInvalidOOO` enforces sequence-ordering constraints.

Its trust model is the interesting part, and it is not the one I had
assumed. From the README:

> the signature on the ooo message doesn't really matter, what matters is
> that the signature on your friends message signs the hash of the ooo
> message.

That is **hash-anchored social proof**. You are entitled to the message
because a feed you already replicate and have chain-validated references
its hash, and a hash reference cannot be forged. The out-of-order
message's own signature is close to incidental.

This is a stronger guarantee than a glimpse gets, and it is unavailable to
us — for the reason that defines the whole feature. `ssb-ooo` requires you
to **already know the message id**, and you only know it because something
you replicate cited it. A glimpse is wanted precisely for a feed that
nothing you replicate has ever cited. There is no anchor, because if there
were an anchor you would already have a reason to be interested and would
not need a glimpse.

The keys do not match either. `ssb-ooo` is indexed **by message id**;
glimpse discovery has to be indexed **by feed id** — "what does this feed
say about itself" is not a question you can ask by hash, because you do
not know the hash of a message you have never heard of.

So the two designs anchor differently, and the contrast is the clearest
way to state what a glimpse actually gives you:

| | `ssb-ooo` | glimpse |
|---|---|---|
| Asked by | message id | feed id |
| Anchored by | a hash reference in a feed you validate | nothing |
| Proves | this exact message existed and a friend saw it | the author wrote *a* glimpse at some point |
| Requires | prior citation | nothing |

The glimpse's weakness is now explicit rather than glossed: **nothing pins
which glimpse, or when.** A peer can serve any glimpse the author ever
signed and I cannot tell it is not the current one. That is the staleness
cost in [What it costs](#staleness), and `ssb-ooo` is the demonstration
that a stronger primitive exists whenever an anchor happens to.

I still think unanchored is acceptable here, for one specific reason —
**a glimpse is advisory, not authoritative.** It feeds exactly one
decision, "replicate this feed or do not", and that decision is
reversible. I am not acting on the glimpse's claims; I am deciding whether
they are interesting enough to go and check properly. Full chain
validation happens if and when I say yes, at which point it is the
ordinary path with nothing special about it. A stale self-description
costs me a replication I might not have chosen, and the remedy is to stop
replicating.

What *is* worth borrowing from `ssb-ooo` is the transport shape. A duplex
query stream between peers is exactly the form the glimpse round needs,
and it is precedent that adding one is acceptable practice rather than a
protocol imposition.

## Discovery

A round, not a connect hook — for the reasons `boundary_discovery` already
gives, which apply unchanged. Periodically ask every connected peer which
glimpses it holds for feeds in my **boundary set**: feeds reachable at
hops+1, which I have deliberately chosen not to replicate. Stage the
answers for the length of the round, then decide.

The graph decides *candidacy*; the glimpse decides *promotion*. That
ordering matters and is not decorative — see [the spam
vector](#the-spam-vector).

Where boundary discovery takes the **lowest** offer, glimpse discovery
takes the **highest sequence** — the newest self-description on offer.
Same staging logic, opposite end.

> **Built: the peer volunteers, the receiver filters.** Nobody names a
> feed on the wire.
>
> "Ask which glimpses it holds *for feeds in my boundary set*" assumes
> the boundary set is something you can put in a question. It is not: on
> a node replicating 2,596 feeds at hops=2, the set at hops+1 is **36,112
> feeds**, and that is a laptop, not a pub. Sending tens of thousands of
> ids per peer per round, to hear "no" about nearly all of them, is the
> want storm `archives.boundaries` was shaped to avoid.
>
> So `glimpses.offers` is a bare source, exactly like the boundary list:
> the peer streams what it holds, and the receiver drops everything
> outside its own boundary set on arrival. While glimpses are rare the
> stream is short. If they ever become universal this needs revisiting —
> but the candidacy rule, and therefore the spam defence, is unchanged
> either way, because it was always the *receiver's* test.
>
> One thing the design did not anticipate, and the implementation had to
> decide: **we serve the view, not the edge.** A node offers glimpses for
> feeds it actually replicates — chain-validated, the sealed-envelope
> case this document argues for. It does *not* relay glimpses it merely
> staged for its own use. Doing so would propagate self-descriptions for
> feeds nobody at either end carries, which is how a bounded feature
> turns into a gossip network of its own. A feed's glimpse reaches a new
> node when somebody who actually replicates that feed offers it.

## Why this cannot ride EBT

EBT clocks express a **prefix**: "I have feed X up to sequence N". A
glimpse is a **selection** — one message out of a feed I otherwise hold
nothing of — and there is no way to say that in a vector clock.

This is the same wall documented in [archive
boundaries](archive-boundaries.md#a-floored-peer-is-a-partial-replica-and-ebt-cannot-say-so),
where a floored peer holds a suffix and cannot advertise it. Prefix,
suffix, selection: the clock can only say one of the three, and it is the
one neither feature needs.

I take this as good news rather than bad. It means glimpses need their own
small RPC and change nothing about replication — additive, ignorable by
anyone who does not implement it, and incapable of corrupting the
replication path if it is wrong.

## Where a glimpse lives

The one real structural difference from boundary offers, and the place I
expect the implementation to be least obvious.

A boundary offer concerns a feed that is *already being replicated*; it
only says where to start. A glimpse concerns a feed deliberately **not**
replicated. There is no `ssb_feed` process for it, `mess_auth` has never
seen its author, and creating either would quietly pull the feed into the
machinery that exists to replicate feeds.

So glimpses need their own small store — a view keyed by feed id, holding
the glimpse message, its sequence, and the blob reference. Explicitly not
the feed store. Promotion to full replication then means adding the feed
to the replication set and letting the ordinary path run from scratch; the
glimpse is discarded, having done its job.

> **Built: two tables, and only one of them is a view.**
>
> `glimpses` is an ordinary core view: the glimpse of every feed we
> replicate, folded out of the log, rebuildable at any time, and the only
> thing we serve. `glimpse_edge` holds what peers offered for feeds at
> our boundary — and a rebuild would erase those rows with no way to
> recreate them, since nothing in the log mentions those feeds. So
> `view_reset/0` clears the first and leaves the second alone. Treating
> the edge table as a view would silently lose it on every schema bump.
>
> Losing it is survivable, which is the nicer half of this. The rows come
> back from the next round, and that turns out to be load-bearing for
> promotion: `promote` forgets the staged glimpse, and if you later
> change your mind and unpin, the feed returns to the boundary and the
> next round re-stages it. The stand-in is disposable state that rebuilds
> itself from the network.
>
> **Promotion is a local pin, not a follow.** The design says "adding the
> feed to the replication set" without saying how, and the obvious answer
> — publish a `contact` — is wrong: following is a public statement about
> a stranger, and un-following is a second one. The whole argument that a
> glimpse is *advisory* rests on the decision being cheap in both
> directions. So `feed_pins` is a private table unioned into the
> replication set beside follows and room members, undone by deleting a
> row, and a block still outranks it. Following is the right thing to do
> once a feed has earned it; a pin is what you do while finding out.
>
> Unpinning stops *asking* for a feed; it does not delete what already
> arrived. Conflating the two would make "try this feed for a week"
> quietly destructive.

## What it costs

### The spam vector

A hostile peer can offer glimpses engineered to look interesting, to
induce me into replicating junk. This is the one genuinely new attack
surface and it is why candidacy is decided by the graph rather than by the
offer: a glimpse for a feed nobody I follow follows is never considered at
all. The peer can make a boundary feed look better than it is; it cannot
introduce a feed. In other words, "we don't want nobody that nobody sent".

That reduces the attack to "someone at hops+1 writes a flattering
self-description", which is not a protocol problem. It is a person being
uninteresting, and the remedy is unfollowing.

### Staleness

I cannot know that a peer showed me the newest glimpse. Taking the highest
sequence across a round narrows it; nothing closes it. This degrades the
same way withheld boundaries do — I see less than I could have, which is
what I would have seen anyway without the feature.

### Curation asymmetry

A glimpse is only as good as the author's willingness to write one. Feeds
whose authors never publish one are invisible at the boundary, which is
exactly their status today, so nothing regresses. But it does mean the
feature rewards people who tend their self-description, and that is a
value judgement baked into a protocol. I am not sure how I feel about it.

## Relationship to trust metrics

`boundary_discovery` contains this line:

> This is the hop count standing in for a trust metric — when one exists,
> it replaces this test and nothing else here changes.

Glimpses are how a user supplies that metric **by hand**, one feed at a
time, until something automatic exists. Every promotion is a human saying
"this one is worth it", which is the training signal any later trust
metric would want anyway. If something like appleseed with per-context
weights ever lands (the idea in the
[2025-02-21](../dev-diary/02-21-2025.md) entry), glimpses become the thing
it ranks rather than something it replaces.

## Open questions

Three of the five below were answered by building it. The two that
remain are the two I most wanted argued with, which is either a good sign
or a sign that they are the ones a prototype cannot settle.

1. **Is author-curated actually sufficient?** *(still open.)* I have
   argued a glimpse need only be as trustworthy as a profile. If there is
   a case where that reasoning fails — where a self-description does real
   damage that an `about` message could not — I want to know.

   Building it sharpened the question rather than answering it. The
   payload shrank to profile-plus-statement precisely because everything
   else was the node speaking in the author's voice; what is left is as
   trustworthy as an `about`, because most of it *is* one. That makes the
   claim narrower and easier to defend, and it does not test it.

2. **Is "advisory, not authoritative" doing too much work?** *(still
   open, and now costed.)* It is the load-bearing claim that lets me
   accept an unanchored message. It holds only if promotion is genuinely
   cheap and reversible — and that part I *have* now costed: promotion is
   one row in a private table, reversal is deleting it, neither is
   published, and nothing is destroyed on the way back. Cheaper than I
   assumed when I wrote this.

   What remains open is whether cheap reversal is *sufficient*, which is
   a question about people rather than rows. `ssb-ooo` still sharpens it:
   an anchored alternative exists wherever a validated feed happens to
   cite the message, and I still cannot find a natural anchor for a
   stranger's self-description, which is an absence of imagination rather
   than a proof.

3. ~~**Does the boundary set need to be hops+1 exactly?**~~ *Dissolved.*
   The set is never enumerated on the wire — the peer volunteers what it
   holds and the receiver filters — so its size costs a membership test
   and nothing else. It is large (36,112 feeds against a 2,596-feed
   replication set) and that turns out not to matter. If glimpses ever
   become common enough that the offered stream is the expensive half,
   this comes back as a different question: how to bound what a peer
   *offers*, not what a receiver *considers*.

4. ~~**Should a glimpse be a single message or a mutable pointer?**~~
   *Settled: a message, newest wins.* Both stores keep one row per feed
   and take the highest sequence, so a republished glimpse supersedes its
   predecessor everywhere that matters. The feed does accumulate
   glimpses, as predicted, and the irony stands — but they are small, and
   a mutable pointer would have needed a second source of truth for
   something the feed already holds.

   Worth stating plainly, because it is the cost of that choice: **a
   glimpse can be superseded but not retracted.** The old message stays
   on the feed and the old blob stays fetchable, so a peer holding it can
   still serve it. Both ends prefer the newest; neither can guarantee it.
   That is the staleness hole in [What it costs](#staleness), seen from
   the author's side.

5. **Is this worth specifying, or is it an erlbutt-local feature?**
   *(still open, but the shape of an answer is clearer.)* Everything here
   is additive: one message type, one source method, no change to
   replication, and a payload whose fields a reader is expected to ignore
   when it does not know them. There is very little to specify and a lot
   to simply do, which argues for "keep it local, describe it honestly,
   and see whether anybody copies it."

---

*Charles Moid — erlbutt. Implemented September 2026; see the `Built:`
notes above for where the implementation diverged. The pattern this
follows is
[boundary_discovery.erl](https://github.com/cmoid/erlbutt/blob/main/apps/ssb/src/boundary_discovery.erl);
the companion design is
[archive-boundaries.md](https://github.com/cmoid/erlbutt/blob/main/doc/research/archive-boundaries.md).*
