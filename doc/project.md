Project notes
=============
These notes are a record of work on erlbutt, a prototype for exploring scuttlebutt.

Introduction
-------------

erlbutt will be something like scuttlebot(now known as [ssb-server][4]), though simpler. It's primary function is to act as a replicator of feeds. It's not aware of friends graphs or other application level extensions to the core protocol. It's told to replicate feeds and it tracks those feeds thereafter.

Anytime an erlbutt node becomes aware of another node or a ssb-server node on the same LAN, broadcasting a heartbeat over UDP, it will connect to it using secret handshake and attempt to update any and all feeds it is currently tracking. 

Originally my idea was to use this in healthcare apps, where the primary function for erlbutt is to act as an aggregator. I've given up on that grandiose scheme. However I'd still like to explore the foundations a bit and nothing beats code for that. It seems reasonable to allow it to be an ssb-server also and support enough of that API to be useful to support a tangle viewer.

Persistence
-----------

Scuttlebutt makes use of [flumedb][6] for persistence, which is not really a database, or as one programmer puts it, it's an [unusual][1] database. Each node in the P2P network has a feed associated with it, uniquely identified by a key-pair. Feeds are immutable and append-only, which makes replication a lot simpler. One node just tells another the current sequence number it is up to for a feed and the other returns the messages since that number. Each message is signed and contains a reference to the prior message so the feed can be validated.

A single database or log contains all the messages for all the feeds in the order they are received. If one views scuttlebutt as a global database then the union of all the feeds is the database. Everything else, including any and all indices, can be built from the  feeds. For example when a user follows another this becomes a message on the following user's feed so this follows relation can be computed. In fact the entire friends graph can be built incrementally for each node as messages are being replicated and fetched from other nodes. It does raise some interesting questions from a data perspective, particularly with regards to indexing as incremental information can impact an index quite a bit.

In much of the discussion the community has, one item that seems to be missing is dunbar's number. It seems the underlying assumption is that each peer will need to support web scale style databases in terms of computing indices. In practice though I don't think anyone needs access to all 15K feeds currently in the entire ecosystem. We only meaningfully interact with a few hundred at most, and at any point in time perhaps are only following 20 or 30 tangles of discussion. 

I spend far too much time in Patchwork pointlessly scrolling.

So I'm wondering about keeping the same log.offset file format, but having a separate file for each feed. Maybe a separate directory for each feed that includes a meta-level file of information about the feed. This meta-level file would capture things like the latest state of the user's follow graph, perhaps lists of ids that are roots of [tangles][3]. In the running peer each feed current in use could map to a `gen_server`, allowing Erlang's awesomeness to add value.

----
[0]: https://github.com/rebar/rebar3
[1]: https://viewer.scuttlebot.io/%25pYmFr6d0QwLP%2BYG0VNoo75PP7eYNZ1Y8C2MC9IjF5aw%3D.sha256
[2]: http://localhost:8989/blobs/get/&4DUnrqwI7xxUpP6omK1wiPSco5uLrNa6Ey7lNrXCzCU=.sha256
[3]: https://github.com/cn-uofbasel/ssbdrv/blob/master/doc/tangle.md
[4]: https://github.com/ssbc/ssb-server
[5]: https://cloud.google.com/healthcare/
[6]: https://github.com/flumedb/flumedb
