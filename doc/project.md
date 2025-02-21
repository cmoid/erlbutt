Project notes
=============
These notes are random thoughts about design I'm having while prototyping. I've been following the scuttlebutt community for 7 years now, and playing with some of the implementation details. Erlbutt wants to be something like an [`ssb-server`][4]. It implements the original core of the scuttlebutt [protocol][7]. At some point this doc will become an overview of the architecture and high level documentation of the code. I say something like an `ssb-server` because though I'd like to use it that way I have quite a different approach with respect to persistence. `ssb-server` is also in my mind a bad name. Peer to peer systems should not have servers and clients. All nodes are `peers` and should act like both clients and servers, so the distinction becomes moot.

Introduction
-------------

Scuttlebutt is an interesting protocol. A feed is an immutable append-only log file, identified by a public key, where each message is signed by a private key and references the prior message. This ensures a single source of truth since only one peer can sign messages on a feed. At any time, different peers might have different versions of the same feed that only differ by the length. In other words one can be a prefix of the other.

Messages are encoded as [json][8] (newer implementations have improved this with various binary formats). Messages have a content object that has a type. Messages of type `follow` form the basis for the social graph that is used to drive the replication process. Feeds follow other feeds more or less. This makes for a simple replication, where one node in the network asks a peer for all messages of a feed beyond the latest sequence number they have.

It's unfortunate that the social graph definition is in the content part of the message. It seems to be more an application concern. At the system level of connections, feeds and replication, there shouldn't be any awareness of the content. It's hard to say. The social graph drives the replication, once connections to other peers are made. Regardless though, almost anything can be worked around becuase the feeds are immutable, so a collection of feeds will only ever grow and anything needed can be built and maintained incrementally.


----
[0]: https://github.com/rebar/rebar3
[1]: https://viewer.scuttlebot.io/%25pYmFr6d0QwLP%2BYG0VNoo75PP7eYNZ1Y8C2MC9IjF5aw%3D.sha256
[2]: http://localhost:8989/blobs/get/&4DUnrqwI7xxUpP6omK1wiPSco5uLrNa6Ey7lNrXCzCU=.sha256
[3]: https://github.com/cn-uofbasel/ssbdrv/blob/master/doc/tangle.md
[4]: https://github.com/ssbc/ssb-server
[5]: https://cloud.google.com/healthcare/
[6]: https://github.com/flumedb/flumedb
[7]: https://ssbc.github.io/scuttlebutt-protocol-guide/
[8]: https://ssbc.github.io/scuttlebutt-protocol-guide/#message-format
