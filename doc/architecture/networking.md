Networking
==========

How two peers find each other, prove who they are, and talk. Replication
— what they say once connected — is in [replication.md](replication.md).

A connection, end to end
------------------------

The listener is ranch, started by `ssb_app` on `SSB_PORT` (default
8008). Every connection, inbound or outbound, is one `ssb_peer` process
holding the socket, plus a per-connection `rpc_processor`.

1. **Secret handshake** (`shs`). Both sides prove possession of their
   long-term Ed25519 keys under a shared *network id* — a 32-byte key
   that partitions SSB networks from one another. A peer on a different
   network id cannot complete the handshake. A node can accept more than
   one network id (`extra_network_ids`); when dialing, `network_id_cache`
   remembers which id last worked for each peer and tries it first.
2. **Box stream** (`boxstream`). Everything after the handshake is
   encrypted and authenticated in packets of at most 4096 bytes, with
   per-direction keys and nonces derived from the handshake.
3. **muxrpc** (`rpc_parse`, `rpc_processor`). Inside the box stream,
   frames carry requests and responses: `async`/`sync` calls with one
   reply, `source`/`sink` streams, and `duplex` streams in both
   directions (EBT, tunnels). Request numbers tie responses to calls.

The authenticated remote key decides the caller's **class**, computed
per call:

- `owner` — the remote key *is* this node's own key. That is a local
  client: silkpurse, HighWire, the `sbutt` CLI. Owners may call anything.
- `member` — a registered member of this node's room.
- `peer` — anyone else who completed the handshake.

Each method declares who may call it (`anyone`, `member`, `owner`, or
`room` for room-only methods), and `rpc_processor` checks before
dispatching. Methods the foundation implements are dispatched inside
`rpc_processor`; everything else is looked up in `plugin_registry`. See
[rpc-methods.md](rpc-methods.md) for the list and
[plugins.md](plugins.md) for adding methods.

`rpc_processor` is per connection, with its own unnamed ETS table of
open streams, so one slow or misbehaving connection cannot stall
another.

Finding peers
-------------

Opening connections is `peer_dialer`'s job. Every 30 seconds it gathers
candidates from two sources and dials some of them:

- **LAN peers**, from `heartbeat`, which broadcasts this node's address
  over UDP on the local network and listens for others doing the same;
- **known addresses** from `conn_db` — `conn.json`, in the same format
  the JavaScript clients use — where `autoconnect` is true. Pub
  announcements (`pub` messages) in the feeds we replicate add entries.

Before dialing, it checks `peer_registry`, which tracks every live
connection by remote key, so a peer that already connected to us is not
dialed back. The dialer keeps at most ten connections, and a peer that
fails is retried on an exponential backoff from five minutes up to six
hours.

The dialer can be switched off — `{peer_dialer, false}` in `ssb.cfg`, or
`admin.dialer.disable` at runtime, which persists in `overrides.cfg`. A
node with the dialer off still accepts inbound connections but never
opens any, which on a personal node looks like a silent outage.
`admin.dialer.status` and `sbutt health` show the state.

Pubs and invites
----------------

A pub is an ordinary node with a public address that others follow so it
replicates them. Getting followed by a stranger's pub is what invites are
for.

- `invite.create` (owner only; `sbutt invite create HOST PORT`) mints a
  code of the form `host:port:@pubkey.ed25519~seed`. The seed derives a
  throwaway keypair; its public key is remembered in `invite_store`
  (`invites.tab`), so an outstanding invite survives a restart.
- The newcomer connects using the throwaway keypair and calls
  `invite.use` with their real feed id. The pub consumes the invite and
  publishes a follow of the newcomer. The newcomer follows the pub back
  and records its address.

Every invite issued and consumed is logged, because nothing else records
who used which invite.

Rooms
-----

A room is a relay: peers that cannot reach each other directly (behind
NAT, say) both connect to the room, and the room pipes a connection
between them. Erlbutt implements the Rooms 2.0 core without its HTTP
parts.

- A node is a room when `{room, true}` is set. `room_privacy` is `open`
  (anyone may use it), `community` or `restricted` (members only).
  Members join by redeeming a room invite, which adds them to
  `room_store` instead of publishing a follow.
- `room_attendants` tracks who is connected right now and pushes
  join/leave events to subscribers (`room.attendants`).
- `tunnel.connect` from B, naming A, makes the room open a stream to A
  and run a `tunnel_bridge` that forwards frames both ways. The room only
  forwards bytes: B and A run their **own secret handshake inside the
  tunnel**, so the room cannot read or forge their traffic.
- On each end, `ssb_peer` runs in *tunnel mode*: the same process,
  muxrpc, EBT and blob machinery, over a tunnel handle instead of a
  socket. Full replication works through a room.

Further reading
---------------

- [Scuttlebutt protocol guide](https://ssbc.github.io/scuttlebutt-protocol-guide/)
  — the handshake, box stream and muxrpc on the wire.
- [research/papers/shs.pdf](../research/papers/shs.pdf) — the secret
  handshake paper.
