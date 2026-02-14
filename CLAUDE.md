# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

erlbutt is an Erlang implementation of the SSB (Secure Scuttlebutt) protocol — a peer-to-peer social networking protocol. It uses a custom network ID (distinct from the main SSB network). Licensed under GPL-2.0-only.

## Build Commands

```bash
make compile      # Compile the project
make test         # Run eunit tests (unzips testdata.zip, runs tests, cleans up)
make ct           # Run Common Test suites (test/erlbutt_basic_SUITE.erl)
make rel          # Build a release
make prod         # Build a production release (warnings_as_errors, includes ERTS)
make all          # Clean, compile, test, and build prod release
```

Run the release: `./_build/default/rel/ssb/bin/ssb-0.0.1 console`

Control logging: `SSB_LOG_LEVEL=debug _build/default/rel/ssb/bin/ssb-0.0.1 console`

The build system uses `./rebar3` (vendored in the repo), not a system-installed rebar3.

## Architecture

Single OTP application (`ssb`) located in `apps/ssb/`. Uses rebar3 with deps: ranch (TCP acceptor pool) and enacl (NaCl crypto bindings, requires libsodium).

### Supervision Tree (`ssb_sup`)

`ssb_app` starts a ranch TCP listener on port 8008, then starts the supervisor with these children:
- **config** — manages SSB repo paths (feeds, blobs) and network ID
- **heartbeat** — keepalive management
- **keys** — loads and serves the node's ed25519 keypair from `~/.ssb/secret`
- **blobs** — blob storage
- **mess_auth** — message authentication
- **ebt** — Epidemic Broadcast Tree replication (in active development)

### Connection Flow

`ssb_peer` is a gen_server implementing `ranch_protocol`. Two modes:
1. **Outbound**: `start_link(Ip, PubKey)` — initiates SHS handshake as client
2. **Inbound**: `start_link(Ref, Socket, Transport, Opts)` — accepts via ranch

After the Secret Handshake (`shs` module), communication switches to box stream encryption (`boxstream` module). Decrypted messages are parsed as RPC (`rpc_parse`) and dispatched by `rpc_processor`.

### Key Modules

- **shs** — Secret Handshake protocol (mutual authentication using enacl)
- **boxstream** — Box stream encryption/decryption for framed messages
- **rpc_processor** — Dispatches incoming RPC calls (createHistoryStream, whoami, ping, blobs, ebt, etc.)
- **ssb_feed** — Per-feed gen_server managing message storage and retrieval via DETS
- **message** — SSB message encoding/decoding with signature verification
- **ebt / ebt_vc** — EBT vector clock replication (work in progress)
- **friends** — Social graph (follow/block relationships)
- **social_msg** — Social message types (post, vote, contact, about, pub)

### Important Records (defined in `apps/ssb/include/ssb.hrl`)

- `#sbox_state{}` — per-connection state (socket, encryption keys, nonces, buffered data)
- `#ssb_conn{}` — connection info passed to RPC processor
- `#message{}` — SSB message with id, author, sequence, content, signature

### Conventions

- Many modules use `-compile({no_auto_import,[size/1]})` and `-import(utils, [...])` to use custom `utils:size/1` (returns byte size of binaries) and `utils:combine/2`
- The `?CHILD(I, Type)` macro in ssb.hrl declares supervisor children
- RPC method names are binary strings defined in ssb.hrl (`?createhistorystream`, `?whoami`, etc.)
- Eunit tests are embedded in source files via `-ifdef(TEST)` blocks; run with `make test`
