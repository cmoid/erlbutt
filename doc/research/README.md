Research and design notes
=========================

These are the records of how erlbutt's designs were reached: the problem,
the options considered, the measurements, the decision, and — where the
build went somewhere the design did not — notes on what changed. They are
kept as written. For what the code does *now*, read
[`doc/architecture`](../architecture/overview.md); each architecture page
links back to the note behind it.

| Note | Written | Status | What it decided |
|---|---|---|---|
| [plugin-architecture.md](plugin-architecture.md) | July 2026 | built | Two behaviours, `ssb_plugin` for methods and `ssb_view` for derived state; per-feed logs rather than a global log; the plugin API version (October). |
| [persistence.md](persistence.md) | July 2026 | built | Truth in flat files, derivation in SQLite; the three layers (foundation, conventions, applications) and the core views. |
| [archive-boundaries.md](archive-boundaries.md) | August 2026 | built | How a feed retires its early history and a reader starts partway down, with no change to validation. Written to be argued with by other implementers. |
| [feed-glimpses.md](feed-glimpses.md) | September 2026 | built | How a feed describes itself to peers at the edge of its graph, and promotion as a private pin. |
| [git-over-ssb.md](git-over-ssb.md) | October 2026 | built as the [ssb_git](https://github.com/cmoid/ssb_git) plugin | Git repositories, pushes, issues and pull requests as SSB messages, compatible with the old git-ssb formats. |

Also here:

- [links.md](links.md) — SSB threads and articles that shaped the
  thinking (message ids are SSB links, readable from any client).
- [papers/](papers/) — the papers behind the protocol and its
  alternatives:

  | File | Paper |
  |---|---|
  | `shs.pdf` | Dominic Tarr, *Designing a Secret Handshake: Authenticated Key Exchange as a Capability System* (2015) — SSB's handshake |
  | `ebt.pdf` | Leitão, Pereira, Rodrigues, *Epidemic Broadcast Trees* (2007) — the basis of EBT replication and of plumtree |
  | `acmdl19-295.pdf` | *A Broadcast-Only Communication Model Based on Replicated Append-Only Logs* (2019) |
  | `dicg20-paper90.pdf` | *Gossiping with Append-Only Logs in Secure-Scuttlebutt* (DICG 2020) |
  | `ByzantineEventualConsistency.pdf` | Kleppmann and Howard, *Byzantine Eventual Consistency and the Fundamental Limits of Peer-to-Peer Databases* |
  | `itc2008.pdf` | Almeida, Baquero, Fonte, *Interval Tree Clocks: A Logical Clock for Dynamic Systems* (2008) |
  | `trustnet-cblgh.pdf` | cblgh, *TrustNet* — subjective trust metrics |
