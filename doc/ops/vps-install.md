# Installing erlbutt as a pub on a VPS

*Operational note, August 2026. Companion files in this directory:
`erlbutt.service` (systemd unit) and `sbutt` (CLI wrapper).*

A pub's address is `net:<host>:8008~shs:<pubkey>`, and peers resolve the
hostname at dial time. **The identity is the key, not the IP** — which is
what makes a pub cheap to move between machines, and what makes losing the
key unrecoverable.

Read §1 before touching anything. The rest is mechanical.

---

## 1. Two rules

### The identity may exist in exactly one running place

`secret` **is** the pub. If two nodes hold the same `secret` and either
publishes, you have written two different messages at the same sequence
number and forked your own feed. There is no merge in SSB: peers keep
whichever chain they saw first and reject the other.

So a machine you are replacing does not "stay up as a backup". Stop it, and
if you want erlbutt running there afterwards, give that install a **new**
identity that follows the pub.

Keeping `secret` is also what lets you move hosts without anyone
re-inviting you: same key, same hostname, every existing `conn.json` entry
still resolves. Losing it means a new pub that nobody has heard of.

### `.ssberl` holds two kinds of thing, and only one is recoverable

| | |
|---|---|
| **Truth** — nothing can rebuild it | `secret`, `feeds/` (especially your own feed), `room_members.tab`, `invites.tab` |
| **Derived** — a refold rebuilds it | `store.db` (all views + checkpoints), `ingest.journal`, `conn.json` |

`room_members.tab` and `invites.tab` look like caches and are not: they
record who was admitted to a room and which invite codes were minted, both
authored by this node. No amount of refolding the log recreates them. They
belong with `secret`, not with the views.

Your own feed is the sharp edge in `feeds/`. Everything else can be
re-replicated from peers; your own log cannot, because no peer will send it
back to you if you are its only author. See `resync-runbook.md` before
deleting anything under `.ssberl`.

## 2. Pin the distribution port

Needed if you want to tunnel in with maxbutt or use `remote_console`;
otherwise the port is random on every boot and no `ssh -L` can name it.
`config/vm.args.prod` carries:

```
-kernel inet_dist_listen_min ${SSB_DIST_PORT:-9100}
```

Setting `min` alone is enough — with `inet_dist_listen_max` unset it
defaults to `min`, pinning the port exactly rather than opening a range.

The cookie is the other half, and it is **not** fixed by editing that file:
`vm.args.prod` is in git, so the value there can only ever be a
placeholder. Set `SSB_COOKIE` in `/etc/erlbutt/env` (§5) and let the
`${SSB_COOKIE:-...}` substitution pick it up. On a box with a public IP
that cookie is the only thing between a reachable distribution port and
arbitrary code execution, and the whole loopback posture assumes it is
real.

## 3. The box

**Hetzner CX23** — 2 vCPU x86, 4GB RAM, 40GB NVMe, €6.49/mo — or anything
comparable. What matters is a **static IPv4**, which is the thing a home
connection does not reliably give you.

Storage on a pub is mostly blobs, and that is a function of how many feeds
it follows and how long they are. Check before sizing:

```
du -sh ~/.ssberl ~/.ssberl/blobs ~/.ssberl/feeds
```

Running the pub here rather than behind the home router is also a security
improvement: 8008 is no longer a hole through NAT into the LAN where
everything else you own lives, and the thing listening on it is a
hand-written binary protocol parser.

## 4. Prepare the box

Non-root user, SSH keys only, no password auth. Then:

```
sudo apt update && sudo apt install -y build-essential libsodium-dev git
sudo apt install -y unattended-upgrades
```

`enacl` links against system libsodium, so `libsodium-dev` is required to
build and the runtime library must stay installed. `esqlite` bundles the
SQLite amalgamation (`SQLITE_DQS=0`) and needs nothing but a C compiler.

Erlang **28** — the floor is 27 (the code uses the `json` module and
`~"..."` sigils). Distro packages are usually older, so use the Erlang
Solutions repository or kerl. Only the *build* needs it: `prod` bundles
ERTS, so the running node carries its own.

Firewall — 8008 and SSH, nothing else. In particular **not** 4369 or 9100:

```
sudo ufw default deny incoming
sudo ufw allow 22/tcp
sudo ufw allow 8008/tcp
sudo ufw enable
```

Only TCP needs opening. `heartbeat.erl` also opens UDP 8008 for LAN
discovery; on a VPS there is no LAN to find and it is harmless.

Two accounts, with deliberately opposite properties: an admin user you SSH
in as (sudo, real shell, your key), and a service account that runs the
daemon and is never logged into. The daemon must not run as the account
holding your keys and sudo rights.

```
sudo useradd --system --home-dir /var/lib/erlbutt --create-home \
             --shell /usr/sbin/nologin erlbutt
sudo install -d -o erlbutt -g erlbutt /opt/erlbutt /var/lib/erlbutt
sudo install -d -o root -g erlbutt -m 750 /etc/erlbutt
sudo -u erlbutt mkdir -p /var/lib/erlbutt/.ssberl
sudo -u erlbutt ln -s /var/lib/erlbutt/.ssberl /opt/erlbutt/.ssberl
```

That symlink is the whole layout, and it is worth understanding rather than
copying. Data should not live inside a directory that upgrades replace, but
`SSB_HOME` cannot simply point at `/var/lib` either, because
**`sbutt.escript` resolves three different things from that one variable**:
the release's beams (`$SSB_HOME/lib/*/ebin`), `ssb.cfg`, and `.ssberl`.
Point it at the data directory and the CLI finds no code; point it at the
release directory and the data sits inside the thing you replace on
upgrade. So `SSB_HOME` is the release root, `.ssberl` under it is a symlink
out to `/var/lib/erlbutt`, and node and CLI agree on one value.

Extracting a release tarball over `/opt/erlbutt` does **not** disturb the
symlink — the tarball contains no `.ssberl` path. It is lost only if a
deploy removes the directory first (`rm -rf /opt/erlbutt`). But the
consequence when it does go missing is the worst one available: the node
boots, finds no `secret`, and mints a new identity. §7's `whoami` is what
catches that.

## 5. Build and install

Build on the box. It takes about a minute and sidesteps every
cross-compilation question — the NIFs are built by the same toolchain that
loads them. (`make prodpkg` runs a macOS dylib vendoring step which detects
a non-Darwin host and copies the tarball through unchanged, so the same
target works on Linux.)

```
git clone https://github.com/cmoid/erlbutt && cd erlbutt
make prodpkg
sudo tar -xzf dist/erlbutt-prod-*-linux-*.tar.gz -C /opt/erlbutt
sudo chown -R erlbutt:erlbutt /opt/erlbutt
```

Build as your admin user in its own home, and extract with plain `sudo`
rather than `sudo -u erlbutt`: the service account cannot read a tarball
sitting in a 0700 home directory, and extracting *as* it fails in a way
that reads like a tar error. Extract as root, then hand the tree over — the
release directory has to end up owned by `erlbutt` regardless, because relx
expands `vm.args.src` and `sys.config.src` into `releases/<vsn>/` on every
boot.

**Config.** `config/default.vars` is the template source and the release
overlay renders it to `/opt/erlbutt/ssb.cfg`. The `prod` profile layers
`config/prod.vars` on top, which is where the real network id lives — every
other profile keeps the almost-mainnet id from `?DEFAULT_NETWORK_ID`, so
reaching the real network is something you ask for by name. Check what
actually landed:

```
grep -E 'network_id|require_valid_sigs|peer_dialer' /opt/erlbutt/ssb.cfg
```

`peer_dialer` off is right for a pub that mostly accepts inbound
connections; turn it on later if you want it dialling out from `conn_db`.

Never hand-edit `/opt/erlbutt/ssb.cfg` as a permanent fix — it is a
rendered overlay and the next deploy silently reverts it. Changes belong in
`config/default.vars` or `config/prod.vars`, in git.

**Cookie:**

```
printf 'SSB_COOKIE=%s\n' "$(head -c 32 /dev/urandom | base64)" \
  | sudo tee /etc/erlbutt/env >/dev/null
sudo chown root:erlbutt /etc/erlbutt/env && sudo chmod 640 /etc/erlbutt/env
```

**Unit file and CLI wrapper:**

```
sudo cp doc/ops/erlbutt.service /etc/systemd/system/
sudo systemctl daemon-reload && sudo systemctl enable erlbutt
sudo install -m 0755 doc/ops/sbutt /usr/local/bin/sbutt
```

Do **not** start it yet if you are bringing an existing identity (§6) —
booting with an empty `.ssberl` generates a fresh one.

## 6. Starting with an existing identity

Skip this for a brand new pub; `systemctl start` will mint an identity and
you are done.

To carry an existing pub across, copy `.ssberl` from the old host. Ordering
matters: copying live state out from under a running node gives you a torn
snapshot — a half-written log, a SQLite file mid-checkpoint.

1. **Stop the old node** and confirm it is down.

2. **Copy.** Not directly to `/var/lib/erlbutt` — the `erlbutt` account is
   `nologin` and holds no SSH key, by design, so it cannot be an rsync
   destination. Land it in your admin user's home and move it with sudo:

   ```
   rsync -a --info=progress2 ~/.ssberl/ admin@<vps>:/home/admin/ssberl-staging/

   # then, on the VPS
   sudo rsync -a /home/admin/ssberl-staging/ /var/lib/erlbutt/.ssberl/
   rm -rf /home/admin/ssberl-staging
   ```

   That needs room for the data twice. If it is close to half the disk, run
   the *remote* side as root instead — `--rsync-path="sudo rsync"` with a
   temporary `NOPASSWD: /usr/bin/rsync` sudoers entry, deleted as soon as
   the copy finishes. Be clear-eyed that rsync-as-root can write any file
   on the box, so that grant is root in all but name.

   If `.ssberl` on the source belongs to a different user than the one you
   SSH out as, the read side needs privilege too — and `sudo rsync -e ssh`
   authenticates as *root*, with root's keys, which is probably not what
   you want. `sudo cp -a` it somewhere readable first.

   Everything under `.ssberl/` goes. See §1 for why `room_members.tab` and
   `invites.tab` are not optional.

3. **Fix ownership**, because rsync preserved the old uid:

   ```
   sudo chown -R erlbutt:erlbutt /var/lib/erlbutt/.ssberl
   sudo chmod 600 /var/lib/erlbutt/.ssberl/secret
   ls -l /opt/erlbutt/.ssberl        # symlink still resolves?
   ```

4. If the hostname is moving too, lower its DNS TTL a day ahead, and
   **verify (§7) before flipping the record**. Until you do, the old host
   is still a working rollback.

## 7. First start and verify

```
sudo systemctl start erlbutt
journalctl -u erlbutt -f
```

First boot on freshly copied data does the view catch-up fold. On a corpus
in the hundreds of thousands of messages that is minutes, not seconds; it
yields between chunks (`view_catch_up_messages`, default 2000) so the node
stays responsive throughout.

Then:

```
sbutt whoami
sbutt health
```

`whoami` is the one that matters: on a migrated pub it must print the
**same identity as before**. If it prints something new, the node booted
without finding `secret` and generated one — stop it, do not let it
publish, and check the `.ssberl` symlink. Nothing is lost as long as
nothing was written; the danger is only in leaving it running.

`health` reports node, view and derived-store state and exits non-zero on
failure, so it works as a post-deploy gate. `sbutt census encoding` reads
the logs directly without connecting, so it is safe against a live node
when you want an independent count of what is actually stored.

`sbutt.escript` needs no installing — a relx overlay ships it at the
release root, where `bin/ssb escript` looks (`relx_escript` prepends
`$ROOTDIR`). The `/usr/local/bin/sbutt` wrapper runs it as the `erlbutt`
account, which is required: `.ssberl/secret` is mode 600 owned by that
user, and every command that opens a connection reads it. The wrapper also
sources `/etc/erlbutt/env`, which matters more than it looks — because
`vm.args.src` exists, **every** `bin/ssb` invocation re-expands `vm.args`
from the current environment, so running without `SSB_COOKIE` rewrites the
node's args file with the placeholder cookie.

Finally, confirm the pub is reachable *as a peer*, not merely listening.
`nc -vz <host> 8008` proves the port is open; only a real handshake proves
the identity and network id line up. Connect with a client you control
before telling anyone else it is ready.

## 8. Day to day

**Logs** are in two places: `journalctl -u erlbutt -f` for the stdout
handler, and `tail -f /opt/erlbutt/log/info.log` for the file handler.

If both are silent, check `SSB_LOG_LEVEL` before concluding the node is
wedged. Nearly all of erlbutt's logging goes through `?SSB_INFO`, which is
`?LOG_INFO`, and `logger:set_primary_config/2` gates on the primary level
*before* any handler is consulted — so `notice` starves `info.log` and
`debug.log` no matter how those handlers are configured, and a perfectly
healthy node emits one line at boot and nothing afterwards. The shipped
unit sets `info` for exactly this reason. Note also that a pub with no
inbound connections and `peer_dialer` off genuinely has nothing to say;
silence is not a symptom.

**An Erlang shell** needs no tunnel, since the node listens on loopback
right there — but it needs the same environment systemd gives it, for the
`vm.args` reason above:

```
sudo -u erlbutt env $(sudo cat /etc/erlbutt/env) \
     SSB_NODE=ssb@127.0.0.1 SSB_DIST_PORT=9100 SSB_HOME=/opt/erlbutt \
     /opt/erlbutt/bin/ssb remote_console
```

Worth putting in `/usr/local/bin/ssb-console` alongside `sbutt`.

**Do not type `q().` in that shell.** It evaluates on the far side —
`init:stop()` — and takes the pub down. Leave with `Ctrl-G` then `q`, which
quits only the local shell job. `Ctrl-C` twice is also safe (it aborts the
hidden remsh node, not the pub), but `Ctrl-G q` is the habit worth having.

**maxbutt from a workstation** goes over an SSH tunnel:

```
epmd -kill                       # local epmd would block the 4369 forward
ssh -N -L 4369:localhost:4369 -L 9100:localhost:9100 <vps>
```

`epmd -kill` first is the part that trips people up: distel starts its own
node — and therefore a local epmd — the moment it comes up, and `-L 4369`
cannot bind a port something already holds. With the tunnel up, distel's
lookup of `ssb` reaches the *remote* epmd, gets 9100, and connects back
through the second forward. Point maxbutt's `ssb-node` at `ssb@127.0.0.1`
(prod naming, not `erlbutt@localhost`) and give it the cookie from
`/etc/erlbutt/env`.

Two gotchas. Every RPC now crosses the internet rather than a LAN, so
chattier commands feel it and `gen_server:call` timeouts tuned for
sub-millisecond round trips may want raising. And while that tunnel is
open, *your local* epmd is the VPS's — anything local needing Erlang
distribution (`make ct`) will register itself remotely. Close it before
running tests.

## 9. Upgrades and rollback

A deploy is: build a tarball, extract it over `/opt/erlbutt`, chown,
restart.

```
cd ~/erlbutt && git pull && make prodpkg
sudo tar -xzf dist/erlbutt-prod-*-linux-*.tar.gz -C /opt/erlbutt
sudo chown -R erlbutt:erlbutt /opt/erlbutt
sudo systemctl restart erlbutt
sbutt health
```

Extraction is additive, and that is what makes rollback possible. Each
release contributes its own `releases/<vsn>/`, its own `lib/<app>-<vsn>/`
directories, and its own `bin/ssb-<vsn>` launcher — while `bin/ssb` is
overwritten to mean "current".

`bin/ssb` and `bin/ssb-<vsn>` are byte-identical at build time; both
hardcode `REL_VSN` and derive `REL_DIR` from it. The versioned copy exists
precisely because the unversioned one gets overwritten by the next deploy.
So after upgrading from `A` to `B` the tree still holds a complete,
bootable `A`, and rolling back is:

```
sudo systemctl stop erlbutt
sudoedit /etc/systemd/system/erlbutt.service   # ExecStart -> bin/ssb-<A>
sudo systemctl daemon-reload && sudo systemctl start erlbutt
```

or simply re-extract `A`'s tarball, which restores `bin/ssb` to `A`.

Three things that do **not** roll back with the code:

- **`ssb.cfg` lives at the release root, not under `releases/<vsn>/`**, so
  it is shared across versions and a new deploy overwrites it. Rolling back
  the launcher does not restore the old config. This is another reason
  config changes belong in `default.vars`/`prod.vars` in git rather than in
  the rendered file.
- **Views refold in either direction.** `view_manager` compares stored and
  code versions with `=:=`, so downgrading a view's `view_version/0` resets
  and refolds exactly as upgrading it does. Harmless — views are derived —
  but budget the catch-up time.
- **Stored truth is not versioned this way.** A change to the on-disk feed
  format would not be undone by swapping launcher scripts. Check what a
  release actually changed before assuming a rollback is clean.

Old versions accumulate. Prune deliberately when the tree gets untidy —
and never with `rm -rf /opt/erlbutt`, which takes the `.ssberl` symlink
with it (§4).

## 10. Backups

The thing you cannot regenerate is `secret`. It is ~100 bytes and belongs
in your password manager today, independently of any backup system.

Beyond that, back up what §1 lists as truth: `feeds/` for your own feed id,
`room_members.tab`, `invites.tab`. Everything else is either re-replicable
from peers or rebuildable by refolding.

Provider snapshots are the low-effort answer and take the whole disk. A
nightly `rsync` or `borg` of `.ssberl/` somewhere else is better, because
it survives the account and not just the disk. Either way take the snapshot
with the node stopped, or accept that a live copy of the SQLite store may
need recovery on restore.

## 11. Rebuilding derived state

If the views or the derived store are wrong — after a crash, a bad upgrade,
or a schema change — they can be rebuilt from the feed store without
touching truth. The blunt version is to stop the node, remove `store.db`
and let it refold; `sbutt health` will show the views catching up.

Refolding a large corpus is minutes, not seconds, and it is worth watching
`info.log`. A line reading `caught up in ~150 ms (1 pass)` means the view
folded **nothing** — a tail read that skipped every feed — so it is not
evidence that work happened. A real fold is orders of magnitude longer.

Wiping anything else under `.ssberl` is a different and more dangerous
operation, because of the own-feed problem in §1. `resync-runbook.md`
covers it, including the trap that peers will not re-send what you lost:
ssb-ebt folds clocks with `max()` and never rewinds, so a peer that
remembers you at sequence N will not go back, no matter what you advertise.
