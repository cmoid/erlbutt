# Moving an SSB feed into erlbutt

Converting a flume store into an erlbutt node that then
*owns* that identity: erlbutt becomes the database for the feed, and
clients (silkpurse in remote mode, maxbutt) talk to it over muxrpc. It's
critical to understand that once this is done, one can't go back and publish against that flume log. Think of this conversion routine as a databse importer.

## The rule that matters

**After the cutover, nothing but this erlbutt node may ever publish to
that feed again.**

Two databases holding the same secret and appending to their own copies of
the log is a fork, and on the real network a forked feed is what other
clients refuse. It is not a risk you take once and get away with — the
divergence is permanent, signed by you, and cannot be retracted.

So: convert from a **backup copy**, verify, and then retire the original
store rather than leaving it runnable. Renaming it is enough; the point is
that double-clicking the old client must not start a second writer.

## Use the prod profile, specifically

`config/prod.vars` is what selects the real network:

```erlang
{network_id, "1KHLiKZvAvjbY1ziZEHMXawbCEIM6qwjCDm3VYRan\/s="}.
```

`default.vars` carries an almost-identical id that differs in its last few
characters, so `make rel`, `make devpkg`, eunit and CT can never reach the
real network by accident. **Building the prod profile is the deliberate act
of joining it** — there is no config file to hand-edit, and editing one by
hand is how you end up on a network of one.

`default.vars` also sets `{peer_dialer, false}`, which is what makes the
sequence below safe: a freshly installed node holds your identity but
talks to nobody until you say so.

## 1. Build and install

```shell
cd <code-path>/erlbutt
make prodpkg                     # -> dist/erlbutt-prod-<vsn>-<platform>.tar.gz

mkdir -p ~/erlbutt-ssb
tar -xzf dist/erlbutt-prod-*.tar.gz -C ~/erlbutt-ssb
```

Anywhere outside the build tree will do. `_build/default/rel/ssb/` is a bad
home for an identity you cannot re-create: `SSB_HOME` defaults to `.`, so a
node run from there puts its data *inside the release directory*, where the
next `make rel` is one command away from confusing the two.


## 2. Give it the identity

```shell
mkdir -p ~/erlbutt-ssb/.ssberl
cp /path/to/backup/secret ~/erlbutt-ssb/.ssberl/secret
chmod 600 ~/erlbutt-ssb/.ssberl/secret
```

`SSB_HOME` is the *parent*: data lands in `$SSB_HOME/.ssberl/`. Getting
this wrong nests a `.ssberl` inside a `.ssberl` and the node starts as a
stranger — which is recoverable, but only if you notice.

## 3. Make the log reachable

`converter:convert/4` resolves its first argument against
the current working directory, not as an absolute path:

```erlang
{ok, Cwd} = file:get_cwd(),
File = Cwd ++ OffsetLog,
```

So symlink the backup in rather than fighting it:

```shell
ln -s /path/to/backup/flume/log.offset ~/erlbutt-ssb/log.offset
```

## 4. Convert, with the views paused

```shell
SSB_HOME=~/erlbutt-ssb SSB_NODE=moid@localhost SSB_DIST_PORT=9300 \
  ~/erlbutt-ssb/bin/ssb console
```

Then, in that shell:

```erlang
%% Confirm the identity BEFORE writing anything.
keys:pub_key_disp().

%% Drop the indexes for the bulk load.
{ok, _} = view_manager:pause_ingest().

converter:convert("log.offset", 200, [all],
                  "/path/to/backup/blobs").

%% Build them once, from the store.
{ok, _} = view_manager:resume_ingest().
```

Pausing matters: every stored message otherwise fans out to ten views
synchronously, each writing to SQLite, paid a couple of million times for
an index that is built once at the end either way.

The blob source must be given explicitly — it defaults to `~/.ssb/blobs`,
which is not yours. Blobs are copied *before* each message is stored, so
that importing them does not raise a want for a blob already in hand.

`resume_ingest/0` returns as soon as the folds are scheduled, not when they
finish. Watch for one `caught up` line per view:

```erlang
view_manager:info().    %% {Mod, Class, Version, FeedsCheckpointed}
```

## 5. Verify before trusting it

```erlang
Id  = keys:pub_key_disp(),
Pid = utils:find_or_create_feed_pid(Id),
ssb_feed:current_seq(Pid).          %% matches the JS client's last sequence?
ssb_feed:fetch_last_msg(Pid).       %% your most recent post?
```

Then point silkpurse at it in remote mode and read the timeline. Compare
against the old client — same posts, same avatars, same follows — while
the old client is still runnable and before anything has been published.

The dialer is off, so none of this touches the network.

## 6. Join the network

Only once the above looks right:

```erlang
peer_dialer:enable().
```

Then retire the old store, so it cannot become a second writer:

```shell
mv ~/.silkpurse-moid ~/.silkpurse-moid.RETIRED-$(date +%Y%m%d)
```

## If it goes wrong

Before step 6 nothing has been published and nothing has been sent: delete
`~/erlbutt-moid` and start again. The backup is untouched throughout —
the converter only ever reads it.

After step 6, a mistake is a published mistake. That asymmetry is the
reason for the order.
