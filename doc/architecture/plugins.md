Plugins
=======

The node's muxrpc surface and its derived state are both extensible
without touching the foundation. Two behaviours do it:

- **`ssb_plugin`** adds muxrpc methods.
- **`ssb_view`** adds a view: state folded from the log and kept current
  as messages arrive.

A module often implements both — a view that also answers queries about
what it folded. silkpurse, admin and the foundation's own core views are
built this way inside the tree. [ssb_git](https://github.com/cmoid/ssb_git)
is the worked example of a plugin built outside it.

Writing a method: `ssb_plugin`
------------------------------

```erlang
-module(hello_rpc).
-behaviour(ssb_plugin).
-include_lib("ssb/include/ssb.hrl").
-export([manifest/0, handle_rpc/3]).

manifest() ->
    [{[~"hello", ~"say"],   async,  anyone},
     {[~"hello", ~"count"], source, owner}].

handle_rpc([~"hello", ~"say"], [Name], _Caller) when is_binary(Name) ->
    {reply, <<"hello ", Name/binary>>};
handle_rpc([~"hello", ~"count"], [N], _Caller) when is_integer(N), N > 0 ->
    {source, lists:seq(1, N)};
handle_rpc(_, _, _) ->
    {error, ~"bad arguments"}.
```

**`manifest/0`** lists `{Name, Kind, Perm}`:

- `Name` is the muxrpc path as a list of binaries; `[~"hello", ~"say"]`
  is `hello.say` to a JavaScript client.
- `Kind` is `sync`, `async` or `source`. (`sync` and `async` are framed
  the same on the wire.) Duplex methods cannot be registered by a plugin
  yet.
- `Perm` is who may call it: `anyone` who completed the handshake, a room
  `member`, or only the node's `owner`. See
  [networking](networking.md#a-connection-end-to-end) for how a
  caller's class is decided.

**`handle_rpc(Name, Args, Caller)`** gets the decoded arguments and a
caller map `#{feed_id => binary() | undefined, class => owner | member |
peer}`, and returns:

| Return | Meaning |
|---|---|
| `{reply, Term}` | one reply, for `sync`/`async`; `Term` is EJSON (`{Proplist}`, binary, number, boolean, list) |
| `{source, [Item]}` | a stream; it closes after the last item. An item may be a zero-arity fun, evaluated lazily as the stream is sent |
| `{live_source, Pairs, ViewMod, EventFun}` | for `{live: true}`: send the snapshot, then keep the stream open and push what `EventFun` makes of each later event from `ViewMod` |
| `{error, Reason}` | a muxrpc error frame; `Reason` is a binary |

A reply or item may be `{json, Bin}` — pre-encoded JSON sent as is, for
serving stored message bytes without decoding and re-encoding them.

A crash in `handle_rpc` becomes an error reply to that caller; it does
not take down the connection.

Names are first come, first served: registering a method another module
already owns is refused (`method_taken`), and so is a builtin name.

Writing a view
--------------

```erlang
-behaviour(ssb_view).

view_version() -> 1.          %% bump when the fold changes
view_class()   -> app.        %% optional; app is the default
view_load()    -> ok.         %% ok: my rows match my checkpoints; empty: rebuild me
view_reset()   -> ...         %% drop all my rows
view_save()    -> ok.         %% flush anything buffered
view_entry(#message{} = Msg) -> ok | {events, [Event]}.
```

The conventions, which every view in the tree follows:

- A view is a `gen_server` that **declares its tables** in its `init/1`
  with `ssb_store:declare(Module, SchemaVersion, DDL)`, then registers
  from `handle_continue`.
- The callbacks run in the **view manager's process**, not the view's.
  Write them as plain functions over `ssb_store` (`q/2` to read,
  `write/2` or `insert_many/2` to write), never as calls into the view's
  own process — that would deadlock registration.
- `view_entry/1` must be **idempotent per (feed, seq)**: after a crash a
  view may be handed a few messages it has already folded.
- Returned events are published to `view_manager:subscribe/1`
  subscribers, which is how `live_source` streams learn of new matches.

How the manager drives these — catch-up, checkpoints, rebuilds — is in
[views.md](views.md).

Registering
-----------

Call `ssb_view:ensure_registered(Module)` — it registers the module with
`plugin_registry` and `view_manager` as appropriate, logs a rejected
registration loudly, and returns `retry` if a registry was not up yet, in
which case send yourself a message and try again in a moment. ssb_git's
`ssb_git_repos:init/1` is a complete example in a dozen lines. Use
`ssb_view:ensure_registered(Module, [plugin])` for a module that is only
a plugin.

Packaging a plugin
------------------

A plugin outside the tree is an ordinary OTP application that depends on
`ssb`. ssb_git's layout:

```
ssb_git/
  rebar.config      {deps, [{ssb, {git_subdir, ".../erlbutt.git", {branch, "main"}, "apps/ssb"}}]}
  src/ssb_git.app.src   {applications, [kernel, stdlib, ssb]}
  src/*.erl
```

The `ssb` dependency is only for compiling — the behaviours, `ssb.hrl`,
the store API. At runtime the plugin uses the beams of the node it is
loaded into. For local work, symlink `_checkouts/ssb` to an erlbutt
checkout's `apps/ssb`.

### Installing

`plugin_loader` looks for plugins at boot in two places, first match
wins:

```
<data>/plugins/<app>/ebin/<app>.app       added by the operator
<release>/plugins/<app>/ebin/<app>.app    shipped in the package
```

So a plugin dropped into the data directory survives upgrades and
overrides a shipped one of the same name. A `priv/` beside `ebin/` is
used too.

`{plugins, all}` (the default) in `ssb.cfg` starts every plugin found;
`{plugins, [ssb_git]}` starts only those named; `{plugins, []}` starts
none.

For each plugin the loader checks the API version, adds the ebin to the
code path, loads every module the `.app` lists (the release boots in
embedded mode, which loads nothing on demand), and starts the
application as **temporary**. A plugin that fails any step is logged and
skipped, and a plugin that crashes later stops without taking the node
with it. `plugin_loader:list/0` reports what happened to each.

To ship a plugin inside a package, build erlbutt with `PLUGINS` naming
the plugin's source tree; `scripts/stage-plugins.sh` builds it against
this tree's `apps/ssb` and the release overlay copies it into
`<release>/plugins/`. The Makefile defaults `PLUGINS` to `../ssb_git`
when that directory exists.

### The API version

`ssb.hrl` stamps every module that includes it with
`-ssb_plugin_api(N)`. The registries refuse a module built against a
different `N` than the node's, and `ssb_plugin:check_app/1` lets an
application refuse to start at all, naming both versions — ssb_git does
this first thing in `start/2`.

The version guards **record layout** — chiefly `#message{}`, which
`view_entry/1` receives. It does not guard function signatures. There is
not yet a formal list of functions a plugin may rely on; in practice,
stick to the behaviours, `ssb_store`, `ssb_view`, `plugin_registry`,
`view_manager:subscribe/1`, `keys`, and the `#message{}` record, and
expect to rebuild against each erlbutt release.

Calling methods from inside the node
------------------------------------

`plugin_registry:call(Name, Args, Caller)` invokes a plugin method
in-process, with the same permission check a muxrpc caller of that class
would get:

```erlang
plugin_registry:call([~"admin", ~"views", ~"list"], [],
                     #{class => owner, feed_id => keys:pub_key_disp()}).
%% {ok, Reply} | {error, not_allowed | unknown_method | builtin | Reason}
```

Sources answer `{ok, [Item]}` (a `live_source` answers its snapshot).
This is for code already running on the node or connected over Erlang
distribution — maxbutt, an Emacs client, a remote shell.

Further reading
---------------

- [rpc-methods.md](rpc-methods.md) — what is already registered.
- [research/plugin-architecture.md](../research/plugin-architecture.md)
  — how secret-stack plugins work in JavaScript and why erlbutt's take
  two behaviours.
- [research/git-over-ssb.md](../research/git-over-ssb.md) — the design
  of ssb_git.
