erlbutt
======

An implementation of the [`SSB`][1] protocol.

## Building and running

```
./rebar3 eunit

./rebar3 ct

./rebar3 release

SSB_LOG_LEVEL=debug _build/default/rel/ssb/bin/ssb-0.0.1 console
```

----
[1]: https://ssbc.github.io/scuttlebutt-protocol-guide/
