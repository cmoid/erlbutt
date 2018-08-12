erlbutt
======

An implementation of the [`SSB`][1] protocol.

## [Project notes][2]

## Building and running

```
./rebar3 eunit

./rebar3 ct

./rebar3 release

SSB_LOG_LEVEL=debug _build/default/rel/ssb/bin/ssb-0.0.1 console
```

----
[1]: https://ssbc.github.io/scuttlebutt-protocol-guide/
[2]: https://github.com/cmoid/erlbutt/blob/main/doc/project.md
