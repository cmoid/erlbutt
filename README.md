erlbutt
======

An implementation of the [`SSB`][1] protocol.

## [Project notes][2]

## Building

```
make test

make ct

make rel

```
To run:
```
./_build/default/rel/ssb/bin/ssb-0.0.1 console

```
An environment variable can be used to control logging, .eg.:

```
SSB_LOG_LEVEL=debug _build/default/rel/ssb/bin/ssb-0.0.1 console
```

There is also a [dev-diary][3] that are posts made on `SSB`, so many of the linked items in those entries are not visible in github.

----
[1]: https://ssbc.github.io/scuttlebutt-protocol-guide/
[2]: https://github.com/cmoid/erlbutt/blob/main/doc/project.md
[3]: https://github.com/cmoid/erlbutt/tree/main/doc/dev-diary
