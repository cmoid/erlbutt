## [erlbutt](https://github.com/cmoid/erlbutt)

Part of my New Years resolution was to spend more time working on this healthcare idea involving [electronic health records](%6cm3l4qv/4KOYJoHhhjZKU2LY09Mp3fE6JHN8WU374M=.sha256) (EHR). The immutable append only feed that's at the heart of scuttlebutt is ideal for providing a source of truth in a portable and private way. I'd like to build a company around this idea.

To that end, I've resurrected this [prototype](https://github.com/cmoid/erlbutt) that was started [ages ago](%d5gv/qcOJiI2ow7VWpZ20LJASksEOtP4asjW28nMyvw=.sha256). The initial goal is to have something like an `ssb-server` running by July 4th. The features that are needed in this use case are replication, feed management, configurability of network id, and possibly support for handling different network ids in the same running instance. Targeting the legacy message format for now, it would be great if the community settles on something like [gabbygrove](https://github.com/ssbc/ssb-spec-drafts/blob/master/drafts/draft-ssb-core-gabbygrove/00/draft-ssb-core-gabbygrove-00.md) to support off-chain content.

It's important to minimize dependencies. That should be a little easier in erlang, as OTP is very mature and includes a lot already. So far Jiffy is used for dealing with JSON, and ranch for managing sockets. Both are first rate and well supported. For persistence we have mnesia. It comes built in and integrates nicely with erlang. I'm also going to stick to the current flumedb architecture for now, at least the `log.offset` foundation as that seems [about right](%pYmFr6d0QwLP+YG0VNoo75PP7eYNZ1Y8C2MC9IjF5aw=.sha256  ), scuttlebutt is more or less a log manager.

This is still very much a prototype. There's a few simple `gen_servers` (erlang's notion of objects) that handle new connections, the secret handshake, boxstream, and parsing of RPC requests.

I'll post a diary here but also keep it in the repo.  When there's something to run that's usable I'll post an update here.

## My Feed Is My Story

----

## 02-21-2020 [erlbutt](https://github.com/cmoid/erlbutt)

Made reasonable progress the last couple of weeks. Spent too much time dealing with sysadmin issues. In the spirit of no good deed going unpunished, I had signed up for early versions of Catalina. Within a few weeks batteries on two macs died, no coincidence. Turns out there was this little process, TrustedPeersHelper, that was running incessantly, hitting a SQL-Lite database doing god knows what. Fortunately Apple was willing to replace the batteries and put in new keyboards also. So I have two macbooks and an old Thinkpad X230 running linux so that I can test erlbutt against the reference implementation and go-ssb.

I like the idea of a dev diary. I'm used to keeping daily logs for my business, so I thought what I'd do is keep a [daily](https://github.com/cmoid/erlbutt/blob/master/doc/dev-diary.md) one and then squash it every now and then to push a summary to `SSB` I do the same for code. I make lots of trivial small commits and then rebase and squash when I want to publish. It makes commit histories actually usable. So until July when hopefully I have a first usable version, I'll be rebasing to root a lot.

 * read threads on [muxrpc](%xcRxdizHSOvF7uV9kVd/SKjrt3Fij37eiQFhMuId29M=.sha256) issues
 * got the go-ssb server running. Really nice how the network id and whether or not it broadcasts and listens for UDP connections are configurable with command line switches
 * erlbutt talks to go-ssb and sees the same issues as with the JS ssb-server, so it's clearly erlbutt, sending bad headers :(
 * read spec on private groups and banana loaf. I'm curious as to why this isn't in the repo with all the other draft specs? Just legacy?
 * simple hack to send a ping request, and receive response
 * removed `ssb_rpc` `gen_server` and replaced with simple parse module
 * added `rpc_processor` to supervisor

So I'm pretty much at the same place but I've got the initial prototype a little more designed, the `SHS`, `boxstream`, and `RPC` parsing components are separated out. I've renamed the main listener `gen_server` to [ssb-peer](https://github.com/cmoid/erlbutt/blob/master/apps/ssb/src/ssb_peer.erl) as I want to get away from the client/server usage of terms. Nodes in P2P systems are all equal peers, in that sense they are both client and servers, so I prefer the term peer.

A key goal in the next couple of weeks is to get the code in better shape with respect to compatibility testing so I can leverage the works others have done for testing, and get at least one api call working end-end. I also want to have a closer look at the sunrise choir work. 

Big thanks to [@cryptix](@p13zSAiOpguI9nsawkGijsnMfWmFd5rlUNpzekEE+vI=.ed25519), [@mixmix](@ye+QM09iPcDJD6YvQYjoQc7sLF/IFhmNbEqgdzQo3lQ=.ed25519), and @Aljoscha for help/discussions.

cc: #erlang, #erlbutt

## My Feed Is My Story

## 02-26-2020

 * `ssb-peer` refactor
 
## 05-01-2020

 * resume work after taking month of April off from erlbutt, to adjust to the pandemic and a new world.
 * added function to fetch feed history from mnesia, and unit test

## 08-26-2020
 * resume project work, after missed deadline.
 * reviewed code, wondered who wrote it.
 
## 12-10-2020
 * small conversion hack to break out `log.offset` file into separate feeds
 
## 12-18-2020
 * squash history, move files around, project notes
 * read ngi-pointer dev diaries, new info on rooms2

## 02-12-2021
 * resume project work.
 
## 02-15-2021
 * ssb_feed2
 * triaged and responded to planetary issues
 
## 02-16-2021
 * minor refactoring

## 02-23-2021
 * fixed log offset file
 
## 02-24-2021
 * start new shard-feed branch
 * refactor converter to run under 2 mins.
 * rewrote `ssb_feed` to handle feeds
 * about profile messages stored in separate offset file

## 02-25-2021
 * make sure files sync when `gen_servers` close
