.PHONY: compile rel test ct clean distclean prod devpkg prodpkg packages release
REBAR=./rebar3

VSN  := $(shell git describe --always --tags)
DIST := dist

## Packages bundle a platform-specific ERTS, so encode the build host's OS and
## CPU in the artifact name (e.g. darwin-arm64, linux-x86_64).  Native build
## only — these reflect the machine you run `make` on, not a cross-target.
OS       := $(shell uname -s | tr '[:upper:]' '[:lower:]')
ARCH     := $(shell uname -m)
PLATFORM := $(OS)-$(ARCH)

DEV_PKG  := erlbutt-dev-$(VSN)-$(PLATFORM).tar.gz
PROD_PKG := erlbutt-prod-$(VSN)-$(PLATFORM).tar.gz

## Target repo for `gh` (passed as -R, so it works regardless of how the git
## remote is named or whether its URL uses an SSH host alias).
GH_REPO  ?= cmoid/erlbutt

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

distclean: clean
	rm -rf $(DIST)

test: compile
	unzip -n -o testdata.zip
	$(REBAR) eunit
	rm -Rf testdata

ct:  compile
	$(REBAR) ct --sname erlbutt_ct

rel: compile
	$(REBAR) release

prod: compile
	$(REBAR) as prod release

## Distributable developer package: bundled ERTS, debug beams, source, and the
## GUI/tooling apps for live development with maxbutt (which ships separately).
devpkg: compile
	$(REBAR) as devpkg tar
	@mkdir -p $(DIST)
	@cp $$(ls -t _build/devpkg/rel/ssb/ssb-*.tar.gz | head -1) $(DIST)/$(DEV_PKG)
	@cd $(DIST) && shasum -a 256 $(DEV_PKG) > $(DEV_PKG).sha256
	@echo "==> $(DIST)/$(DEV_PKG)"

## Production package: bundled ERTS, stripped beams, no source, no GUI stack.
prodpkg: compile
	$(REBAR) as prod tar
	@mkdir -p $(DIST)
	@cp $$(ls -t _build/prod/rel/ssb/ssb-*.tar.gz | head -1) $(DIST)/$(PROD_PKG)
	@cd $(DIST) && shasum -a 256 $(PROD_PKG) > $(PROD_PKG).sha256
	@echo "==> $(DIST)/$(PROD_PKG)"

packages: devpkg prodpkg

## Build this host's packages and attach them to the GitHub release for the
## current tag.  Run on each build machine (e.g. Mac then Linux): the first call
## creates the (published) release, later calls add their platform's assets
## (--clobber lets a re-run replace them).  The release is published, not draft —
## drafts can't be addressed by tag, so a per-machine create-or-upload flow can
## only work against a published release.  Requires the gh CLI (authenticated)
## and HEAD checked out at a tag you have pushed.
release:
	@git describe --exact-match --tags HEAD >/dev/null 2>&1 || { \
	  echo "ERROR: HEAD is not at a tag. Tag and push first, e.g.:"; \
	  echo "  git tag -a v0.1.0 -m 'erlbutt v0.1.0' && git push gh v0.1.0"; \
	  exit 1; }
	$(MAKE) packages
	@cd $(DIST) && \
	  gh release create $(VSN) -R $(GH_REPO) --generate-notes --title $(VSN) \
	      $(DEV_PKG) $(PROD_PKG) \
	  || gh release upload $(VSN) -R $(GH_REPO) --clobber \
	      $(DEV_PKG) $(PROD_PKG)
	@echo "==> $(PLATFORM) assets attached to release $(VSN)"

all:  clean compile test prod
