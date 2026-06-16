.PHONY: compile rel test ct clean distclean prod devpkg prodpkg packages
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
	$(REBAR) ct

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

all:  clean compile test prod
