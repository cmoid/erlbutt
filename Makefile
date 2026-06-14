.PHONY: compile rel test ct clean distclean prod devpkg prodpkg packages
REBAR=./rebar3

VSN  := $(shell git describe --always --tags)
DIST := dist

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
	@cp $$(ls -t _build/devpkg/rel/ssb/ssb-*.tar.gz | head -1) $(DIST)/erlbutt-dev-$(VSN).tar.gz
	@cd $(DIST) && shasum -a 256 erlbutt-dev-$(VSN).tar.gz > erlbutt-dev-$(VSN).tar.gz.sha256
	@echo "==> $(DIST)/erlbutt-dev-$(VSN).tar.gz"

## Production package: bundled ERTS, stripped beams, no source, no GUI stack.
prodpkg: compile
	$(REBAR) as prod tar
	@mkdir -p $(DIST)
	@cp $$(ls -t _build/prod/rel/ssb/ssb-*.tar.gz | head -1) $(DIST)/erlbutt-prod-$(VSN).tar.gz
	@cd $(DIST) && shasum -a 256 erlbutt-prod-$(VSN).tar.gz > erlbutt-prod-$(VSN).tar.gz.sha256
	@echo "==> $(DIST)/erlbutt-prod-$(VSN).tar.gz"

packages: devpkg prodpkg

all:  clean compile test prod
