.PHONY: compile rel test
export LIBRARY_PATH=/opt/homebrew/lib 
export CPATH=/opt/homebrew/include
REBAR=./rebar3

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test: compile
	unzip -n -o testdata.zip
	$(REBAR) eunit
	rm -Rf testdata

rel: compile
	$(REBAR) release

prod: compile
	$(REBAR) as prod release

