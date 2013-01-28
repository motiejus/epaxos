REBAR ?= $(shell which rebar || ./rebar)
REBAR_URL ?= https://github.com/downloads/Motiejus/erlualib/rebar

.PHONY: clean compile test

compile: $(REBAR)
	$(REBAR) get-deps compile xref

clean:
	$(REBAR) clean

test: compile
	$(REBAR) eunit -v skip_deps=true

$(REBAR):
	erl -noshell -s inets start -s ssl start \
        -eval 'httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
        -s inets stop -s init stop
	chmod +x ./rebar
