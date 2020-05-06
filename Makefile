# Build tools
REBAR := $(shell which rebar3)

$(REBAR):
	@echo Please install \`$@\' manually!
	@exit 1

shell: $(REBAR)
	$(REBAR) shell

prod-shell: build $(REBAR)
	./_build/default/rel/timeseries/bin/timeseries console

.PHONY: build
build: $(REBAR)
	$(REBAR) compile
	$(REBAR) escriptize
	$(REBAR) release
	$(REBAR) tar

.PHONY: test
test: build $(REBAR)
	$(REBAR) dialyzer
	$(REBAR) ct --suite=apps/timeseries/test/integration_SUITE
	$(REBAR) ct --suite=apps/timeseries_cli/test/cli_SUITE

.PHONY: cli
cli: ./timeseries_cli

./timeseries_cli: build
	ln -s _build/default/bin/timeseries_cli $@
