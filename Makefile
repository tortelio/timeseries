###-----------------------------------------------------------------------------
### Build tools
###-----------------------------------------------------------------------------

REBAR := $(shell which rebar3)

$(REBAR):
	@echo Please install \`$@\' manually!
	@exit 1

###=============================================================================
### Targets
###=============================================================================

.PHONY: build
build: build-client build-server build-cli

.PHONY: test
test: test-client test-server

.PHONY: shell
shell: release-server
	./_build/default/rel/timeseries/bin/timeseries console

.PHONY: shell-dev
shell-dev: $(REBAR)
	$(REBAR) shell

.PHONY: cli
cli: ./timeseries_cli

###-----------------------------------------------------------------------------
### Server related targets
###-----------------------------------------------------------------------------

.PHONY: build-server
build-server: $(REBAR)
	$(REBAR) compile

.PHONY: build-cli
build-cli:
	$(REBAR) escriptize

.PHONY: test-server
test-server: build-cli $(REBAR)
	$(REBAR) dialyzer
	$(REBAR) ct --suite=apps/timeseries/test/integration_SUITE
	$(REBAR) ct --suite=apps/timeseries_cli/test/cli_SUITE

./timeseries_cli: build
	ln -s _build/default/bin/timeseries_cli $@

.PHONY: release-server
release-server: $(REBAR)
	$(REBAR) release
	$(REBAR) tar

###-----------------------------------------------------------------------------
### Server related targets
###-----------------------------------------------------------------------------

.PHONY: build-client
build-client:
	cd apps/timeseries/priv/monitor && \
	make install

.PHONY: test-client
test-client:
	cd apps/timeseries/priv/monitor && \
	make test
