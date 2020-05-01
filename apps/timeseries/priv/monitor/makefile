.DEFAULT_GOAL := install

install:
	@echo "Compiling Index.elm to index.js..."
	elm make src/Index.elm --output assets/Index.js
	@echo "Compiling TimeseriesClient.elm to TimeseriesClient.js..."
	elm make src/TimeseriesClient.elm --output assets/TimeseriesClient.js

clean:
	@echo "Deleting index.js..."
	rm assets/Index.js
	@echo "Deleting TimeseriesClient.js..."
	rm assets/TimeseriesClient.js
