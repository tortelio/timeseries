.DEFAULT_GOAL := install

install:
	@echo "Compiling Index.elm to index.js..."
	elm make src/Index.elm --output assets/Index.js
	@echo "Compiling TimeseriesClient.elm to TimeseriesClient.js..."
	elm make src/TimeseriesClient.elm --output assets/TimeseriesClient.js

clean:
ifeq ($(OS),Windows_NT)
	@echo "Deleting index.js..."
	del assets\Index.js
	@echo "Deleting TimeseriesClient.js..."
	del assets\TimeseriesClient.js
else
	@echo "Deleting index.js..."
	rm assets/Index.js
	@echo "Deleting TimeseriesClient.js..."
	rm assets/TimeseriesClient.js
endif
