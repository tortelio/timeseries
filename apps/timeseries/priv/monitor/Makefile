.DEFAULT_GOAL := install

install:
	@echo "Creating directories..."
	mkdir -p dist/assets
	@echo "Copying html files..."
	cp src/index.html dist/index.html
	cp src/TimeseriesClient.html dist/TimeseriesClient.html
	@echo "Copying css file..."
	cp assets/TimeseriesClient.css dist/assets/TimeseriesClient.css
	@echo "Compiling Index.elm to index.js..."
	elm make src/Index.elm --output dist/assets/Index.js
	@echo "Compiling TimeseriesClient.elm to TimeseriesClient.js..."
	elm make src/TimeseriesClient.elm --output dist/assets/TimeseriesClient.js
clean:
	@echo "Deleting generated files, directories..."
	rm -rf dist

test:
	@echo "Testing"
	elm-test tests/TimeseriesClientTest.elm
