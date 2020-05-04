.DEFAULT_GOAL := install

install:
	@echo "Creating directories..."
	mkdir -p this/assets
	@echo "Copying html files..."
	cp src/index.html this/index.html
	cp src/TimeseriesClient.html this/TimeseriesClient.html
	@echo "Copying css file..."
	cp assets/TimeseriesClient.css this/assets/TimeseriesClient.css
	@echo "Compiling Index.elm to index.js..."
	elm make src/Index.elm --output this/assets/Index.js
	@echo "Compiling TimeseriesClient.elm to TimeseriesClient.js..."
	elm make src/TimeseriesClient.elm --output this/assets/TimeseriesClient.js
clean:
	@echo "Deleting generated files, directories..."
	rm -rf this
