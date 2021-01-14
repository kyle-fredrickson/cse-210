BIN = bin

test: $(BIN)/main
	@echo "Running tests..."
	@./test.sh tests.txt

run: $(BIN)/main
	@echo "Running..."
	@./$(BIN)/main

$(BIN)/main: language.hs
	@mkdir -p $(BIN)
	ghc language.hs -outputdir $(BIN) -o $(BIN)/arith
