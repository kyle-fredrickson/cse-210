BIN = bin

test: $(BIN)/arith
	@echo "Running tests..."
	@./test.sh tests.txt

run: $(BIN)/arith
	@echo "Running..."
	@./$(BIN)/arith

$(BIN)/arith: language.hs
	@mkdir -p $(BIN)
	ghc language.hs -outputdir $(BIN) -o $(BIN)/arith
