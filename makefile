BIN = arith

test: $(BIN)
	@echo "Running tests..."
	@./test.sh tests.txt

run: $(BIN)/arith
	@echo "Running..."
	@./$(BIN)

$(BIN): language.hs
	@mkdir -p
	ghc language.hs -o $(BIN)
