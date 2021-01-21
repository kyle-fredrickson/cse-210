BIN = arith

$(BIN): Arith.hs
	ghc Arith.hs -o $(BIN)

run: $(BIN)
	@echo "Running..."
	@./$(BIN)