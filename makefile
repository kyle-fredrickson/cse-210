BIN = arith

$(BIN): language.hs
	ghc language.hs -o $(BIN)

run: $(BIN)
	@echo "Running..."
	@./$(BIN)