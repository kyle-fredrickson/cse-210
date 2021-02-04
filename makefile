BIN = while-ss
FILE = EvalCommSmallStep

$(BIN): $(FILE).hs
	ghc $(FILE).hs -o $(BIN)

run: $(BIN)
	@echo "Running..."
	@./$(BIN)

clean:
	@rm *.hi
	@rm *.o
	@rm $(BIN)
