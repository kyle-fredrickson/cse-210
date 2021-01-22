BIN = while
FILE = Comm

$(BIN): $(FILE).hs
	ghc $(FILE).hs -o $(BIN)

run: $(BIN)
	@echo "Running..."
	@./$(BIN)

clean:
	@rm *.hi
	@rm *.o
