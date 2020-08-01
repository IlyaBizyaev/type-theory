TASK=A
HC=ghc

.PHONY: all gen run clean pack

all:
	alex src/Lexer.x -o src/Lexer.hs
	happy src/Parser.y -o src/Parser.hs
	$(HC) -i./src -tmpdir . ./src/Main.hs -o parser

run:
	./parser

clean:
	rm -rf src/*.o src/*.hi src/Lexer.hs src/Parser.hs parser

pack:
	zip $(TASK).zip -r Makefile src
