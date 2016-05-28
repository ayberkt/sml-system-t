all:
	mkdir -p bin
	sml src/repl/repl-make.sml

clean:
	rm -rf bin/.heapimg*
