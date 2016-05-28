all:
	mkdir -p bin
	echo 'CM.make "$(shell pwd)/src/repl/repl.cm";' > bin/repl-make.sml
	echo 'SMLofNJ.exportFn ("$(shell pwd)/bin/.heapimg", Repl.main)' >> bin/repl-make.sml
	sml "$(shell pwd)/bin/repl-make.sml"

clean:
	rm -rf bin/.heapimg*
	rm -rf bin/repl-make.sml
