all:
	mkdir -p bin
	echo 'CM.make "$(shell pwd)/src/repl/repl.cm";' > bin/repl-make.sml
	echo 'SMLofNJ.exportFn ("$(shell pwd)/bin/.heapimg", Repl.main)' >> bin/repl-make.sml
	sml "$(shell pwd)/bin/repl-make.sml"
	echo "#!/bin/sh" > bin/repl
	mv $(shell pwd)/bin/.heapimg.* "$(shell pwd)/bin/.heapimg"
	echo "sml @SMLload $(shell pwd)/bin/.heapimg" >> bin/repl
	chmod a+x bin/repl

clean:
	rm -rf bin
	rm -rf .cm
	rm -rf src/abt/.cm
	rm -rf src/dynamics/.cm
	rm -rf src/parser/.cm
	rm -rf src/repl/.cm
	rm -rf src/statics/.cm
