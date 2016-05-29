all:
	mkdir -p bin
	echo 'CM.make "$(shell pwd)/src/repl/repl.cm";' > bin/repl-make.sml
	echo 'SMLofNJ.exportFn ("$(shell pwd)/bin/.heapimg", Repl.main)' >> bin/repl-make.sml
	sml "$(shell pwd)/bin/repl-make.sml"
	echo "#!/bin/sh" > bin/repl1
	echo "sml @SMLload /Users/ayberkt/Developer/sml-system-t/bin/.heapimg.x86-darwin" >> bin/repl1
	chmod a+x bin/repl1

clean:
	rm -rf bin/.heapimg*
	rm -rf bin/repl-make.sml
	rm -rf src/repl/.cm
