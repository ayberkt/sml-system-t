all:
	mkdir -p bin
	echo 'CM.make "$(shell pwd)/src/repl/repl.cm";' > bin/repl-make.sml
	echo 'SMLofNJ.exportFn ("$(shell pwd)/bin/.heapimg", Repl.main)' >> bin/repl-make.sml
	echo 'CM.make "$(shell pwd)/src/read_file/read-file.cm";' > bin/file-make.sml
	echo 'SMLofNJ.exportFn ("$(shell pwd)/bin/.system-t.heapimg", FileReader.main)' >> bin/file-make.sml
	sml "$(shell pwd)/bin/repl-make.sml"
	sml "$(shell pwd)/bin/file-make.sml"
	echo "#!/bin/sh" > bin/repl
	mv $(shell pwd)/bin/.heapimg.* "$(shell pwd)/bin/.heapimg"
	echo "#!/bin/sh" > bin/system-t
	mv $(shell pwd)/bin/.system-t.heapimg.* "$(shell pwd)/bin/.system-t.heapimg"
	echo "sml @SMLload $(shell pwd)/bin/.heapimg" >> bin/repl
	echo 'sml @SMLload $(shell pwd)/bin/.system-t.heapimg $$1' >> bin/system-t
	chmod a+x bin/repl
	chmod a+x bin/system-t

clean:
	rm -rf bin
	rm -rf .cm
	rm -rf src/abt/.cm
	rm -rf src/dynamics/.cm
	rm -rf src/parser/.cm
	rm -rf src/repl/.cm
	rm -rf src/statics/.cm
