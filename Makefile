all: docs bin

bin:
	mkdir -p bin
	sbcl --eval "(progn (ql:quickload 'sassetti)(sb-ext:save-lisp-and-die \"bin/sassetti\" :executable t))"

docs:
	make -C doc

clean:
	make -C doc clean
	rm -rf bin

install:
	echo "No install target yet."
