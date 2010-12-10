MAKE=@`which make`

all: docs bin

bin:
	mkdir -p bin
	sbcl --eval "(progn (ql:quickload 'sassetti)(sb-ext:save-lisp-and-die \"bin/sassetti\" :executable t))"

docs:
	$(MAKE) -C doc

doc: docs

clean:
	$(MAKE) -C doc clean
	rm -rf bin

install:
	@echo "No install target yet."
