BIN=sassetti
SBCL_CONFIG=~/.sbclrc
INSTALL_TARGET=/usr/local/stow
MAKE=make
STOW=stow



all: bin docs

bin/sassetti: sassetti.asd *.lisp Makefile
	@mkdir -p bin
	buildapp \
	  --load $(SBCL_CONFIG) \
	  --eval "(progn (ql:quickload 'sassetti)) (setf *debugger-hook* 'sassetti::debug-ignore))" \
	  --entry sassetti::main \
	  --output bin/$(BIN) || \
	bash -c 'if [ "`file bin/$(BIN)`" == "bin/$(BIN): ASCII text" ]; then rm -f bin/$(BIN); fi'

bin: bin/sassetti

docs:
	$(MAKE) -C doc
doc: docs


dist: all
	$(MAKE) -C doc dist
	@mkdir -p dist/bin
	@cp bin/sassetti dist/bin

stow: bin dist
	rm -rf $(INSTALL_TARGET)/$(BIN)/*
	mkdir -p $(INSTALL_TARGET)/$(BIN)/share/man/man1 $(INSTALL_TARGET)/$(BIN)/share/doc/$(BIN)
	cp -r bin $(INSTALL_TARGET)/$(BIN)
	cp dist/doc/sassetti.1 $(INSTALL_TARGET)/$(BIN)/share/man/man1
	cp dist/doc/* $(INSTALL_TARGET)/$(BIN)/share/doc/$(BIN)
	$(STOW) --restow -d $(INSTALL_TARGET) -t $(INSTALL_TARGET)/.. $(BIN)
	mandb

unstow:
	$(STOW) --delete -d $(INSTALL_TARGET) -t $(INSTALL_TARGET)/.. $(BIN)
	rm -rf $(INSTALL_TARGET)/$(BIN)

install:
	@echo Run \'make stow\' to install via GNU stow.  You might need to \'apt-get install stow\' first.

clean:
	$(MAKE) -C doc clean
	rm -rf bin dist

