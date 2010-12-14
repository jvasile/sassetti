BIN=sassetti
SBCL_CONFIG=~/.sbclrc
INSTALL_TARGET=/usr/local/stow
MAKE=make
STOW=stow


## Catch-all tagets
default: bin docs dist
all: default test

## Content targets
bin/sassetti: sassetti.asd *.lisp Makefile
	@mkdir -p bin
	buildapp \
	  --load $(SBCL_CONFIG) \
	  --eval "(ql:quickload 'sassetti)" \
	  --entry sassetti::main \
	  --output bin/$(BIN) || \
	bash -c 'if [ "`file bin/$(BIN)`" == "bin/$(BIN): ASCII text" ]; then rm -f bin/$(BIN); fi'
bin: bin/sassetti

bin/sassetti-test: sassetti-test.asd *.lisp Makefile
	@mkdir -p bin
	buildapp \
	  --load $(SBCL_CONFIG) \
	  --eval "(ql:quickload 'sassetti-test)" \
	  --entry sassetti::test-all \
	  --output bin/$(BIN)-test || \
	bash -c 'if [ "`file bin/$(BIN)-test`" == "bin/$(BIN)-test: ASCII text" ]; then rm -f bin/$(BIN)-test; fi'
test: bin/sassetti-test

docs:
	$(MAKE) -C doc
doc: docs


## Distribution targets
dist: default
	$(MAKE) -C doc dist
	@mkdir -p dist/bin
	@cp bin/sassetti dist/bin


## Install targets
stow: default
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
	@echo Run \'make stow\' to install via GNU stow.  
	@echo You might need to install and configure stow first \(\'apt-get install stow\'\).

clean:
	$(MAKE) -C doc clean
	rm -rf bin dist

