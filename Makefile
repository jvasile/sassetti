BIN=sassetti
MAKE=make
SBCL_CONFIG=~/.sbclrc
INSTALL_TARGET=/usr/local/stow
STOW=stow

all: bin docs

bin/sassetti: sassetti.asd *.lisp
	@mkdir -p bin
	buildapp \
	  --load $(SBCL_CONFIG) \
	  --eval "(ql:quickload 'sassetti)" \
	  --entry sassetti::main \
	  --output bin/sassetti

bin: bin/sassetti

docs:
	$(MAKE) -C doc

doc: docs


dist: all
	$(MAKE) -C doc dist
	@mkdir -p dist/bin
	@cp bin/sassetti dist/bin

stow: bin
	@mkdir -p $(INSTALL_TARGET)/$(BIN)
	@cp -r bin $(INSTALL_TARGET)/$(BIN)
	@$(STOW) -d $(INSTALL_TARGET) -t $(INSTALL_TARGET)/.. $(BIN)

install: bin stow

clean:
	$(MAKE) -C doc clean
	rm -rf bin dist

