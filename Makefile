all: docs

docs:
	make -C doc

clean:
	make -C doc clean

sterile:
	make -C doc sterile
