
ERL=erl

.PHONY: all src test clean
all: src 

src:
	$(MAKE) -C src

test:
	$(MAKE) -C src test
	$(MAKE) -C test test

clean:
	$(MAKE) -C src clean
	$(MAKE) -C test clean


