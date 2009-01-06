
ERL=erl

.PHONY: all src test clean
all: src 

src:
	$(MAKE) -C src

test:
	$(MAKE) -C test 

clean:
	rm -f ebin/*
	$(MAKE) -C src clean
	$(MAKE) -C test clean


