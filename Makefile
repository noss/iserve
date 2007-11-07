
ERL=erl

.PHONY: all src test clean
all: src test

src:
	cd src && $(ERL) -make all

test:
	cd test && $(ERL) -pa ../ebin -make all

clean:
	cd ebin && rm -f *.beam


