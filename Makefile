
ERL=erl

.PHONY: all src test clean
all: src

src:
	cd src && $(ERL) -make all

test:
	cd test && $(ERL) -make all

clean:
	cd ebin && rm -f *.beam


