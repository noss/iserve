
ERL=erl
ERLC=erlc

.PHONY: all src clean
all:
	cp -f iserve.app ../ebin/
	$(ERL) -make all

test:
	cp -f iserve.app ../test/
	$(ERLC) -o ../test -DTEST *.erl

clean:
	rm -f ../ebin/*.beam
	rm -f ../ebin/*.app


