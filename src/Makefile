
ERL=erl
ERLC=erlc

.PHONY: all src clean
all:
	$(ERL) -make all

test:
	$(ERLC) -o ../test -DTEST *.erl

clean:
	rm -f ../ebin/*.beam


