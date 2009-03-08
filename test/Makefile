
ERL=erl

.PHONY: all src clean

all: src

src:
	$(ERL) -pz ../../*/ebin -make 
	cp -f ../src/iserve.app .

clean:
	rm -f iserve.app *.beam *~

