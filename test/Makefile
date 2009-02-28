
ERL=erl

.PHONY: all 

all:
	$(ERL) -pz ../../*/ebin -make 
	cp -f ../src/iserve.app .

clean:
	rm -f iserve.app *.beam *~


