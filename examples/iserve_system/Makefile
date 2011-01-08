

.PHONY: all src test clean
all: src 
	../../rel.escript iserve_system "IServe system" "1.0" > iserve_system.rel
	erl -pa ebin -noinput -eval 'systools:make_script("iserve_system").' -s erlang halt

src:
	cd src && erl -make

clean:
	rm -f ebin/*.beam

