
ERL=erl

.PHONY: all src test clean
all: src 
	./app.escript `cat LATEST_VERSION` > ebin/iserve.app

src:
	$(MAKE) -C src

clean:
	rm -f ebin/*
	rm -f examples/*.beam
	rm -f test/*.beam test/*.html
	$(MAKE) -C src clean
	$(MAKE) -C test clean

examples: all
	$(MAKE) -C examples/iserve_system

test.spec: test.spec.in
	cat test.spec.in | sed -e "s,@PATH@,$(PWD)," > $(PWD)/test.spec
cover.spec: cover.spec.in
	cat cover.spec.in | sed -e "s,@PATH@,$(PWD)," > $(PWD)/cover.spec

test: test.spec cover.spec src
	mkdir -p etc/log
	run_test -pa $PWD/test -spec test.spec -cover cover.spec

cover: 
	@(cd test && erl -make )
	@erl -noshell \
         -eval 'file:set_cwd(test).' \
         -eval 'cover:compile_beam_directory().' \
         -eval 'iserve_master_test:test().' \
         -eval '[cover:analyse_to_file(M, [html]) || M <- cover:modules()].' \
         -s init stop

