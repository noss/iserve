
ERL=erl

.PHONY: all src test clean
all: src 

src:
	$(MAKE) -C src

clean:
	rm -f ebin/*
	$(MAKE) -C src clean
	$(MAKE) -C test clean

test.spec: test.spec.in
	cat test.spec.in | sed -e "s,@PATH@,$(PWD)," > $(PWD)/test.spec
cover.spec: cover.spec.in
	cat cover.spec.in | sed -e "s,@PATH@,$(PWD)," > $(PWD)/cover.spec

test: test.spec cover.spec src
	mkdir -p etc/log
	run_test -pa $PWD/test -spec test.spec -cover cover.spec


