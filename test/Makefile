# [make test] runs two tests in parallel, namely
# a test of the functor [Make] and
# a test of the functor [MakeWithData].

.PHONY: test
test:
	@ make clean
	@ dune build @all
	@ parallel \
	  --tag --line-buffer \
	  make -C {} random_nodep \
	  ::: \
	  Make \
	  Data \

.PHONY: clean
clean:
	@ git clean -dfX
