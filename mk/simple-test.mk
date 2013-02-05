
%.diff: %.ref %.out
	diff -u $^ > $@

%.out:
	./dist/build/$*/$* > $@ 2> $*.err

build:
	$(CABAL) $(CABAL_FLAGS) install

check: build $(TEST_PROG).diff

clean::
	$(CABAL) $(CABAL_FLAGS) clean
	rm -rf $(TEST_PROG).{out,err,diff}

.SECONDARY:

