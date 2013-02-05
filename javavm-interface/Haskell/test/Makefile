build: HelloWorld.diff TestException.diff

%.diff: %.ref %.out
	diff -u $^

%.out: %
	./$< > $@

HelloWorld: HelloWorld.hs
	ghc -o $@ $< -XOverloadedStrings -hide-all-packages -package base -package witness -package javavm -package javavm-interface

TestException: TestException.hs
	ghc -o $@ $< -XOverloadedStrings -hide-all-packages -package base -package witness -package javavm -package javavm-interface

clean:
	rm -f HelloWorld TestException *.hi *.o *.out

check: test

.SECONDARY:
