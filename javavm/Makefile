

CABAL=cabal

default: build

# Building

clean:
	$(CABAL) clean

configure:
	$(CABAL) configure

build: configure
	$(CABAL) build --ghc-options="-Wall -Werror"

haddock: configure
	$(CABAL) haddock

install: build haddock
	$(CABAL) install --user

sdist: configure
	$(CABAL) sdist

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default configure build haddock install test sdist
