default: build

datadir = /usr/lib/jvm-bridge/share
includedir = /usr/lib/jvm-bridge/include
libdir = /usr/lib/jvm-bridge/lib
bindir = /usr/lib/jvm-bridge/bin
JVMFILE			= $(datadir)/current.jvm

javavm-interface.cabal: javavm-interface.cabal.m4
	. $(JVMFILE); m4 \
		-DOSNAME="`uname -o`" \
		-DINCLUDEDIRS="$(includedir) $${JVM_INCLUDES}" \
		-DLIBDIRS="$(libdir) $${JVM_LIBDIRS}" \
		-DJVM_LDFLAGS="$${JVM_LDFLAGS}" \
		-DJVM_FRAMEWORKDIRS="$${JVM_FRAMEWORKDIRS}" \
		-DJVM_FRAMEWORKS="$${JVM_FRAMEWORKS}" \
		-DPROGRAM= $< > $@

Foreign/JavaVM/Configure.hs: Foreign/JavaVM/Configure.hs.m4
	. $(JVMFILE); m4 -DADDRESSSIZE="$${JVM_ADDRESSSIZE}" $< > $@

clean: javavm-interface.cabal
	cd test; make clean
	cabal clean
	rm javavm-interface.cabal
	rm -f Foreign/JavaVM/Configure.hs

configure: javavm-interface.cabal Foreign/JavaVM/Configure.hs
	cabal configure

build: configure
	cabal build

haddock: configure
	cabal haddock

install: build haddock
	cabal install --user

check: install
	cd test; make test

.SECONDARY:
