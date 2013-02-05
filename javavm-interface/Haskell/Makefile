
TOP=$(shell pwd)/../..
include $(TOP)/mk/common.mk

default: install

javavm-interface.cabal: javavm-interface.cabal.m4 Foreign/JavaVM/Configure.hs
	m4 -DOSNAME="$(shell uname -s)"                              \
	   -DINCLUDEDIRS="$(JVMB_INCLUDEDIR) $(JVM_INCLUDES)"        \
	   -DLIBDIRS="$(JVMB_LIBDIR) $(JVM_LIBDIRS)"                 \
	   -DFRAMEWORKDIR=$(JVM_FRAMEWORKDIR)                        \
	   -DFRAMEWORK=$(JVM_FRAMEWORK)                              \
	   -DPROGRAM= $< > $@

Foreign/JavaVM/Configure.hs: Foreign/JavaVM/Configure.hs.m4
	m4 -DADDRESSSIZE="$(JVM_ADDRESSSIZE)" $< > $@

clean: javavm-interface.cabal
	cd test && make clean
	$(CABAL) $(CABAL_FLAGS) clean
	rm javavm-interface.cabal
	# rm -f Foreign/JavaVM/Configure.hs

haddock:
	$(CABAL) $(CABAL_FLAGS) haddock

install: javavm-interface.cabal
	$(CABAL) $(CABAL_FLAGS) install

check: install
	rm -f javavm-interface.cabal
	cd test && make check

.SECONDARY:

