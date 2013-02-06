
TOP=$(shell pwd)
include $(TOP)/mk/common.mk

GNU_LIBTOOLIZE=glibtoolize
# GNU_LIBTOOLIZE=libtoolize

JAVAVM_IFC=javavm-interface
JAVAVM_NATIVE=$(JAVAVM_IFC)/Native
JAVAVM_HASKELL=$(JAVAVM_IFC)/Haskell
JAVAVM=javavm
JAVAVM_TYPES=javavm-typed

default: install.javavm-typed

.$(JAVAVM).built: $(JAVAVM)/$(JAVAVM).cabal
	rm -rf $@
	( cd $(JAVAVM) && $(CABAL) $(CABAL_FLAGS) install )

install.javavm: .$(JAVAVM).built
	touch .$(JAVAVM).built

.javavm-interface.Native.pre-build: $(JAVAVM_NATIVE)/Makefile.am
	rm -rf $@
	( cd $(JAVAVM_NATIVE)                                       && \
	  $(GNU_LIBTOOLIZE)                                         && \
	  aclocal                                                   && \
	  autoconf                                                  && \
	  autoheader                                                && \
	  automake -a                                               && \
	  autoheader                                                && \
	  ./configure --prefix=$(JVMB_PREFIX_DIR)                      )
	touch $@

install.javavm-interface.Native: .javavm-interface.Native.pre-build
	$(MAKE) -C $(JAVAVM_NATIVE)
	$(MAKE) -C $(JAVAVM_NATIVE) install

install.javavm-interface.Haskell: install.javavm install.javavm-interface.Native
	$(MAKE) -C $(JAVAVM_HASKELL) install

install.javavm-typed: install.javavm-interface.Haskell
	$(MAKE) -C $(JAVAVM_TYPES)
	$(MAKE) -C $(JAVAVM_TYPES) postinstall

clean:
	$(MAKE) -C $(JAVAVM_TYPES) clean
	$(MAKE) -C $(JAVAVM_HASKELL) clean
	$(MAKE) -C $(JAVAVM) clean
	rm -rf $(SANDBOX)
	rm -rf .$(JAVAVM_NATIVE).pre-build
	rm -rf .$(JAVAVM).built
