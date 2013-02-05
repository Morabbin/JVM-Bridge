
TOP=$(shell pwd)
include $(TOP)/mk/common.mk

default: install.javavm-typed

.PHONY: install.javavm install.javavm-interface.Native
install.javavm:
	( cd javavm && $(CABAL) $(CABAL_FLAGS) install )

install.javavm-interface.Native:
	( cd javavm-interface/Native;                                  \
	  glibtoolize;                                                 \
	  aclocal;                                                     \
	  autoconf;                                                    \
	  autoheader;                                                  \
	  automake -a;                                                 \
	  autoheader;                                                  \
	  ./configure --prefix=$(JVMB_PREFIX_DIR);                     \
	  make && make install                                         )


install.javavm-interface.Haskell: install.javavm install.javavm-interface.Native
	( cd javavm-interface/Haskell                               && \
	  make install                                                 )

install.javavm-typed: install.javavm-interface.Haskell
	( cd javavm-typed;                                             \
	  glibtoolize;                                                 \
	  aclocal;                                                     \
	  autoconf;                                                    \
	  automake -a;                                                 \
	  ./configure --prefix=$(JVMB_PREFIX_DIR);                     \
	  make;                                                        \
	  sudo make postinstall                                        )

clean:
	cd javavm-typed && make clean
	cd javavm-interface/Haskell && make clean
	cd javavm && make clean
