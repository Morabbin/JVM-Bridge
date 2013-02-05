
TOP=$(shell pwd)/..
include $(TOP)/mk/common.mk

default: build


ShowClasses.cabal: cabal.m4
	m4 -DPROGRAM=ShowClasses $< > $@

stage2.cabal: cabal.m4
	m4 -DPROGRAM= $< > $@

cabals: ShowClasses.cabal stage2.cabal

cabal-clean-%:
	rm -f *.cabal
	make $*.cabal
	make cabal-clean
	rm $*.cabal

cabal-build-%:
	rm -f *.cabal
	make $*.cabal
	make cabal-build
	rm $*.cabal

cabal-haddock-%:
	rm -f *.cabal
	make $*.cabal
	make cabal-haddock
	rm $*.cabal

cabal-install-%:
	rm -f *.cabal
	make $*.cabal
	make cabal-install
	rm $*.cabal

dist/build/ShowClasses/ShowClasses: cabal-build-ShowClasses

cabal-clean:
	$(CABAL) $(CABAL_FLAGS) clean

cabal-haddock:
	-$(CABAL) $(CABAL_FLAGS) haddock

cabal-install: 
	$(CABAL) $(CABAL_FLAGS) install

clean: cabal-clean-ShowClasses cabal-clean-stage2
	rm -rf Foreign/JavaVM/Invocation.hs Foreign/JavaVM/Lib/Header/* Foreign/JavaVM/Lib/Class_* ShowClasses *.hi *.o *.inplace *.classlist MakeClassModule MakeHeaderModule MakeJVMModule

ShowClasses: dist/build/ShowClasses/ShowClasses
	ln -sf $< $@

.SECONDARY:

MakeHeaderModule.inplace: MakeHeaderModule.m4
	m4 -Ddatadir=. $< > $@
	chmod a+x $@

MakeClassModule.inplace: MakeClassModule.m4
	m4 -Dbindir=. -Ddatadir=. -Djardir=$(JVMB_DATADIR) -Dlibdirs="$(JVMB_LIBDIR):$(JVM_LIBDIRS)" $< > $@
	chmod a+x $@

MakeClassModule: MakeClassModule.m4
	. $(JVMFILE); m4 -Dbindir=$(JVMB_BINDIR) -Ddatadir=$(JVMB_DATADIR) -Djardir=$(JVMB_DATADIR) -Dlibdirs="$(JVMB_LIBDIR):$(JVM_LIBDIRS)" $< > $@
	chmod a+x $@

MakeHeaderModule: MakeHeaderModule.m4
	m4 -Ddatadir=$(JVMB_DATADIR) $< > $@
	chmod a+x $@

MakeJVMModule: MakeJVMModule.m4
	m4 -Ddatadir=$(JVMB_DATADIR) $< > $@
	chmod a+x $@

%.classlist: All.classlist %.allowedclasses
	sed -n -e "`sed -e 's|\.|\\\.|g; s|^|/^|; s|$$|/ p;|' < $*.allowedclasses`" < $< > $@

All.classlist: $(JVMFILE)
	. $(JVMFILE); for ff in `echo $${JVM_CLASSES}`; do $${JAR} tf $${ff}; done | sed -n -e 's/^\([^-]*\).class$$/\1/ p' | sort -u | tr / . > $@

Foreign/JavaVM/Lib/Header/%.hs: %.classlist MakeHeaderModule.inplace Header.m4
	./MakeHeaderModule.inplace -module Foreign.JavaVM.Lib.Header.$* $< > $@

Foreign/JavaVM/Lib/Class_%.hs: MakeClassModule.inplace Java.classlist ShowClasses Class.m4
	./MakeClassModule.inplace -import Foreign.JavaVM.Lib.Header.Java `echo $* | sed -e 's/_/\./g; s/\.\./_/g'` > $@

Foreign/JavaVM/Invocation.hs: Foreign/JavaVM/Invocation.hs.m4
	m4 -DBRIDGEJAR="$(JVMB_DATADIR)/JVMBridge.jar" $< > $@

otherstuff:	\
	Foreign/JavaVM/Lib/Header/Java.hs	\
	Foreign/JavaVM/Lib/Header/JavaX.hs	\
	Foreign/JavaVM/Lib/Header/OMG.hs	\
	Foreign/JavaVM/Lib/Header/Misc.hs	\
	Foreign/JavaVM/Lib/Class_java_lang_Thread.hs	\
	Foreign/JavaVM/Lib/Class_java_lang_Object.hs	\
	Foreign/JavaVM/Lib/Class_java_lang_Class.hs	\
	Foreign/JavaVM/Invocation.hs

extrastuff: MakeClassModule ShowClasses Class.m4 MakeJVMModule JVM.m4 MakeHeaderModule Header.m4 otherstuff

build: extrastuff cabal-build-stage2

haddock: otherstuff cabal-haddock-stage2

install: extrastuff cabal-install-stage2
	sudo install -D -s ShowClasses $(JVMB_BINDIR)/ShowClasses
	sudo install -D Class.m4 $(JVMB_DATADIR)/Class.m4
	sudo install -D MakeClassModule $(JVMB_BINDIR)/MakeClassModule
	sudo install -D JVM.m4 $(JVMB_DATADIR)/JVM.m4
	sudo install -D MakeJVMModule $(JVMB_BINDIR)/MakeJVMModule
	sudo install -D Header.m4 $(JVMB_DATADIR)/Header.m4
	sudo install -D MakeHeaderModule $(JVMB_BINDIR)/MakeHeaderModule
