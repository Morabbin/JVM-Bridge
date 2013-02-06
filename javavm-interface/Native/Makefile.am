
TOP=$(shell pwd)/../..
include $(TOP)/mk/common.mk

if DLL
SUBDIRS=win32
endif

include_HEADERS= JVMBridge.h ExecuteFunctionControl.h ValueList.h JVMInvocation.h
lib_LTLIBRARIES	= libJVMBridge.la libJVMInvocation.la
data_DATA= JVMBridge.jar
libJVMBridge_la_SOURCES= JVMBridge.cpp ValueList.cpp ExecuteFunction.cpp Debug.hpp JVMBridge.h CallbackSupport.h ValueListPrivate.hpp ValueList.h Configure.h ExecuteFunctionControl.h
libJVMInvocation_la_SOURCES= JVMInvocation.cpp File.cpp Debug.hpp JVMInvocation.h File.hpp Configure.h
libJVMBridge_la_LDFLAGS= -Wl,-lstdc++
libJVMInvocation_la_LDFLAGS= -Wl,-lstdc++
INCLUDES=$(JVM_INCLUDE_FLAGS)

TestJVMBridge_SOURCES=TestJVMBridge.cpp JVMInvocation.h JVMBridge.h CallbackSupport.h Debug.hpp Configure.h
TestJVMBridge_LDADD=libJVMInvocation.la libJVMBridge.la $(JVM_LDFLAGS)
TestJVMBridge_LDFLAGS=$(JVM_LDFLAGS)
TestFile_SOURCES=TestFile.cpp
FindClasses_SOURCES=FindClasses.cpp JVMInvocation.h JVMBridge.h CallbackSupport.h Debug.hpp Configure.h
FindClasses_LDADD=libJVMInvocation.la libJVMBridge.la $(JVM_LDFLAGS)
FindClasses_LDFLAGS=$(JVM_LDFLAGS)

PACKAGE_SLASH=org/semantic/jvmbridge/
# JAVACFLAGS=$(JAVACFLAGS)
ACLOCAL_AMFLAGS =-I m4
AM_CPPFLAGS=$(JVM_CPPFLAGS) @DEBUG_CPPFLAGS@
AM_CXXFLAGS=-O3 -W -Wall -fno-rtti

check_PROGRAMS= TestJVMBridge TestFile FindClasses

check: TestJVMBridge.out TestFile.out FindClasses.out

TestJVMBridge.out: TestJVMBridge JVMBridge.jar TestApp.jar
	./TestJVMBridge ./JVMBridge.jar:./TestApp.jar org/semantic/test/TestApp

TestFile.out: TestFile
	./$<

FindClasses.out: FindClasses JVMBridge.jar
	./$< ./JVMBridge.jar

MAINTAINERCLEANFILES = config.log config.status config.guess config.sub configure install-sh ltmain.sh mkinstalldirs missing aclocal.m4 Makefile.in depcomp 
MOSTLYCLEANFILES = ExecuteFunction.java *.class
#MOSTLYCLEANFILES += org
CLEANFILES = *.jar
DISTCLEANFILES =

ExecuteFunction.lo: ExecuteFunctionControl.h Debug.hpp
ValueList.lo: ValueListPrivate.hpp ValueList.h Debug.hpp
JVMBridge.lo: JVMBridge.h ValueListPrivate.hpp ValueList.h Debug.hpp

TestApp.java: TestApp.java-pp Configure.h autoheader.h
	$(PP) $(PPFLAGS) -P $(CPPFLAGS) -DLIBNAME="JVMBridge" $< | grep -v '^typedef' > $@

ExecuteFunction.java: ExecuteFunction.java-pp Configure.h autoheader.h
	$(PP) $(PPFLAGS) -P $(CPPFLAGS) -DLIBNAME="JVMBridge" $< | grep -v '^typedef' > $@

$(PACKAGE_SLASH)ExecuteFunction.class: ExecuteFunction.java
	$(JAVAC) $(JAVACFLAGS) -d . $<

JVMBridge.jar: $(PACKAGE_SLASH)ExecuteFunction.class
	$(JAR) cvf $@ $(PACKAGE_SLASH)ExecuteFunction*.class

org/semantic/test/TestApp.class: TestApp.java
	$(JAVAC) $(JAVACFLAGS) -d . $<

TestApp.jar: org/semantic/test/TestApp.class
	$(JAR) cvf $@ $^

install-dll: dll
	$(mkinstalldirs) $(DESTDIR)$(libdir)
	-$(LIBTOOL) --mode=uninstall rm -f $(DESTDIR)$(libdir)/libJVMBridge.*a
	-$(LIBTOOL) --mode=uninstall rm -f $(DESTDIR)$(libdir)/libJVMInvocation.*a
	$(LIBTOOL) --mode=install $(INSTALL) $(INSTALL_STRIP_FLAG) libJVMBridge_imp.a $(DESTDIR)$(libdir)
	$(LIBTOOL) --mode=install $(INSTALL) $(INSTALL_STRIP_FLAG) libJVMInvocation_imp.a $(DESTDIR)$(libdir)
	$(LIBTOOL) --mode=install $(INSTALL) $(INSTALL_STRIP_FLAG) JVMBridge.dll $(DESTDIR)$(libdir)
	$(LIBTOOL) --mode=install $(INSTALL) $(INSTALL_STRIP_FLAG) JVMInvocation.dll $(DESTDIR)$(libdir)

dll: JVMBridge.dll JVMInvocation.dll

DLL_LIBADD=-Lwin32 -ljvm_imp -L$$ghc_home -lHShaskell98 -lHSrts -lHSbase -lHSrts -lHSbase_cbits -lgmp -lwsock32

JVMBridge.dll: $(libJVMBridge_la_OBJECTS) $(libJVMBridge_la_DEPENDENCIES)
	@objects="`echo '$(libJVMBridge_la_OBJECTS)' | sed -e 's/\.lo/\.o/g'`"; \
	ghc_home="`which ghc | sed -e 's/\/bin\/ghc//'`"; \
	echo "$(CXX) -shared $$objects ../Haskell/CallbackSupport.o ../Haskell/RawCallback_stub.o $(DLL_LIBADD) -o $@"; \
	$(CXX) -shared $$objects ../Haskell/CallbackSupport.o ../Haskell/RawCallback_stub.o $(DLL_LIBADD) \
	  -Wl,--out-implib,libJVMBridge_imp.a -o $@

#    ghc -v2 --mk-dll -package lang -package concurrent -package haskell98 \
#      -package-name javavm -o $@ $$objects  ../Haskell/CallbackSupport.o ../Haskell/*_stub.o \
#      -L/d/MinGW/lib -lstdc++ -Ld:/MinGW/lib/gcc-lib/mingw32/3.2.3 -lgcc -L$(prefix)/lib -ljvm_imp

JVMInvocation.dll: $(libJVMInvocation_la_OBJECTS) $(libJVMInvocation_la_DEPENDENCIES)
	@objects="`echo '$(libJVMInvocation_la_OBJECTS)' | sed -e 's/\.lo/\.o/g'`"; \
	ghc_home="`which ghc | sed -e 's/\/bin\/ghc//'`"; \
	echo "$(CXX) -shared $$objects -Lwin32 -ljvm_imp -o $@"; \
	$(CXX) -shared $$objects -Lwin32 -ljvm_imp \
	  -Wl,--out-implib,libJVMInvocation_imp.a -o $@
