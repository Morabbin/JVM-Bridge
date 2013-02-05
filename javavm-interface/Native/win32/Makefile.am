DLLTOOL=@DLLTOOL@
MAINTAINERCLEANFILES= Makefile.in
CLEANFILES=libjvm.a
lib_LIBRARIES=libjvm_imp.a

#install-exec-hook: libjvm_imp.a 
#    $(mkinstalldirs) $(DESTDIR)$(libdir)
#    $(LIBTOOL) --mode=install $(INSTALL) $(INSTALL_STRIP_FLAG) libjvm_imp.a $(DESTDIR)$(libdir)

all: libjvm_imp.a

libjvm_imp.a: jvm.def
    $(DLLTOOL) -k --def jvm.def --dllname jvm.dll --output-lib $@
