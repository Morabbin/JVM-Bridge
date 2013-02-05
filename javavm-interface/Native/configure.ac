AC_DEFUN([CONF_CHECK_FILE],
[DIR=[$1]
FILE=[$2]
AC_MSG_CHECKING(for ${FILE})
if test -f "${DIR}/${FILE}" ; then
    echo "yes"
else
    echo "no"
    AC_MSG_ERROR([cannot find ${FILE} in ${DIR}])
fi
])

AC_DEFUN([CONF_FIND_FILE],
[DIR=[$1]
FILE=[$2]
AC_MSG_CHECKING(for ${FILE})
FDIR=`(find "${DIR}/" -name ${FILE} | sed 's/\/${FILE}//')`
if test -f "${DIR}/${FILE}" ; then
    echo "yes"
else
    echo "no"
    AC_MSG_ERROR([cannot find ${FILE} within ${DIR}])
fi
])

AC_DEFUN([CONF_CHECK_EXEC],
[FILE=[$1]
AC_MSG_CHECKING(for ${FILE})
if test -x "${FILE}" ; then
    echo "yes"
else
    echo "no"
    AC_MSG_ERROR([cannot find executable ${FILE}])
fi
])

AC_DEFUN([CHECK_JVM],
[THEJVM=[$1]
. "./${THEJVM}.jvm"
if test -e "${JVM_CHECKFILE}" ; then
    JVM="${THEJVM}"
    echo "yes"
else
    echo "no"
fi
])

AC_DEFUN([CHECK_JVMDIR],
[THEJVMDIR=[$1]
THEJVM=[$2]
JVM_BASEDIR="$(cd ${THEJVMDIR} ; pwd)/"
if test "z${CYGPATH}" != "z" ; then
    # hack for Cygwin
    JVM_BASEDIR=$(${CYGPATH} --windows ${JVM_BASEDIR} | tr \\\\ /)
fi
AC_MSG_CHECKING(for JVM ${THEJVM} in ${JVM_BASEDIR})
CHECK_JVM(${THEJVM})
])

#
AC_INIT([JVM-Bridge], [0.3.1])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([JVMBridge.cpp])
AM_INIT_AUTOMAKE
AC_CONFIG_HEADERS(autoheader.h)

AC_PREFIX_DEFAULT(/usr/local/jvm-bridge)

AC_ARG_WITH([jvm],[  --with-jvm=JVM          use Java VM type JVM],
[JVMFILTER="^\(.*\.\)*$(echo ${with_jvm}|sed -e 's/\./\\\./')\(\..*\)*$"],
[JVMFILTER="^.*$"])

AC_ARG_WITH([jvmdir],[  --with-jvmdir=DIR       use Java VM located in DIR],
[JVMDIR=${with_jvmdir}],[JVMDIR=""])

AC_ARG_ENABLE([debug],[  --enable-debug[=level]    switch on debugging level 0-4 (enable default 3)],
[
    if test "${enableval}" == "0" ; then
        DEBUG_CPPFLAGS=""
    else
        if test "${enableval}" == "no" ; then
            DEBUG_CPPFLAGS=""
        else
            if test "${enableval}" == "1" ; then
                DEBUG_CPPFLAGS="-DDEBUGINFO"
            else
                if test "${enableval}" == "2" ; then
                    DEBUG_CPPFLAGS="-DDEBUGINFO -DDEBUGVM -DDEBUGEXCEPTION"
                else
                    if test "${enableval}" == "3" ; then
                        DEBUG_CPPFLAGS="-DDEBUG"
                    else
                        if test "${enableval}" == "yes" ; then
                            DEBUG_CPPFLAGS="-DDEBUG"
                        else
                            if test "${enableval}" == "4" ; then
                                DEBUG_CPPFLAGS="-DDEBUG -DDEBUGTHREAD"
                            else
                                AC_MSG_ERROR([unrecognised --enable-debug flag: ${enableval}])
                            fi
                        fi
                    fi
                fi
            fi
        fi
    fi
],[DEBUG_CPPFLAGS=""])

AC_CHECK_SIZEOF(void*)
JVM_ADDRESSSIZE=$((${ac_cv_sizeof_voidp}*8))

# hack for Cygwin
CYGPATH=`type -p cygpath`

JVM=""
if test -r /etc/jvm ; then
    for TESTJVM in $(sed -e '/^#/ d; /^$/ d' /etc/jvm | grep "${with_jvm}" | sed -e 's|/\([^/]*\)$|/.\1.jinfo|') ; do
        AC_MSG_CHECKING([for JVM ${TESTJVM}])
        if test -r ${TESTJVM} ; then
            JVM_MACHINE="`uname -p`"
            if test "${JVM_MACHINE}" == "unknown" ; then
                if test "${JVM_ADDRESSSIZE}" == "64" ; then
                    JVM_MACHINE=amd64
                fi
                if test "${JVM_ADDRESSSIZE}" == "32" ; then
                    JVM_MACHINE=i386
                fi
            fi
            rm -f current.jvm
            echo "JVM_ADDRESSSIZE=${JVM_ADDRESSSIZE}" > current.jvm
            echo "JVM_MACHINE=${JVM_MACHINE}" >> current.jvm
            ./jinfo2jvm < ${TESTJVM} >> current.jvm
            JVM="${TESTJVM}"
            echo "yes"
            break 1
        else
            echo "no"
        fi
    done
fi

if test "z${JVM}" == "z" ; then
    for TESTJVM in $(sed -e '/^#.*/ d' jvm.list | grep "${JVMFILTER}") ; do
        JVM_CHECKFILE=""
        if test "z${JVMDIR}" == "z" ; then
            if test -f "${TESTJVM}.jvmdirs" ; then
                for JVMDIRS in $(sed -e '/^#.*/ d' "${TESTJVM}.jvmdirs") ; do
                    AC_MSG_CHECKING([for JVM ${TESTJVM} in any ${JVMDIRS}])
                    echo
                    for JVMDIR in $(ls -d ${JVMDIRS} 2>&- | sort -r) ; do
                        CHECK_JVMDIR(${JVMDIR},${TESTJVM})
                        if test -e "${JVM_CHECKFILE}" ; then
                            break 3
                        fi
                    done
                done
            else
                JVM_BASEDIR=""
                AC_MSG_CHECKING(for JVM ${TESTJVM})
                CHECK_JVM(${TESTJVM})
                if test -e "${JVM_CHECKFILE}" ; then
                    break
                fi
            fi
        else
            CHECK_JVMDIR(${JVMDIR},${TESTJVM})
            if test -e "${JVM_CHECKFILE}" ; then
                break
            fi
        fi
    done

    if test "z${JVM}" == "z" ; then
        if test "z${with_jvm}" == "z" ; then
            AC_MSG_ERROR([cannot find suitable JVM])
        else
            AC_MSG_ERROR([cannot find suitable JVM matching ${with_jvm}])
        fi
    fi

    rm -f current.jvm
    echo "JVM_ADDRESSSIZE=${JVM_ADDRESSSIZE}" > current.jvm
    echo 'JVM_BASEDIR="'${JVM_BASEDIR}'"' >> current.jvm
    cat "${JVM}.jvm" >> current.jvm
fi

. ./current.jvm
CONF_CHECK_EXEC(${JAVAC})
AC_SUBST(JAVAC)

CONF_CHECK_EXEC(${JAR})
AC_SUBST(JAR)

AC_PROG_CXX
LT_INIT

PP=`type -p cpp`
if test "z${PP}" == "z" ; then
    AC_PROG_CPP
    PP=${CPP}
fi
PPFLAGS="-xassembler-with-cpp"
CONF_CHECK_EXEC(${PP})
AC_SUBST(PP)
AC_SUBST(PPFLAGS)

DLLTOOL=`type -p dlltool`
#CONF_CHECK_EXEC(${DLLTOOL})
AC_SUBST(DLLTOOL)

CPPFLAGS=-DJVM_ADDRESSSIZE=${JVM_ADDRESSSIZE}

#AC_SUBST(CPPFLAGS)
AC_SUBST(DEBUG_CPPFLAGS)
AC_SUBST(JVM_CPPFLAGS)
AC_DEFINE_UNQUOTED(JVM_CLASSES, "$JVM_CLASSES",[DEFS inclusion])
AC_SUBST(JVM_FRAMEWORKDIRS)
AC_SUBST(JVM_FRAMEWORKS)
AC_SUBST(JVM_INCLUDES)
AC_SUBST(JVM_LIBDIRS)
AC_DEFINE_UNQUOTED(JVM_LIBDIRS,"$JVM_LIBDIRS",[DEFS inclusion])
AC_SUBST(JVM_LDFLAGS)
AC_SUBST(JAVACFLAGS)

AM_CONDITIONAL(DLL, test "$host_os" = cygwin -o "$host_os" = mingw32)
AC_DEFINE_UNQUOTED(MINGW,`if test $host_os = mingw32; then echo 1; else echo 0; fi`,[MinGW build])
AC_DEFINE_UNQUOTED(CYGWIN,`if test $host_os = cygwin; then echo 1; else echo 0; fi`,[Cygwin build])
AC_DEFINE_UNQUOTED(WIN32,`if test $host_os = cygwin -o $host_os = mingw32; then echo 1; else echo 0; fi`,[Win32 build])

AC_CONFIG_FILES([Makefile win32/Makefile])
AC_OUTPUT
