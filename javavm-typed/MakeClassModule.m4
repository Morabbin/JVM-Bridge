dnl JVM-Bridge -- bridge from FP languages and others to the Java VM
dnl Copyright (C) 2001-2002 Ashley Yakeley <ashley@semantic.org>
dnl
dnl This library is free software; you can redistribute it and/or
dnl modify it under the terms of the GNU Lesser General Public
dnl License as published by the Free Software Foundation; either
dnl version 2.1 of the License, or (at your option) any later version.
dnl
dnl This library is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl Lesser General Public License for more details.
dnl
dnl You should have received a copy of the GNU Lesser General Public
dnl License along with this library; if not, write to the Free Software
dnl Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
dnl
dnl
#!/bin/sh
changequote([[[[,]]]])dnl so m4 doesn't mess with the backquotes
set -e
classpath=""
classnames=""
header=""
modulename=""
imports=""
while :
do
	case $# in
	0)	break;;
	*)	case "$1" in
		-module)	[[[[shift]]]]; modulename="$1";;
		-header)	header="1";;
		-import)	[[[[shift]]]]; imports="${imports} $1";;
		-classpath)	[[[[shift]]]]; classpath="$1";;
		-cp)		[[[[shift]]]]; classpath="$1";;
		-*)			echo "$0: unknown option ($1)" 1>&2; exit 1;;
		*)			classnames="${classnames} $1";;
		esac
		[[[[shift]]]];;
	esac
done

export PATH=$PATH:"libdirs"
case ${modulename} in
"") modulename=`echo ${classnames} | sed -e 's/[ 	]*\([^ 	][^ 	]*\).*/\1/g; s/_/__/g; s/\./_/g; s/^/Foreign.JavaVM.Lib.Class_/'`;;
esac
tempm4=`mktemp /tmp/MakeClassModule.XXXXXX`
echo ${classnames} |
bindir/ShowClasses jardir/JVMBridge.jar ${classpath} > ${tempm4}
m4 -DCLASSESFILE="${tempm4}" -DMODULENAME=${modulename} -DHEADER=${header} -DIMPORTS="${imports}" datadir/Class.m4
rm ${tempm4}

