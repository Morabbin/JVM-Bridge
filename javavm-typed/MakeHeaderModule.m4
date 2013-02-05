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
classlistfiles=""
jarfiles=""
modulename="Header_Project"
while :
do
	case $# in
	0)	break;;
	*)	case "$1" in
		-module)	[[[[shift]]]]; modulename="$1";;
		-jar)		[[[[shift]]]]; jarfiles="${jarfiles} $1";;
		-*)			echo "$0: unknown option ($1)" 1>&2; exit 1;;
		*)			classlistfiles="${classlistfiles} $1";;
		esac
		[[[[shift]]]];;
	esac
done

tempm4=`mktemp /tmp/MakeHeaderModule.XXXXXX`
	for jf in ${jarfiles}
	do jar tf "${jf}" | sed -n -e 's|^\([^-]*\)\.class$|\1| p' | tr / . >> ${tempm4}
	done
	case ${jarfiles} in
	"") cat ${classlistfiles} | tr / .  >> ${tempm4};;
	*)
		case ${classlistfiles} in
		"") ;;
		*) cat ${classlistfiles} | tr / .  >> ${tempm4};;
		esac
	esac
m4 -DCLASSLISTFILE=${tempm4} -DMODULENAME=${modulename} datadir/Header.m4
rm ${tempm4}
