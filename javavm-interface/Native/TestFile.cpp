/*
JVM-Bridge -- bridge from FP languages and others to the Java VM
Copyright (C) 2001 Ashley Yakeley <ashley@semantic.org>
This file (C) 2003 Thomas Pasch <thomas.pasch@gmx.de>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <stdio.h>
#include "File.cpp"

int main(int argc, char *argv[]) {
    printf("input: '%s'\n", argv[0]);
    File file(argv[0]);
    printf("norm: '%s'\n", file.normalizedName().c_str());
    printf("unix: '%s'\n", file.unixfiedName().c_str());
    printf("dir: %i file: %i\n", file.isDir(), file.isFile());
    //
    if (argc > 1) {
        printf("FilePath from '%s'\n", argv[1]);
        std::auto_ptr<FilePath> path(parseStringAsFilePath(argv[1]));
        printf("norm: '%s'\n", path->normalizedPath().c_str());
        printf("unix: '%s'\n", path->unixfiedPath().c_str());
        printf("check: %i\n", path->check());
    }
}
