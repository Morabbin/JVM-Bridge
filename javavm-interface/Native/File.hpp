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

#ifndef __File_hpp__
#define __File_hpp__

#include "Configure.h"
#include <sys/stat.h>
#include <memory>
#include <string>
#include <list>

#define UNIX_PATH_SEPERATOR ":"
#define UNIX_FILE_SEPERATOR "/"

#if WIN32
#define PATH_SEPERATOR ";"
#define FILE_SEPERATOR "\\"
#else
#define PATH_SEPERATOR UNIX_PATH_SEPERATOR
#define FILE_SEPERATOR UNIX_FILE_SEPERATOR
#endif

class File {
    private:
        std::string normalizedFile_;
        std::string unixfiedFile_;
        struct stat attr_;
    public:
        File(const File& file);
        explicit File(const char* filename);
        explicit File(const std::string filename);

        const std::string normalizedName() const;
        const std::string unixfiedName() const;
        bool isFile() const;
        bool isDir() const;
};

class FilePath {
    private:
        std::list<File> path_;
    public:
        FilePath(const FilePath& filePath);
        explicit FilePath(const std::list<File>&);
        explicit FilePath(char const* const*);
        explicit FilePath();

        FilePath& append(const File& file);
        FilePath& append(const FilePath& filePath);

        const std::string normalizedPath() const;
        const std::string unixfiedPath() const;
        bool check() const;
        char** normalizedArray() const;
};

char** splitFilePath(const char* s);

std::auto_ptr<FilePath> parseStringAsFilePath(const std::string& filePath);

std::auto_ptr<FilePath> parseStringAsUnixFilePath(const std::string& filePath);

#endif
