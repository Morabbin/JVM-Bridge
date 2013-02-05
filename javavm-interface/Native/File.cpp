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

#include <jni.h>
#include "ValueList.h"
#include "Debug.hpp"
#include "File.hpp"
#include <stdexcept>
#include <cstring>
#include <unistd.h>
#include <sys/types.h>

#define BUFFER_SIZE 1024

//////////////
// File
//////////////

// File helper functions

namespace {

void trim(std::string& file) {
    static const char* WS = "\t\n\r\v ";
    std::string::size_type last = file.find_last_not_of(WS);
    if (last == std::string::npos) {
        file = file.substr(0,0);
        return;
    }
    if (file.at(last) == '/') {
        while (file.at(last) == '/') --last;
    }
#if WIN32
    else {
        while (file.at(last) == '\\') --last;
    }
#endif
    file = file.substr(file.find_first_not_of(WS), last+1);
}

std::string& replaceAll(std::string& file, const char org, const char replace) {
    std::string::iterator pos;
    char c;
    for (pos = file.begin(); pos != file.end(); ++pos) {
        c = *pos;
        if (c == org) {
            file.replace(pos, pos+1, 1, replace);
        }
    }
    return file;
}

inline std::string replaceAll(const char* file, const char org, const char replace) {
    std::string str(file);
    return replaceAll(str, org, replace);
}

const std::string normalizeFile(const char* f)
{
    if (!f) {
        ErrorMessage("normalizeFile(NULL): Illegal argument\n");
    }
    std::string file(f);
    try {
        trim(file);
        // printf("after trim: '%s'\n", file.c_str());
#if WIN32
        const int length = file.length();
        if (length < 2) {
            char buffer[BUFFER_SIZE];
            const char* cwd = getcwd(buffer, BUFFER_SIZE);
            InfoDebugMessage("Win32 relative file location format: %s relative to %s\n", file.c_str(), cwd);
            return replaceAll(cwd, '/', '\\') + FILE_SEPERATOR + file;
        }
        char c = file.at(0);
        if (c != '/') {
            if (file.at(1) == ':') {
                replaceAll(file, '/', '\\');
            }
            else {
                char buffer[BUFFER_SIZE];
                const char* cwd = getcwd(buffer, BUFFER_SIZE);
                InfoDebugMessage("Win32 relative file location format: %s relative to %s\n", file.c_str(), cwd);
                file = replaceAll(cwd, '/', '\\') + FILE_SEPERATOR + file;
            }
        }
        else {
#if CYGWIN
            if (file.find("/cygdrive/") == 0) {
                file.replace(0, 9, "");
            }
#endif // CYGWIN
            // remove starting '/'
            file.replace(0, 1, "");
            if (file.at(1) != '/') {
                ErrorMessage("Wrong unixfied win32 file location: /%s\n", file.c_str());
            }
            file.replace(1, 1, ":/");
        }
#endif
    }
    catch (const ::std::out_of_range& e) {
        ErrorMessage("::std::out_of_range: %s in normalizeFile(%s)\n", e.what(), f);
    }
    return file;
}

const std::string unixfyFile(const std::string& normalizedFile) {
    std::string file(normalizedFile);
#if WIN32
    // remove ':'
    file.replace(1, 1, "");
    // insert starting '/'
#if CYGWIN
    file = "/cygdrive/" + file;
#else
    file = "/" + file;
#endif // CYGWIN
    replaceAll(file, '\\', '/');
#endif // WIN32
    return file;
}

} // end anonymous namespace

// File implementation

File::File(const File& file)
    : normalizedFile_(file.normalizedFile_),
      unixfiedFile_(file.unixfiedFile_),
      attr_(file.attr_)
{
    // printf("Copying File(%s) dir %i file %i\n", file.normalizedFile_.c_str(), isDir(), isFile());
}

File::File(const char* filename)
    : normalizedFile_(normalizeFile(filename)),
      unixfiedFile_(unixfyFile(normalizedFile_))
{
    __attribute__((unused)) int error = stat(normalizedFile_.c_str(), &attr_);
    // printf("Creating File(%s) returns %i dir %i file %i\n", filename, error, isDir(), isFile());
}

const std::string File::normalizedName() const
{
    return normalizedFile_;
}

const std::string File::unixfiedName() const
{
    return unixfiedFile_;
}

bool File::isFile() const
{
    return S_ISREG(attr_.st_mode);
}

bool File::isDir() const
{
    return S_ISDIR(attr_.st_mode);
}

/////////////
// FilePath
/////////////

FilePath::FilePath(const FilePath& filePath)
    : path_(filePath.path_)
{
}

FilePath::FilePath(const std::list<File>& filePath)
    : path_(filePath)
{
}

FilePath::FilePath(char const* const* fileArray)
{
    for (char const* const* i=fileArray;*i;i++) {
        DebugMessage("file: %s\n",*i);
        append(File(*i));
    }
}

FilePath::FilePath()
{
}

FilePath& FilePath::append(const File& file)
{
    path_.push_back(file);
    return *this;
}

FilePath& FilePath::append(const FilePath& filePath) {
    std::list<File>::const_iterator it;
    for (it = filePath.path_.begin(); it != filePath.path_.end(); ++it) {
        append(*it);
    }
    return *this;
}

const std::string FilePath::normalizedPath() const
{
    std::string result;
    std::list<File>::const_iterator it;
    bool gotOne = false;
    for (it = path_.begin(); it != path_.end(); ++it) {
        result.append(it->normalizedName());
        result.append(PATH_SEPERATOR);
        gotOne = true;
    }
    if (gotOne)
        result.replace(result.size() - 1, 1, "");
    return result;
}

const std::string FilePath::unixfiedPath() const
{
    std::string result;
    std::list<File>::const_iterator it;
    bool gotOne = false;
    for (it = path_.begin(); it != path_.end(); ++it) {
        result.append(it->unixfiedName());
        result.append(UNIX_PATH_SEPERATOR);
        gotOne = true;
    }
    if (gotOne)
        result.replace(result.size() - 1, 1, "");
    return result;
}

bool FilePath::check() const
{
    std::list<File>::const_iterator it;
    for (it = path_.begin(); it != path_.end(); ++it) {
        InfoDebugMessage("check %s dir %i file %i\n", it->normalizedName().c_str(), it->isDir(), it->isFile());
        if (!(it->isFile() || it->isDir())) {
            return false;
        }    
    }
    return true;
}

char** FilePath::normalizedArray() const
{
    int n = path_.size();
    char** array = new char*[n+1];
    int i = 0;
    for (std::list<File>::const_iterator it = path_.begin(); it != path_.end(); ++it,++i) {
        const char* s = it->normalizedName().c_str();
        int b = strlen(s)+1;
        array[i] = new char[b];
        memcpy(array[i],s,b);
    }    
    array[n] = 0;    // null-termination;
    return array;
}


// FilePath Helper

namespace {

std::auto_ptr<FilePath> _parseStringAsFilePath(const std::string& filePath, const char* separator)
{
    std::auto_ptr<FilePath> result_p(new FilePath());
    std::string::size_type begin = 0;
    std::string::size_type end = filePath.find(separator);
    while(end != std::string::npos) {
        const File file(filePath.substr(begin, end-begin).c_str());
        result_p->append(file);
        begin = ++end;
        end = filePath.find(separator, end);
    }
    const File file(filePath.substr(begin).c_str());
    result_p->append(file);
    return result_p;
}

} // end anonymous namespace

std::auto_ptr<FilePath> parseStringAsFilePath(const std::string& filePath)
{
    return _parseStringAsFilePath(filePath, PATH_SEPERATOR);
}

std::auto_ptr<FilePath> parseStringAsUnixFilePath(const std::string& filePath)
{
    return _parseStringAsFilePath(filePath, UNIX_PATH_SEPERATOR);
}

char** splitFilePath(const char* s)
{
    std::auto_ptr<FilePath> fpp = parseStringAsFilePath(s);
    return fpp->normalizedArray();
}





