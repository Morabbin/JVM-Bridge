/*
JVM-Bridge -- bridge from FP languages and others to the Java VM
Copyright (C) 2001 Ashley Yakeley <ashley@semantic.org>

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
#include "JVMInvocation.h"
#include "Debug.hpp"
#include "File.hpp"

#if VM_IBM
#define SET_LIB_PATH_ENV 1
#else
#define SET_LIB_PATH_ENV 0
#endif

#include <string>
#include <vector>
#if SET_LIB_PATH_ENV
#include <stdlib.h>
#endif

using namespace ::std;

#define INVOKE_1_2 JNI_VERSION_1_2

inline void SetOption(JavaVMOption& option,const char* text)
    {
    option.optionString    = const_cast<char*>(text);
    option.extraInfo = 0;
    InfoDebugMessage ("option: %s\n",option.optionString);
    }

char** JVMBridge_PlatformSplitClassPaths(const char* userClassPath)
    {
    return splitFilePath(userClassPath);
    }

void JVMBridge_DeletePathArray(char** userClassPath)
    {
    for (char** p=userClassPath;*p;p++)
        delete[](*p);
    delete[](userClassPath);
    }

JavaVM* JVMBridge_CreateJavaVM(const char* const* userClassPath)
    {
    DebugBlockTracer tracer("CreateJavaVM");

#if SET_LIB_PATH_ENV
        {
        const char* kEnvName="LD_LIBRARY_PATH";
        const char* extraPath=JVM_LIBDIRS;
        const char* oldPath=getenv(kEnvName);
        string newPath("");
        if (oldPath)
            {
            newPath = oldPath;
            newPath += UNIX_PATH_SEPERATOR;
            newPath += extraPath;
            }
        else newPath = extraPath;
        setenv (kEnvName,newPath.c_str(),1);
        }
#endif

    JavaVM* vm;

#if INVOKE_1_2
    InfoDebugMessage("1.2-style JVM invocation\n");
#if DEBUGVM
    const int nOptions=4;
#else
    const int nOptions=2;
#endif
    JavaVMOption options[nOptions];
    string classOptionString("-Djava.class.path=");
    if (!userClassPath)
        ErrorMessage("null class path array");
    FilePath filePath(userClassPath);
    classOptionString += filePath.normalizedPath();
    SetOption(options[0],classOptionString.c_str());
    string libraryOptionString("-Djava.library.path=");
    filePath = *parseStringAsUnixFilePath(JVM_LIBDIRS);
    libraryOptionString += filePath.normalizedPath();
    SetOption(options[1], libraryOptionString.c_str());
#if DEBUGVM
    SetOption(options[2],"-Djava.compiler=NONE"); // disable JIT
    SetOption(options[3],"-verbose:class,jni"); // print JNI-related messages
#endif
    JavaVMInitArgs args;
#if JNI_VERSION_1_4
    args.version            = JNI_VERSION_1_4;
#else
    args.version            = JNI_VERSION_1_2;
#endif
    args.nOptions            = nOptions;
    args.options            = options;
    args.ignoreUnrecognized    = false;
#else
    InfoDebugMessage("1.1-style JVM invocation\n");
    JDK1_1InitArgs args;
    args.version    = JNI_VERSION_1_1;
    ::JNI_GetDefaultJavaVMInitArgs(&args);
    string classpath;
    if (args.classpath)
        {
        classpath += args.classpath;
        classpath += PATH_SEPERATOR;
        }
    auto_ptr<FilePath> filePath_p(parseStringAsUnixFilePath(JVM_CLASSES));
    filePath_p->append(userClassPath);
    classpath += filePath_p->normalizedPath();
    InfoDebugMessage("args.classpath: %s\n", classpath.c_str());
    if (!filePath_p->check())
     ErrorMessage("At least one path doesn't exist in %s\n", filePath_p->normalizedPath().c_str());

    args.classpath = const_cast<char*>(classpath.c_str());

    const char* properties[2];
    string libraryPropertyString("java.library.path=");
    filePath_p = parseStringAsUnixFilePath(JVM_LIBDIRS);
    libraryPropertyString += filePath_p->normalizedPath();
    InfoDebugMessage("property: %s\n", libraryPropertyString.c_str());
    if (!filePath_p->check())
     ErrorMessage("At least one path doesn't exist in %s\n", filePath_p->normalizedPath().c_str());
    properties[0] = libraryPropertyString.c_str();
    properties[1] = 0;

    args.properties = const_cast<char**>(properties);
#endif
    JNIEnv* env;
    jint error;
#if JNI_VERSION_1_2
    error = ::JNI_CreateJavaVM(&vm,reinterpret_cast<void**>(&env),&args);
#else
    error = ::JNI_CreateJavaVM(&vm,&env,&args);
#endif
    if (error || !vm || !env)
     ErrorMessage("JNI_CreateJavaVM failed with %i: vm %x env %x\n", error, vm, env);
    DebugShowPointer(vm,"vm");
    return vm;
    }
