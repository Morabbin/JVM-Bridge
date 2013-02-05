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
#ifndef __JVMInvocation_h__
#define __JVMInvocation_h__

#include <jni.h>

#ifndef JNIEXPORT
#define JNIEXPORT extern
#endif 

#ifdef __cplusplus
extern "C"
    {
#endif
    // userClassPath is a null-terminated list of paths
    JNIEXPORT JavaVM* JVMBridge_CreateJavaVM(const char* const* userClassPath);

    // split according to the platform. Generally this is by ":", but it's ";" on Windows
    JNIEXPORT char** JVMBridge_PlatformSplitClassPaths(const char*);
    // use this when you're finished
    JNIEXPORT void JVMBridge_DeletePathArray(char**);
#ifdef __cplusplus
     } 
#endif

#endif // ndef __JVMInvocation_h__
