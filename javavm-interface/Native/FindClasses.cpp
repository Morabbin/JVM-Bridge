/*
JVM-Bridge -- bridge from FP languages and others to the Java VM
Copyright (C) 2007 Ashley Yakeley <ashley@semantic.org>

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
#include "Debug.hpp"
#include "JVMBridge.h"
#include "JVMInvocation.h"
#include "ExecuteFunctionControl.h"
//#include "ValueList.h"
#include <jni.h>

bool FindClass(JNIEnv* env,char const* name)
{
    jobject cls = JVMBRIDGEFUNC(FindClass)(env,name);
    bool success = cls != 0;
    fprintf(stderr,"FindClass for %s %s.\n", name,success? "succeeded":"failed");
    return success;
}

int main(int argc,char** argv)
    {
    DebugBlockTracer tracer("main");
    
    if (argc < 2)
        {
        ErrorMessage("Need classpath argument\n");
        return 1;
        }
    DebugMessage("Creating JVM\n");
    char** classPath = JVMBRIDGEFUNC(PlatformSplitClassPaths)(argv[1]);
    
    JavaVM* vm = JVMBRIDGEFUNC(CreateJavaVM)(classPath);
    JVMBRIDGEFUNC(DeletePathArray)(classPath);
    DebugShowPointer(vm,"VM");
    DebugMessage("Attaching Thread\n");
    JNIEnv* env = JVMBRIDGEFUNC(AttachCurrentThread)(vm);
    bool ok = true;
    ok &= FindClass(env,"java/lang/Object");
    ok &= FindClass(env,"java/lang/Class");
    ok &= FindClass(env,"java/lang/String");
    ok &= FindClass(env,"java/lang/Throwable");
    ok &= FindClass(env,"java/awt/Component");
    ok &= FindClass(env,"java/awt/Frame");
    JVMBRIDGEFUNC(DetachCurrentThread)(vm);
    return ok? 0: 1;
    }
