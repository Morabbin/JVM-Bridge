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
#include "Debug.hpp"
#include "JVMBridge.h"
#include "JVMInvocation.h"
#include "ExecuteFunctionControl.h"
//#include "ValueList.h"
#include <jni.h>

extern "C" void CallbackFunction (JNIEnv,jvaluelist vl)
    {
    DebugBlockTracer("CallbackFunction");
    jshort s = JVMBRIDGEFUNC(GetNthValueAsShort)(vl,0);
    DebugShowShort(s,"s");
    };

void NullFree (void* f)
    {
    DebugBlockTracer tracer("! Null FreeFunction");
    // do nothing
    }

int main(int argc,char** argv)
    {
    DebugBlockTracer tracer("main");
    
    if (argc < 3)
        {
        ErrorMessage("Need two args\n");
        return 1;
        }
    DebugMessage("Creating JVM\n");
    char** classPath = JVMBRIDGEFUNC(PlatformSplitClassPaths)(argv[1]);
    
    JavaVM* vm = JVMBRIDGEFUNC(CreateJavaVM)(classPath);
    JVMBRIDGEFUNC(DeletePathArray)(classPath);
    DebugShowPointer(vm,"VM");
    DebugMessage("Attaching Thread\n");
    JNIEnv* env = JVMBRIDGEFUNC(AttachCurrentThread)(vm);
    JVMBRIDGEFUNC(FindClass)(env,"java/lang/Object");
    jobject stringClass = JVMBRIDGEFUNC(FindClass)(env,"java/lang/String");
    if (stringClass == 0)
        {
        ErrorMessage("FindClass for java/lang/String failed.\n", argv[2]);
        return 1;
        }
    if (JVMBRIDGEFUNC(FindClass)(env,"org/semantic/jvmbridge/ExecuteFunction") == 0)
        {
        ErrorMessage("FindClass for org/semantic/jvmbridge/ExecuteFunction failed.\n", argv[2]);
        return 1;
        }
    jobject appClass = JVMBRIDGEFUNC(FindClass)(env,argv[2]);
    if (appClass == 0)
        {
        ErrorMessage("FindClass for %s failed.\n", argv[2]);
        return 1;
        }
    
        {
        jmethodID mainMethod = JVMBRIDGEFUNC(GetStaticMethodID)    (env,appClass,"main","([Ljava/lang/String;)V");

        jobject stringArray = JVMBRIDGEFUNC(NewObjectArray)(env,0,stringClass,0);
        jvaluelist vlist = JVMBRIDGEFUNC(CreateValueList)();
        JVMBRIDGEFUNC(AddObjectToValueList)(vlist,stringArray);
        JVMBRIDGEFUNC(CallStaticVoidMethodVL)(env,appClass,mainMethod,vlist);
        }

    JVMBRIDGEFUNC(StartExecuteFunction)(env,&NullFree);

        {
        jmethodID fooMethod = JVMBRIDGEFUNC(GetStaticMethodID)    (env,appClass,"foo","(" SigOpaqueAddress "S)V");
        jvaluelist vlist = JVMBRIDGEFUNC(CreateValueList)();
        #if ARCH_64BIT
        JVMBRIDGEFUNC(AddLongToValueList)
        #else
        JVMBRIDGEFUNC(AddIntToValueList)
        #endif
            (vlist,reinterpret_cast<COpaqueAddress>(&CallbackFunction));
        JVMBRIDGEFUNC(AddShortToValueList)(vlist,37);
        JVMBRIDGEFUNC(CallStaticVoidMethodVL)(env,appClass,fooMethod,vlist);
        }
    

    JVMBRIDGEFUNC(DetachCurrentThread)(vm);
    
    return 0;
    }
