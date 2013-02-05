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
#ifndef __ValueList_h__
#define __ValueList_h__

#include <jni.h>

typedef void* jvaluelist;

#define JVMBRIDGEFUNC(x) JVMBridge_##x

#ifndef JNIEXPORT
#define JNIEXPORT extern
#endif 

#ifdef __cplusplus
extern "C"
    {
#endif
    JNIEXPORT jvaluelist    JVMBRIDGEFUNC(CreateValueList)                    ();
    JNIEXPORT void            JVMBRIDGEFUNC(DestroyValueList)                    (jvaluelist);
    JNIEXPORT void            JVMBRIDGEFUNC(AddBooleanToValueList)            (jvaluelist,jboolean);
    JNIEXPORT void            JVMBRIDGEFUNC(AddByteToValueList)                (jvaluelist,jbyte);
    JNIEXPORT void            JVMBRIDGEFUNC(AddCharToValueList)                (jvaluelist,jchar);
    JNIEXPORT void            JVMBRIDGEFUNC(AddShortToValueList)                (jvaluelist,jshort);
    JNIEXPORT void            JVMBRIDGEFUNC(AddIntToValueList)                (jvaluelist,jint);
    JNIEXPORT void            JVMBRIDGEFUNC(AddLongToValueList)                (jvaluelist,jlong);
    JNIEXPORT void            JVMBRIDGEFUNC(AddFloatToValueList)                (jvaluelist,jfloat);
    JNIEXPORT void            JVMBRIDGEFUNC(AddDoubleToValueList)                (jvaluelist,jdouble);
    JNIEXPORT void            JVMBRIDGEFUNC(AddObjectToValueList)                (jvaluelist,jobject);

    JNIEXPORT jboolean        JVMBRIDGEFUNC(GetNthValueAsBoolean)                (jvaluelist,jshort);
    JNIEXPORT jbyte            JVMBRIDGEFUNC(GetNthValueAsByte)                (jvaluelist,jshort);
    JNIEXPORT jchar            JVMBRIDGEFUNC(GetNthValueAsChar)                (jvaluelist,jshort);
    JNIEXPORT jshort        JVMBRIDGEFUNC(GetNthValueAsShort)                (jvaluelist,jshort);
    JNIEXPORT jint            JVMBRIDGEFUNC(GetNthValueAsInt)                    (jvaluelist,jshort);
    JNIEXPORT jlong            JVMBRIDGEFUNC(GetNthValueAsLong)                (jvaluelist,jshort);
    JNIEXPORT jfloat        JVMBRIDGEFUNC(GetNthValueAsFloat)                (jvaluelist,jshort);
    JNIEXPORT jdouble        JVMBRIDGEFUNC(GetNthValueAsDouble)                (jvaluelist,jshort);
    JNIEXPORT jobject        JVMBRIDGEFUNC(GetNthValueAsObject)                (jvaluelist,jshort);
#ifdef __cplusplus
     }
#endif

#endif // ndef __ValueList_h__
