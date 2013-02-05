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
#ifndef __JVMBridge_h__
#define __JVMBridge_h__

#include "ValueList.h"
#include <jni.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C"
    {
#endif
    JNIEXPORT JNIEnv*        JVMBRIDGEFUNC(AttachCurrentThread)                (JavaVM*);
    JNIEXPORT void            JVMBRIDGEFUNC(DetachCurrentThread)                (JavaVM*);
    JNIEXPORT JavaVM*        JVMBRIDGEFUNC(GetJavaVM)                        (JNIEnv*);

    JNIEXPORT jobject        JVMBRIDGEFUNC(FindClass)                        (JNIEnv*,const char*);
    JNIEXPORT uintptr_t        JVMBRIDGEFUNC(ConvertCallback)                    (void*);

    JNIEXPORT jobject        JVMBRIDGEFUNC(AllocObject)                        (JNIEnv*,jobject cl);

    JNIEXPORT jobject        JVMBRIDGEFUNC(NewBooleanArray)                    (JNIEnv*,jsize);
    JNIEXPORT jobject        JVMBRIDGEFUNC(NewByteArray)                        (JNIEnv*,jsize);
    JNIEXPORT jobject        JVMBRIDGEFUNC(NewCharArray)                        (JNIEnv*,jsize);
    JNIEXPORT jobject        JVMBRIDGEFUNC(NewShortArray)                    (JNIEnv*,jsize);
    JNIEXPORT jobject        JVMBRIDGEFUNC(NewIntArray)                        (JNIEnv*,jsize);
    JNIEXPORT jobject        JVMBRIDGEFUNC(NewLongArray)                        (JNIEnv*,jsize);
    JNIEXPORT jobject        JVMBRIDGEFUNC(NewFloatArray)                    (JNIEnv*,jsize);
    JNIEXPORT jobject        JVMBRIDGEFUNC(NewDoubleArray)                    (JNIEnv*,jsize);
    JNIEXPORT jobject        JVMBRIDGEFUNC(NewObjectArray)                    (JNIEnv*,jsize,jobject cl,jobject init);
    JNIEXPORT jobject        JVMBRIDGEFUNC(NewString)                        (JNIEnv*,const jchar*,jsize);
    JNIEXPORT jobject        JVMBRIDGEFUNC(NewStringUTF)                        (JNIEnv*,const char*);

    JNIEXPORT void            JVMBRIDGEFUNC(FreePointer)                        (char*);

    JNIEXPORT jsize            JVMBRIDGEFUNC(GetArrayLength)                    (JNIEnv*,jobject);

    JNIEXPORT jboolean*        JVMBRIDGEFUNC(GetBooleanArrayRegion)            (JNIEnv*,jobject,jsize,jsize);
    JNIEXPORT jbyte*        JVMBRIDGEFUNC(GetByteArrayRegion)                (JNIEnv*,jobject,jsize,jsize);
    JNIEXPORT jchar*        JVMBRIDGEFUNC(GetCharArrayRegion)                (JNIEnv*,jobject,jsize,jsize);
    JNIEXPORT jshort*        JVMBRIDGEFUNC(GetShortArrayRegion)                (JNIEnv*,jobject,jsize,jsize);
    JNIEXPORT jint*            JVMBRIDGEFUNC(GetIntArrayRegion)                (JNIEnv*,jobject,jsize,jsize);
    JNIEXPORT jlong*        JVMBRIDGEFUNC(GetLongArrayRegion)                (JNIEnv*,jobject,jsize,jsize);
    JNIEXPORT jfloat*        JVMBRIDGEFUNC(GetFloatArrayRegion)                (JNIEnv*,jobject,jsize,jsize);
    JNIEXPORT jdouble*        JVMBRIDGEFUNC(GetDoubleArrayRegion)                (JNIEnv*,jobject,jsize,jsize);
    JNIEXPORT jobject        JVMBRIDGEFUNC(GetObjectArrayElement)            (JNIEnv*,jobject,jsize);

    JNIEXPORT void            JVMBRIDGEFUNC(SetBooleanArrayRegion)            (JNIEnv*,jobject,jsize,jsize,const jboolean*);
    JNIEXPORT void            JVMBRIDGEFUNC(SetByteArrayRegion)                (JNIEnv*,jobject,jsize,jsize,const jbyte*);
    JNIEXPORT void            JVMBRIDGEFUNC(SetCharArrayRegion)                (JNIEnv*,jobject,jsize,jsize,const jchar*);
    JNIEXPORT void            JVMBRIDGEFUNC(SetShortArrayRegion)                (JNIEnv*,jobject,jsize,jsize,const jshort*);
    JNIEXPORT void            JVMBRIDGEFUNC(SetIntArrayRegion)                (JNIEnv*,jobject,jsize,jsize,const jint*);
    JNIEXPORT void            JVMBRIDGEFUNC(SetLongArrayRegion)                (JNIEnv*,jobject,jsize,jsize,const jlong*);
    JNIEXPORT void            JVMBRIDGEFUNC(SetFloatArrayRegion)                (JNIEnv*,jobject,jsize,jsize,const jfloat*);
    JNIEXPORT void            JVMBRIDGEFUNC(SetDoubleArrayRegion)                (JNIEnv*,jobject,jsize,jsize,const jdouble*);
    JNIEXPORT void            JVMBRIDGEFUNC(SetObjectArrayElement)            (JNIEnv*,jobject,jsize,jobject);

    JNIEXPORT jfieldID        JVMBRIDGEFUNC(GetStaticFieldID)                    (JNIEnv*,jobject cl,const char* name,const char* sig);
    JNIEXPORT jfieldID        JVMBRIDGEFUNC(GetFieldID)                        (JNIEnv*,jobject cl,const char* name,const char* sig);
    
    JNIEXPORT jboolean        JVMBRIDGEFUNC(GetStaticBooleanField)            (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jbyte            JVMBRIDGEFUNC(GetStaticByteField)                (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jchar            JVMBRIDGEFUNC(GetStaticCharField)                (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jshort        JVMBRIDGEFUNC(GetStaticShortField)                (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jint            JVMBRIDGEFUNC(GetStaticIntField)                (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jlong            JVMBRIDGEFUNC(GetStaticLongField)                (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jfloat        JVMBRIDGEFUNC(GetStaticFloatField)                (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jdouble        JVMBRIDGEFUNC(GetStaticDoubleField)                (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jobject        JVMBRIDGEFUNC(GetStaticObjectField)                (JNIEnv*,jobject,jfieldID);
    JNIEXPORT void            JVMBRIDGEFUNC(SetStaticBooleanField)            (JNIEnv*,jobject,jfieldID,jboolean);
    JNIEXPORT void            JVMBRIDGEFUNC(SetStaticByteField)                (JNIEnv*,jobject,jfieldID,jbyte);
    JNIEXPORT void            JVMBRIDGEFUNC(SetStaticCharField)                (JNIEnv*,jobject,jfieldID,jchar);
    JNIEXPORT void            JVMBRIDGEFUNC(SetStaticShortField)                (JNIEnv*,jobject,jfieldID,jshort);
    JNIEXPORT void            JVMBRIDGEFUNC(SetStaticIntField)                (JNIEnv*,jobject,jfieldID,jint);
    JNIEXPORT void            JVMBRIDGEFUNC(SetStaticLongField)                (JNIEnv*,jobject,jfieldID,jlong);
    JNIEXPORT void            JVMBRIDGEFUNC(SetStaticFloatField)                (JNIEnv*,jobject,jfieldID,jfloat);
    JNIEXPORT void            JVMBRIDGEFUNC(SetStaticDoubleField)                (JNIEnv*,jobject,jfieldID,jdouble);
    JNIEXPORT void            JVMBRIDGEFUNC(SetStaticObjectField)                (JNIEnv*,jobject,jfieldID,jobject);
    
    JNIEXPORT jboolean        JVMBRIDGEFUNC(GetBooleanField)                    (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jbyte            JVMBRIDGEFUNC(GetByteField)                        (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jchar            JVMBRIDGEFUNC(GetCharField)                        (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jshort        JVMBRIDGEFUNC(GetShortField)                    (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jint            JVMBRIDGEFUNC(GetIntField)                        (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jlong            JVMBRIDGEFUNC(GetLongField)                        (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jfloat        JVMBRIDGEFUNC(GetFloatField)                    (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jdouble        JVMBRIDGEFUNC(GetDoubleField)                    (JNIEnv*,jobject,jfieldID);
    JNIEXPORT jobject        JVMBRIDGEFUNC(GetObjectField)                    (JNIEnv*,jobject,jfieldID);
    JNIEXPORT void            JVMBRIDGEFUNC(SetBooleanField)                    (JNIEnv*,jobject,jfieldID,jboolean);
    JNIEXPORT void            JVMBRIDGEFUNC(SetByteField)                        (JNIEnv*,jobject,jfieldID,jbyte);
    JNIEXPORT void            JVMBRIDGEFUNC(SetCharField)                        (JNIEnv*,jobject,jfieldID,jchar);
    JNIEXPORT void            JVMBRIDGEFUNC(SetShortField)                    (JNIEnv*,jobject,jfieldID,jshort);
    JNIEXPORT void            JVMBRIDGEFUNC(SetIntField)                        (JNIEnv*,jobject,jfieldID,jint);
    JNIEXPORT void            JVMBRIDGEFUNC(SetLongField)                        (JNIEnv*,jobject,jfieldID,jlong);
    JNIEXPORT void            JVMBRIDGEFUNC(SetFloatField)                    (JNIEnv*,jobject,jfieldID,jfloat);
    JNIEXPORT void            JVMBRIDGEFUNC(SetDoubleField)                    (JNIEnv*,jobject,jfieldID,jdouble);
    JNIEXPORT void            JVMBRIDGEFUNC(SetObjectField)                    (JNIEnv*,jobject,jfieldID,jobject);
    
    JNIEXPORT jmethodID        JVMBRIDGEFUNC(GetStaticMethodID)                (JNIEnv*,jobject cl,const char* name,const char* sig);
    JNIEXPORT jmethodID        JVMBRIDGEFUNC(GetMethodID)                        (JNIEnv*,jobject cl,const char* name,const char* sig);
    
    // NOTE: all of these functions destroy the value list for you.
    JNIEXPORT void            JVMBRIDGEFUNC(CallStaticVoidMethodVL)            (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jboolean        JVMBRIDGEFUNC(CallStaticBooleanMethodVL)        (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jbyte            JVMBRIDGEFUNC(CallStaticByteMethodVL)            (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jchar            JVMBRIDGEFUNC(CallStaticCharMethodVL)            (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jshort        JVMBRIDGEFUNC(CallStaticShortMethodVL)            (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jint            JVMBRIDGEFUNC(CallStaticIntMethodVL)            (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jlong            JVMBRIDGEFUNC(CallStaticLongMethodVL)            (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jfloat        JVMBRIDGEFUNC(CallStaticFloatMethodVL)            (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jdouble        JVMBRIDGEFUNC(CallStaticDoubleMethodVL)            (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jobject        JVMBRIDGEFUNC(CallStaticObjectMethodVL)            (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT void            JVMBRIDGEFUNC(CallVoidMethodVL)                    (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jboolean        JVMBRIDGEFUNC(CallBooleanMethodVL)                (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jbyte            JVMBRIDGEFUNC(CallByteMethodVL)                    (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jchar            JVMBRIDGEFUNC(CallCharMethodVL)                    (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jshort        JVMBRIDGEFUNC(CallShortMethodVL)                (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jint            JVMBRIDGEFUNC(CallIntMethodVL)                    (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jlong            JVMBRIDGEFUNC(CallLongMethodVL)                    (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jfloat        JVMBRIDGEFUNC(CallFloatMethodVL)                (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jdouble        JVMBRIDGEFUNC(CallDoubleMethodVL)                (JNIEnv*,jobject,jmethodID,jvaluelist);
    JNIEXPORT jobject        JVMBRIDGEFUNC(CallObjectMethodVL)                (JNIEnv*,jobject,jmethodID,jvaluelist);
     JNIEXPORT void            JVMBRIDGEFUNC(CallNonvirtualVoidMethodVL)        (JNIEnv*,jobject,jobject,jmethodID,jvaluelist);
    JNIEXPORT jboolean        JVMBRIDGEFUNC(CallNonvirtualBooleanMethodVL)    (JNIEnv*,jobject,jobject,jmethodID,jvaluelist);
    JNIEXPORT jbyte            JVMBRIDGEFUNC(CallNonvirtualByteMethodVL)        (JNIEnv*,jobject,jobject,jmethodID,jvaluelist);
    JNIEXPORT jchar            JVMBRIDGEFUNC(CallNonvirtualCharMethodVL)        (JNIEnv*,jobject,jobject,jmethodID,jvaluelist);
    JNIEXPORT jshort        JVMBRIDGEFUNC(CallNonvirtualShortMethodVL)        (JNIEnv*,jobject,jobject,jmethodID,jvaluelist);
    JNIEXPORT jint            JVMBRIDGEFUNC(CallNonvirtualIntMethodVL)        (JNIEnv*,jobject,jobject,jmethodID,jvaluelist);
    JNIEXPORT jlong            JVMBRIDGEFUNC(CallNonvirtualLongMethodVL)        (JNIEnv*,jobject,jobject,jmethodID,jvaluelist);
    JNIEXPORT jfloat        JVMBRIDGEFUNC(CallNonvirtualFloatMethodVL)        (JNIEnv*,jobject,jobject,jmethodID,jvaluelist);
    JNIEXPORT jdouble        JVMBRIDGEFUNC(CallNonvirtualDoubleMethodVL)        (JNIEnv*,jobject,jobject,jmethodID,jvaluelist);
    JNIEXPORT jobject        JVMBRIDGEFUNC(CallNonvirtualObjectMethodVL)        (JNIEnv*,jobject,jobject,jmethodID,jvaluelist);

    JNIEXPORT jboolean        JVMBRIDGEFUNC(IsSameObject)                        (JNIEnv*,jobject,jobject);
    JNIEXPORT jboolean        JVMBRIDGEFUNC(IsInstanceOf)                        (JNIEnv*,jobject,jobject cl);

    JNIEXPORT jint            JVMBRIDGEFUNC(ThrowException)                    (JNIEnv*,jobject);
    JNIEXPORT jobject        JVMBRIDGEFUNC(GetClearException)                (JNIEnv*);
    JNIEXPORT jboolean        JVMBRIDGEFUNC(ExceptionPending)                    (JNIEnv*);

    JNIEXPORT jint            JVMBRIDGEFUNC(MonitorEnter)                        (JNIEnv*,jobject);
    JNIEXPORT jint            JVMBRIDGEFUNC(MonitorExit)                        (JNIEnv*,jobject);

    JNIEXPORT jobject        JVMBRIDGEFUNC(NewGlobalRef)                        (JNIEnv*,jobject);
    JNIEXPORT jobject        JVMBRIDGEFUNC(ConvertToGlobalRef)                (JNIEnv*,jobject);
    JNIEXPORT void            JVMBRIDGEFUNC(DeleteGlobalRef)                    (JNIEnv*,jobject);
    JNIEXPORT void            JVMBRIDGEFUNC(DeleteGlobalRefGC)                (JavaVM*,jobject);

    JNIEXPORT char*            JVMBRIDGEFUNC(GetStringUTFChars)                (JNIEnv*,jobject);
    JNIEXPORT jint            JVMBRIDGEFUNC(GetStringUTFLength)                (JNIEnv*,jobject);

    JNIEXPORT jobject        JVMBRIDGEFUNC(DefineClass)                        (JNIEnv*,const char*,jobject,const jbyte*,jsize);
#ifdef __cplusplus
     }
#endif

#endif // ndef __JVMBridge_h__
