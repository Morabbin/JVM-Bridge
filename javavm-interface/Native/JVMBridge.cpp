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
#include "ValueListPrivate.hpp"

#include <cstring>
#include <string>
#include <vector>
using namespace ::std;

inline bool IsV1_2(JNIEnv* env)
    {
    return env->GetVersion() >= 0x00010002;
    }

// delete[] this after you're finished with it.
template <class T>
inline T* VectorToArray(const vector<T>& v)
    {
    unsigned long n = v.size();
    T* array = new T[n];
    for (unsigned long i=0;i<n;i++)
        array[i] = v[i];
    return array;
    }

JNIEnv* JVMBRIDGEFUNC(AttachCurrentThread)(JavaVM* vm)
    {
#if DEBUGGC
    DebugBlockTracer tracer("AttachCurrentThread");
    DebugShowPointer(vm,"vm");
#endif
    JNIEnv* env;
#if JNI_VERSION_1_2
    vm->AttachCurrentThread(reinterpret_cast<void**>(&env),0);
#else
    vm->AttachCurrentThread(&env,0);
#endif
#if DEBUGGC
    DebugShowPointer(env,"env");
#endif
    return env;
    }

void JVMBRIDGEFUNC(DetachCurrentThread)(JavaVM* vm)
    {
#if DEBUGGC
    DebugBlockTracer tracer("DetachCurrentThread");
#endif
    vm->DetachCurrentThread();
    }

JavaVM* JVMBRIDGEFUNC(GetJavaVM)(JNIEnv* env)
    {
    DebugBlockTracer tracer("GetJavaVM");
    JavaVM* vm;
    env->GetJavaVM(&vm);
    DebugShowPointer(vm,"vm");
    return vm;
    }

jobject JVMBRIDGEFUNC(NewStringUTF)(JNIEnv* env,const char* utfStr)
    {
    DebugBlockTracer tracer("NewStringUTF");
    return env->NewStringUTF(utfStr);
    }

jobject JVMBRIDGEFUNC(NewString)(JNIEnv* env,const jchar* arr,jsize len)
    {
    DebugBlockTracer tracer("NewString");
    return env->NewString(arr,len);
    }

jobject JVMBRIDGEFUNC(FindClass)(JNIEnv* env,const char* name)
    {
    DebugBlockTracer tracer("FindClass");
    DebugMessage("%s\n",name);
    jobject obj=env->FindClass(name);
    DebugShowPointer(obj,name);
    return obj;
    }

jmethodID JVMBRIDGEFUNC(GetStaticMethodID)(JNIEnv* env,jobject cl,const char* name,const char* sig)
    {
    DebugBlockTracer tracer("GetStaticMethodID");
    DebugShowPointer(cl,"class");
    DebugMessage("%s %s\n",name,sig);
    jmethodID methodID = env->GetStaticMethodID(static_cast<jclass>(cl),name,sig);
    DebugShowPointer(methodID,"static method ID");
    return methodID;
    }

jmethodID JVMBRIDGEFUNC(GetMethodID)(JNIEnv* env,jobject cl,const char* name,const char* sig)
    {
    DebugBlockTracer tracer("GetMethodID");
    DebugShowPointer(cl,"class");
    DebugMessage("%s %s\n",name,sig);
    jmethodID methodID = env->GetMethodID(static_cast<jclass>(cl),name,sig);
    DebugShowPointer(methodID,"method ID");
    return methodID;
    }

jobject JVMBRIDGEFUNC(AllocObject) (JNIEnv* env,jobject cl)
    {
    DebugBlockTracer tracer("AllocObject");
    DebugShowPointer(cl,"class");
    jobject obj = env->AllocObject(static_cast<jclass>(cl));
    DebugShowPointer(obj,"raw object");
    return obj;
    }

jobject JVMBRIDGEFUNC(NewBooleanArray) (JNIEnv* env,jsize size)
    {
    DebugBlockTracer tracer("NewBooleanArray");
    DebugShowInt(size,"size");
    jobject array = env->NewBooleanArray(size);
    DebugShowPointer(array,"new array");
    return array;
    }

jobject JVMBRIDGEFUNC(NewByteArray) (JNIEnv* env,jsize size)
    {
    DebugBlockTracer tracer("NewByteArray");
    DebugShowInt(size,"size");
    jobject array = env->NewByteArray(size);
    DebugShowPointer(array,"new array");
    return array;
    }

jobject JVMBRIDGEFUNC(NewCharArray) (JNIEnv* env,jsize size)
    {
    DebugBlockTracer tracer("NewCharArray");
    DebugShowInt(size,"size");
    jobject array = env->NewCharArray(size);
    DebugShowPointer(array,"new array");
    return array;
    }

jobject JVMBRIDGEFUNC(NewShortArray) (JNIEnv* env,jsize size)
    {
    DebugBlockTracer tracer("NewShortArray");
    DebugShowInt(size,"size");
    jobject array = env->NewShortArray(size);
    DebugShowPointer(array,"new array");
    return array;
    }

jobject JVMBRIDGEFUNC(NewIntArray) (JNIEnv* env,jsize size)
    {
    DebugBlockTracer tracer("NewIntArray");
    DebugShowInt(size,"size");
    jobject array = env->NewIntArray(size);
    DebugShowPointer(array,"new array");
    return array;
    }

jobject JVMBRIDGEFUNC(NewLongArray) (JNIEnv* env,jsize size)
    {
    DebugBlockTracer tracer("NewLongArray");
    DebugShowInt(size,"size");
    jobject array = env->NewLongArray(size);
    DebugShowPointer(array,"new array");
    return array;
    }

jobject JVMBRIDGEFUNC(NewFloatArray) (JNIEnv* env,jsize size)
    {
    DebugBlockTracer tracer("NewFloatArray");
    DebugShowInt(size,"size");
    jobject array = env->NewFloatArray(size);
    DebugShowPointer(array,"new array");
    return array;
    }

jobject JVMBRIDGEFUNC(NewDoubleArray) (JNIEnv* env,jsize size)
    {
    DebugBlockTracer tracer("NewDoubleArray");
    DebugShowInt(size,"size");
    jobject array = env->NewDoubleArray(size);
    DebugShowPointer(array,"new array");
    return array;
    }

jobject JVMBRIDGEFUNC(NewObjectArray) (JNIEnv* env,jsize size,jobject cl,jobject init)
    {
    DebugBlockTracer tracer("NewObjectArray");
    DebugShowInt(size,"size");
    jobject array = env->NewObjectArray(size,static_cast<jclass>(cl),init);
    DebugShowPointer(array,"new array");
    return array;
    }

// safe for null pointers
void JVMBRIDGEFUNC(FreePointer) (char* cs)
    {
    DebugBlockTracer tracer("FreePointer");
    DebugShowPointer(cs,"ptr");
    if (cs)
        delete[] cs;
    }

jsize JVMBRIDGEFUNC(GetArrayLength) (JNIEnv* env,jobject array)
    {
    DebugBlockTracer tracer("GetArrayLength");
    return env->GetArrayLength(static_cast<jarray>(array));
    }

jboolean* JVMBRIDGEFUNC(GetBooleanArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len)
    {
    DebugBlockTracer tracer("GetBooleanArrayRegion");
    jboolean* buffer = new jboolean[len];
    env->GetBooleanArrayRegion(static_cast<jbooleanArray>(array),start,len,buffer);
    return buffer;
    }

jbyte* JVMBRIDGEFUNC(GetByteArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len)
    {
    DebugBlockTracer tracer("GetByteArrayRegion");
    jbyte* buffer = new jbyte[len];
    env->GetByteArrayRegion(static_cast<jbyteArray>(array),start,len,buffer);
    return buffer;
    }

jchar* JVMBRIDGEFUNC(GetCharArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len)
    {
    DebugBlockTracer tracer("GetCharArrayRegion");
    jchar* buffer = new jchar[len];
    env->GetCharArrayRegion(static_cast<jcharArray>(array),start,len,buffer);
    return buffer;
    }

jshort* JVMBRIDGEFUNC(GetShortArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len)
    {
    DebugBlockTracer tracer("GetShortArrayRegion");
    jshort* buffer = new jshort[len];
    env->GetShortArrayRegion(static_cast<jshortArray>(array),start,len,buffer);
    return buffer;
    }

jint* JVMBRIDGEFUNC(GetIntArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len)
    {
    DebugBlockTracer tracer("GetIntArrayRegion");
    jint* buffer = new jint[len];
    env->GetIntArrayRegion(static_cast<jintArray>(array),start,len,buffer);
    return buffer;
    }

jlong* JVMBRIDGEFUNC(GetLongArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len)
    {
    DebugBlockTracer tracer("GetLongArrayRegion");
    jlong* buffer = new jlong[len];
    env->GetLongArrayRegion(static_cast<jlongArray>(array),start,len,buffer);
    return buffer;
    }

jfloat* JVMBRIDGEFUNC(GetFloatArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len)
    {
    DebugBlockTracer tracer("GetFloatArrayRegion");
    jfloat* buffer = new jfloat[len];
    env->GetFloatArrayRegion(static_cast<jfloatArray>(array),start,len,buffer);
    return buffer;
    }

jdouble* JVMBRIDGEFUNC(GetDoubleArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len)
    {
    DebugBlockTracer tracer("GetDoubleArrayRegion");
    jdouble* buffer = new jdouble[len];
    env->GetDoubleArrayRegion(static_cast<jdoubleArray>(array),start,len,buffer);
    return buffer;
    }

jobject JVMBRIDGEFUNC(GetObjectArrayElement) (JNIEnv* env,jobject array,jsize start)
    {
    DebugBlockTracer tracer("GetObjectArrayElement");
    return env->GetObjectArrayElement(static_cast<jobjectArray>(array),start);
    }

void JVMBRIDGEFUNC(SetBooleanArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len,const jboolean* buffer)
    {
    DebugBlockTracer tracer("SetBooleanArrayRegion");
    DebugShowPointer(array,"array");
    DebugShowInt(start,"start");
    DebugShowInt(len,"len");
    env->SetBooleanArrayRegion(static_cast<jbooleanArray>(array),start,len,const_cast<jboolean*>(buffer));
    }

void JVMBRIDGEFUNC(SetByteArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len,const jbyte* buffer)
    {
    DebugBlockTracer tracer("SetByteArrayRegion");
    DebugShowPointer(array,"array");
    DebugShowInt(start,"start");
    DebugShowInt(len,"len");
    env->SetByteArrayRegion(static_cast<jbyteArray>(array),start,len,const_cast<jbyte*>(buffer));
    }

void JVMBRIDGEFUNC(SetCharArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len,const jchar* buffer)
    {
    DebugBlockTracer tracer("SetCharArrayRegion");
    DebugShowPointer(array,"array");
    DebugShowInt(start,"start");
    DebugShowInt(len,"len");
    env->SetCharArrayRegion(static_cast<jcharArray>(array),start,len,const_cast<jchar*>(buffer));
    }

void JVMBRIDGEFUNC(SetShortArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len,const jshort* buffer)
    {
    DebugBlockTracer tracer("SetShortArrayRegion");
    DebugShowPointer(array,"array");
    DebugShowInt(start,"start");
    DebugShowInt(len,"len");
    env->SetShortArrayRegion(static_cast<jshortArray>(array),start,len,const_cast<jshort*>(buffer));
    }

void JVMBRIDGEFUNC(SetIntArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len,const jint* buffer)
    {
    DebugBlockTracer tracer("SetIntArrayRegion");
    DebugShowPointer(array,"array");
    DebugShowInt(start,"start");
    DebugShowInt(len,"len");
    env->SetIntArrayRegion(static_cast<jintArray>(array),start,len,const_cast<jint*>(buffer));
    }

void JVMBRIDGEFUNC(SetLongArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len,const jlong* buffer)
    {
    DebugBlockTracer tracer("SetLongArrayRegion");
    DebugShowPointer(array,"array");
    DebugShowInt(start,"start");
    DebugShowInt(len,"len");
    env->SetLongArrayRegion(static_cast<jlongArray>(array),start,len,const_cast<jlong*>(buffer));
    }

void JVMBRIDGEFUNC(SetFloatArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len,const jfloat* buffer)
    {
    DebugBlockTracer tracer("SetFloatArrayRegion");
    DebugShowPointer(array,"array");
    DebugShowInt(start,"start");
    DebugShowInt(len,"len");
    env->SetFloatArrayRegion(static_cast<jfloatArray>(array),start,len,const_cast<jfloat*>(buffer));
    }

void JVMBRIDGEFUNC(SetDoubleArrayRegion) (JNIEnv* env,jobject array,jsize start,jsize len,const jdouble* buffer)
    {
    DebugBlockTracer tracer("SetDoubleArrayRegion");
    DebugShowPointer(array,"array");
    DebugShowInt(start,"start");
    DebugShowInt(len,"len");
    env->SetDoubleArrayRegion(static_cast<jdoubleArray>(array),start,len,const_cast<jdouble*>(buffer));
    }

void JVMBRIDGEFUNC(SetObjectArrayElement) (JNIEnv* env,jobject array,jsize start,jobject element)
    {
    DebugBlockTracer tracer("SetObjectArrayElement");
    DebugShowPointer(array,"array");
    DebugShowInt(start,"i");
    DebugShowPointer(element,"element");
    env->SetObjectArrayElement(static_cast<jobjectArray>(array),start,element);
    }

jfieldID JVMBRIDGEFUNC(GetStaticFieldID) (JNIEnv* env,jobject cl,const char* name,const char* sig)
    {
    DebugBlockTracer tracer("GetStaticFieldID");
    return env->GetStaticFieldID(static_cast<jclass>(cl),name,sig);
    }

jfieldID JVMBRIDGEFUNC(GetFieldID) (JNIEnv* env,jobject cl,const char* name,const char* sig)
    {
    DebugBlockTracer tracer("GetFieldID");
    return env->GetFieldID(static_cast<jclass>(cl),name,sig);
    }

jboolean JVMBRIDGEFUNC(GetStaticBooleanField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetStaticBooleanField");
    return env->GetStaticBooleanField(static_cast<jclass>(cl),fieldID);
    }

jbyte JVMBRIDGEFUNC(GetStaticByteField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetStaticByteField");
    return env->GetStaticByteField(static_cast<jclass>(cl),fieldID);
    }

jchar JVMBRIDGEFUNC(GetStaticCharField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetStaticCharField");
    return env->GetStaticCharField(static_cast<jclass>(cl),fieldID);
    }

jshort JVMBRIDGEFUNC(GetStaticShortField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetStaticShortField");
    return env->GetStaticShortField(static_cast<jclass>(cl),fieldID);
    }

jint JVMBRIDGEFUNC(GetStaticIntField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetStaticIntField");
    return env->GetStaticIntField(static_cast<jclass>(cl),fieldID);
    }

jlong JVMBRIDGEFUNC(GetStaticLongField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetStaticLongField");
    return env->GetStaticLongField(static_cast<jclass>(cl),fieldID);
    }

jfloat JVMBRIDGEFUNC(GetStaticFloatField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetStaticFloatField");
    return env->GetStaticFloatField(static_cast<jclass>(cl),fieldID);
    }

jdouble JVMBRIDGEFUNC(GetStaticDoubleField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetStaticDoubleField");
    return env->GetStaticDoubleField(static_cast<jclass>(cl),fieldID);
    }

jobject JVMBRIDGEFUNC(GetStaticObjectField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetStaticObjectField");
    return env->GetStaticObjectField(static_cast<jclass>(cl),fieldID);
    }

void JVMBRIDGEFUNC(SetStaticBooleanField) (JNIEnv* env,jobject cl,jfieldID fieldID,jboolean val)
    {
    DebugBlockTracer tracer("SetStaticBooleanField");
    env->SetStaticBooleanField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetStaticByteField) (JNIEnv* env,jobject cl,jfieldID fieldID,jbyte val)
    {
    DebugBlockTracer tracer("SetStaticByteField");
    env->SetStaticByteField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetStaticCharField) (JNIEnv* env,jobject cl,jfieldID fieldID,jchar val)
    {
    DebugBlockTracer tracer("SetStaticCharField");
    env->SetStaticCharField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetStaticShortField) (JNIEnv* env,jobject cl,jfieldID fieldID,jshort val)
    {
    DebugBlockTracer tracer("SetStaticShortField");
    env->SetStaticShortField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetStaticIntField) (JNIEnv* env,jobject cl,jfieldID fieldID,jint val)
    {
    DebugBlockTracer tracer("SetStaticIntField");
    env->SetStaticIntField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetStaticLongField) (JNIEnv* env,jobject cl,jfieldID fieldID,jlong val)
    {
    DebugBlockTracer tracer("SetStaticLongField");
    env->SetStaticLongField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetStaticFloatField) (JNIEnv* env,jobject cl,jfieldID fieldID,jfloat val)
    {
    DebugBlockTracer tracer("SetStaticFloatField");
    env->SetStaticFloatField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetStaticDoubleField) (JNIEnv* env,jobject cl,jfieldID fieldID,jdouble val)
    {
    DebugBlockTracer tracer("SetStaticDoubleField");
    env->SetStaticDoubleField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetStaticObjectField) (JNIEnv* env,jobject cl,jfieldID fieldID,jobject val)
    {
    DebugBlockTracer tracer("SetStaticObjectField");
    env->SetStaticObjectField(static_cast<jclass>(cl),fieldID,val);
    }

jboolean JVMBRIDGEFUNC(GetBooleanField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetBooleanField");
    return env->GetBooleanField(static_cast<jclass>(cl),fieldID);
    }

jbyte JVMBRIDGEFUNC(GetByteField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetByteField");
    return env->GetByteField(static_cast<jclass>(cl),fieldID);
    }

jchar JVMBRIDGEFUNC(GetCharField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetCharField");
    return env->GetCharField(static_cast<jclass>(cl),fieldID);
    }

jshort JVMBRIDGEFUNC(GetShortField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetShortField");
    return env->GetShortField(static_cast<jclass>(cl),fieldID);
    }

jint JVMBRIDGEFUNC(GetIntField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetIntField");
    return env->GetIntField(static_cast<jclass>(cl),fieldID);
    }

jlong JVMBRIDGEFUNC(GetLongField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetLongField");
    return env->GetLongField(static_cast<jclass>(cl),fieldID);
    }

jfloat JVMBRIDGEFUNC(GetFloatField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetFloatField");
    return env->GetFloatField(static_cast<jclass>(cl),fieldID);
    }

jdouble JVMBRIDGEFUNC(GetDoubleField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetDoubleField");
    return env->GetDoubleField(static_cast<jclass>(cl),fieldID);
    }

jobject JVMBRIDGEFUNC(GetObjectField) (JNIEnv* env,jobject cl,jfieldID fieldID)
    {
    DebugBlockTracer tracer("GetObjectField");
    return env->GetObjectField(static_cast<jclass>(cl),fieldID);
    }

void JVMBRIDGEFUNC(SetBooleanField) (JNIEnv* env,jobject cl,jfieldID fieldID,jboolean val)
    {
    DebugBlockTracer tracer("SetBooleanField");
    env->SetBooleanField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetByteField) (JNIEnv* env,jobject cl,jfieldID fieldID,jbyte val)
    {
    DebugBlockTracer tracer("SetByteField");
    env->SetByteField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetCharField) (JNIEnv* env,jobject cl,jfieldID fieldID,jchar val)
    {
    DebugBlockTracer tracer("SetCharField");
    env->SetCharField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetShortField) (JNIEnv* env,jobject cl,jfieldID fieldID,jshort val)
    {
    DebugBlockTracer tracer("SetShortField");
    env->SetShortField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetIntField) (JNIEnv* env,jobject cl,jfieldID fieldID,jint val)
    {
    DebugBlockTracer tracer("SetIntField");
    env->SetIntField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetLongField) (JNIEnv* env,jobject cl,jfieldID fieldID,jlong val)
    {
    DebugBlockTracer tracer("SetLongField");
    env->SetLongField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetFloatField) (JNIEnv* env,jobject cl,jfieldID fieldID,jfloat val)
    {
    DebugBlockTracer tracer("SetFloatField");
    env->SetFloatField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetDoubleField) (JNIEnv* env,jobject cl,jfieldID fieldID,jdouble val)
    {
    DebugBlockTracer tracer("SetDoubleField");
    env->SetDoubleField(static_cast<jclass>(cl),fieldID,val);
    }

void JVMBRIDGEFUNC(SetObjectField) (JNIEnv* env,jobject cl,jfieldID fieldID,jobject val)
    {
    DebugBlockTracer tracer("SetObjectField");
    env->SetObjectField(static_cast<jclass>(cl),fieldID,val);
    }

template <class T>
inline T CallMethodVL(JNIEnv* env,jobject object,jmethodID methodID,jvaluelist vl)
    {
    }

void JVMBRIDGEFUNC(CallVoidMethodVL)(JNIEnv* env,jobject object,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallVoidMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    env->CallVoidMethodA(object,methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    }

jboolean JVMBRIDGEFUNC(CallBooleanMethodVL)(JNIEnv* env,jobject object,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallBooleanMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jboolean result = env->CallBooleanMethodA(object,methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowBoolean(result,"returns");
    return result;
    }

jbyte JVMBRIDGEFUNC(CallByteMethodVL)(JNIEnv* env,jobject object,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallByteMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jbyte result = env->CallByteMethodA(object,methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowByte(result,"returns");
    return result;
    }

jchar JVMBRIDGEFUNC(CallCharMethodVL)(JNIEnv* env,jobject object,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallCharMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jchar result = env->CallCharMethodA(object,methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowChar(result,"returns");
    return result;
    }

jshort JVMBRIDGEFUNC(CallShortMethodVL)(JNIEnv* env,jobject object,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallShortMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jshort result = env->CallShortMethodA(object,methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowShort(result,"returns");
    return result;
    }

jint JVMBRIDGEFUNC(CallIntMethodVL)(JNIEnv* env,jobject object,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallIntMethodVL");
#if DEBUGGC
    DebugShowPointer(env,"env");
#endif
    DebugShowPointer(object,"object");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jint result = env->CallIntMethodA(object,methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowInt(result,"returns");
    return result;
    }

jlong JVMBRIDGEFUNC(CallLongMethodVL)(JNIEnv* env,jobject object,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallLongMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jlong result = env->CallLongMethodA(object,methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowLong(result,"returns");
    return result;
    }

jfloat JVMBRIDGEFUNC(CallFloatMethodVL)(JNIEnv* env,jobject object,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallFloatMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jfloat result = env->CallFloatMethodA(object,methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowFloat(result,"returns");
    return result;
    }

jdouble JVMBRIDGEFUNC(CallDoubleMethodVL)(JNIEnv* env,jobject object,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallDoubleMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jdouble result = env->CallDoubleMethodA(object,methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowDouble(result,"returns");
    return result;
    }

jobject JVMBRIDGEFUNC(CallObjectMethodVL)(JNIEnv* env,jobject object,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallObjectMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jobject result = env->CallObjectMethodA(object,methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowPointer(result,"returns");
    return result;
    }

void JVMBRIDGEFUNC(CallNonvirtualVoidMethodVL)(JNIEnv* env,jobject object,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallNonvirtualVoidMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    env->CallNonvirtualVoidMethodA(object,static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    }

jboolean JVMBRIDGEFUNC(CallNonvirtualBooleanMethodVL)(JNIEnv* env,jobject object,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallNonvirtualBooleanMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jboolean result = env->CallNonvirtualBooleanMethodA(object,static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowBoolean(result,"returns");
    return result;
    }

jbyte JVMBRIDGEFUNC(CallNonvirtualByteMethodVL)(JNIEnv* env,jobject object,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallNonvirtualByteMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jbyte result = env->CallNonvirtualByteMethodA(object,static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowByte(result,"returns");
    return result;
    }

jchar JVMBRIDGEFUNC(CallNonvirtualCharMethodVL)(JNIEnv* env,jobject object,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallNonvirtualCharMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jchar result = env->CallNonvirtualCharMethodA(object,static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowChar(result,"returns");
    return result;
    }

jshort JVMBRIDGEFUNC(CallNonvirtualShortMethodVL)(JNIEnv* env,jobject object,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallNonvirtualShortMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jshort result = env->CallNonvirtualShortMethodA(object,static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowShort(result,"returns");
    return result;
    }

jint JVMBRIDGEFUNC(CallNonvirtualIntMethodVL)(JNIEnv* env,jobject object,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallNonvirtualIntMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jint result = env->CallNonvirtualIntMethodA(object,static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowInt(result,"returns");
    return result;
    }

jlong JVMBRIDGEFUNC(CallNonvirtualLongMethodVL)(JNIEnv* env,jobject object,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallNonvirtualLongMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jlong result = env->CallNonvirtualLongMethodA(object,static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowLong(result,"returns");
    return result;
    }

jfloat JVMBRIDGEFUNC(CallNonvirtualFloatMethodVL)(JNIEnv* env,jobject object,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallNonvirtualFloatMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jfloat result = env->CallNonvirtualFloatMethodA(object,static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowFloat(result,"returns");
    return result;
    }

jdouble JVMBRIDGEFUNC(CallNonvirtualDoubleMethodVL)(JNIEnv* env,jobject object,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallNonvirtualDoubleMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jdouble result = env->CallNonvirtualDoubleMethodA(object,static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowDouble(result,"returns");
    return result;
    }

jobject JVMBRIDGEFUNC(CallNonvirtualObjectMethodVL)(JNIEnv* env,jobject object,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallNonvirtualObjectMethodVL");
    DebugShowPointer(object,"object");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jobject result = env->CallNonvirtualObjectMethodA(object,static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowRef(result,"returns");
    return result;
    }

void JVMBRIDGEFUNC(CallStaticVoidMethodVL)(JNIEnv* env,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallStaticVoidMethodVL");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    env->CallStaticVoidMethodA(static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    }

jboolean JVMBRIDGEFUNC(CallStaticBooleanMethodVL)(JNIEnv* env,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallStaticBooleanMethodVL");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jboolean result = env->CallStaticBooleanMethodA(static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowBoolean(result,"returns");
    return result;
    }

jbyte JVMBRIDGEFUNC(CallStaticByteMethodVL)(JNIEnv* env,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallStaticByteMethodVL");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jbyte result = env->CallStaticByteMethodA(static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowByte(result,"returns");
    return result;
    }

jchar JVMBRIDGEFUNC(CallStaticCharMethodVL)(JNIEnv* env,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallStaticCharMethodVL");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jchar result = env->CallStaticCharMethodA(static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowChar(result,"returns");
    return result;
    }

jshort JVMBRIDGEFUNC(CallStaticShortMethodVL)(JNIEnv* env,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallStaticShortMethodVL");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jshort result = env->CallStaticShortMethodA(static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowShort(result,"returns");
    return result;
    }

jint JVMBRIDGEFUNC(CallStaticIntMethodVL)(JNIEnv* env,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallStaticIntMethodVL");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jint result = env->CallStaticIntMethodA(static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowInt(result,"returns");
    return result;
    }

jlong JVMBRIDGEFUNC(CallStaticLongMethodVL)(JNIEnv* env,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallStaticLongMethodVL");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jlong result = env->CallStaticLongMethodA(static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowLong(result,"returns");
    return result;
    }

jfloat JVMBRIDGEFUNC(CallStaticFloatMethodVL)(JNIEnv* env,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallStaticFloatMethodVL");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jfloat result = env->CallStaticFloatMethodA(static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowFloat(result,"returns");
    return result;
    }

jdouble JVMBRIDGEFUNC(CallStaticDoubleMethodVL)(JNIEnv* env,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallStaticDoubleMethodVL");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jdouble result = env->CallStaticDoubleMethodA(static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowDouble(result,"returns");
    return result;
    }

jobject JVMBRIDGEFUNC(CallStaticObjectMethodVL)(JNIEnv* env,jobject cl,jmethodID methodID,jvaluelist vl)
    {
    DebugBlockTracer tracer("CallStaticObjectMethodVL");
    DebugShowPointer(cl,"class");
    DebugShowPointer(methodID,"method ID");
    DebugShowPointer(vl,"value-list");
    jvalue* valueArray = VectorToArray(GetValueList(vl));
    jobject result = env->CallStaticObjectMethodA(static_cast<jclass>(cl),methodID,valueArray);
    delete[] valueArray;
    JVMBRIDGEFUNC(DestroyValueList)(vl);
    DebugShowRef(result,"returns");
    return result;
    }

jboolean JVMBRIDGEFUNC(IsSameObject)(JNIEnv* env,jobject a,jobject b)
    {
    DebugBlockTracer tracer("IsSameObject");
    DebugShowPointer(a,"a");
    DebugShowPointer(b,"b");
    return env->IsSameObject(a,b);
    }

jboolean JVMBRIDGEFUNC(IsInstanceOf)(JNIEnv* env,jobject obj,jobject cl)
    {
    DebugBlockTracer tracer("IsInstanceOf");
    DebugShowPointer(obj,"obj");
    DebugShowPointer(cl,"cl");
    return env->IsInstanceOf(obj,static_cast<jclass>(cl));
    }

jint JVMBRIDGEFUNC(ThrowException)(JNIEnv* env,jobject th)
    {
    DebugBlockTracer tracer("ThrowException");
    DebugShowPointer(th,"exception");
    ExceptionDebugMessage("Throwing Exception = 0x%X\n",th);
    return env->Throw(static_cast<jthrowable>(th));
    }

jobject JVMBRIDGEFUNC(GetClearException)(JNIEnv* env)
    {
    DebugBlockTracer tracer("GetClearException");
    jobject ex = env->ExceptionOccurred();
    DebugMessage("Exception = 0x%X\n",ex);
#if DEBUGEXCEPTION
    ExceptionDebugMessage("Exception = 0x%X\n",ex);
    env->ExceptionDescribe();
#endif
    env->ExceptionClear();
    return ex;
    }

jboolean JVMBRIDGEFUNC(ExceptionPending)(JNIEnv* env)
    {
    DebugBlockTracer tracer("ExceptionPending");
    jboolean gotException =
#if JNI_VERSION_1_2
     IsV1_2(env)?
        env->ExceptionCheck():
#endif
        env->ExceptionOccurred() != 0
    ;
    DebugMessage("Got Exception? %d\n",gotException);
    if (gotException)
        ExceptionDebugMessage("Got Exception\n");
    return gotException;
    }

jint JVMBRIDGEFUNC(MonitorEnter) (JNIEnv* env,jobject object)
    {
    DebugBlockTracer tracer("MonitorEnter");
    return env->MonitorEnter(object);
    }

jint JVMBRIDGEFUNC(MonitorExit) (JNIEnv* env,jobject object)
    {
    DebugBlockTracer tracer("MonitorExit");
    return env->MonitorEnter(object);
    }

jobject JVMBRIDGEFUNC(NewGlobalRef) (JNIEnv* env,jobject localRef)
    {
    DebugBlockTracer tracer("NewGlobalRef");
    DebugShowRef(localRef,"local");
    jobject globalRef = env->NewGlobalRef(localRef);
    DebugShowRef(globalRef,"global");
    return globalRef;
    }

jobject JVMBRIDGEFUNC(ConvertToGlobalRef) (JNIEnv* env,jobject localRef)
    {
    DebugBlockTracer tracer("ConvertToGlobalRef");
    jobject globalRef = JVMBRIDGEFUNC(NewGlobalRef)(env,localRef);
    env->DeleteLocalRef(localRef);
    return globalRef;
    }

void JVMBRIDGEFUNC(DeleteGlobalRef) (JNIEnv* env,jobject globalRef)
    {
    DebugBlockTracer tracer("DeleteGlobalRef");
    DebugShowRef(globalRef,"global");
    env->DeleteGlobalRef(globalRef);
    }

void JVMBRIDGEFUNC(DeleteGlobalRefGC) (JavaVM* vm,jobject globalRef)
    {
#if DEBUGGC
    DebugBlockTracer tracer("DeleteGlobalRefGC");
#endif
    JNIEnv* env = JVMBRIDGEFUNC(AttachCurrentThread)(vm);
    // in 1.1, this is all we can do. And there's no way of detecting version
    // until we've done this.
#if DEBUGGC
    DebugShowRef(globalRef,"global");
#endif
    env->DeleteGlobalRef(globalRef);
//    JVMBRIDGEFUNC(DeleteGlobalRef)(env,globalRef);
    // not safe to detach the thread
    }

char* JVMBRIDGEFUNC(GetStringUTFChars) (JNIEnv* env,jobject jstrobj)
    {
    DebugBlockTracer tracer("GetStringUTFChars");
    jstring jstr = static_cast<jstring>(jstrobj);
    jsize len = env->GetStringUTFLength(jstr);
    char* buf = new char[len];
#if JNI_VERSION_1_2
    if (IsV1_2(env))
        env->GetStringUTFRegion(jstr,0,len,buf);
    else
#endif
        {
        jboolean copy;
        const char* bytes = env->GetStringUTFChars(jstr,&copy);
        memcpy(buf,bytes,len);
        if (copy)
            env->ReleaseStringUTFChars(jstr,bytes);
        }
    return buf;
    }

jint JVMBRIDGEFUNC(GetStringUTFLength) (JNIEnv* env,jobject jstr)
    {
    DebugBlockTracer tracer("GetStringUTFLength");
    return env->GetStringUTFLength(static_cast<jstring>(jstr));
    }

jobject JVMBRIDGEFUNC(DefineClass)
    (JNIEnv* env,const char* name,jobject loader,const jbyte* buf,jsize bufLen)
    {
    DebugBlockTracer tracer("DefineClass");
    DebugShowBytes(buf,bufLen);
    jobject cls = env->DefineClass(name,loader,buf,bufLen);
    DebugShowPointer(cls,name);
    return cls;
    }

