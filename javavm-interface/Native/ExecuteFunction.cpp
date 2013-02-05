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
#include "ExecuteFunctionControl.h"

const char* kExecuteFunctionClassName = EXECUTEFUNCTION_CLASSNAME;

template <typename T>
struct Attrs
    {
    typedef T (*Function) (JNIEnv* env,jvaluelist args);
    };

inline jvaluelist ToValueList(COpaqueAddress vl)
    {
    return reinterpret_cast<jvaluelist>(vl);
    }

// destroys argsVal
template <typename T>
inline T executeFunction(JNIEnv* env,COpaqueAddress functionVal,COpaqueAddress argsVal)
    {
    typedef typename Attrs<T>::Function Function;
    Function function = reinterpret_cast<Function>(functionVal);
    jvaluelist args = ToValueList(argsVal);
    T result;
        {
        DebugBlockTracer tracer("calling function");
        result = (*function)(env,args);
        }
    JVMBRIDGEFUNC(DestroyValueList)(args);
    return result;
    }

// destroys argsVal
template <>
inline void executeFunction<void>(JNIEnv* env,COpaqueAddress functionVal,COpaqueAddress argsVal)
    {
    typedef Attrs<void>::Function Function;
    Function function = reinterpret_cast<Function>(functionVal);
    jvaluelist args = ToValueList(argsVal);
        {
        DebugBlockTracer tracer("calling function");
        (*function)(env,args);
        }
    JVMBRIDGEFUNC(DestroyValueList)(args);
    }

extern "C" COpaqueAddress JNICALL createValueList (JNIEnv*,jclass)
    {
    DebugBlockTracer tracer("native createValueList");
    return reinterpret_cast<COpaqueAddress>(JVMBRIDGEFUNC(CreateValueList)());
    }

uintptr_t JVMBRIDGEFUNC(ConvertCallback) (void* addr)
    {
    DebugBlockTracer tracer("ConvertCallback");
    DebugShowPointer(addr,"addr");
    return reinterpret_cast<uintptr_t>(addr);
    }

extern "C" void JNICALL destroyValueList (JNIEnv*,jclass,COpaqueAddress vl)
    {
    DebugBlockTracer tracer("native destroyValueList");
    JVMBRIDGEFUNC(DestroyValueList)(ToValueList(vl));
    }

extern "C" void JNICALL addToValueList_Boolean (JNIEnv*,jclass,COpaqueAddress vl,jboolean v)
    {
    DebugBlockTracer tracer("native addToValueList __IZ");
    DebugShowBoolean(v,"adding");
    JVMBRIDGEFUNC(AddBooleanToValueList)(ToValueList(vl),v);
    }

extern "C" void JNICALL addToValueList_Byte (JNIEnv*,jclass,COpaqueAddress vl,jbyte v)
    {
    DebugBlockTracer tracer("native addToValueList __IB");
    DebugShowByte(v,"adding");
    JVMBRIDGEFUNC(AddByteToValueList)(ToValueList(vl),v);
    }

extern "C" void JNICALL addToValueList_Char (JNIEnv*,jclass,COpaqueAddress vl,jchar v)
    {
    DebugBlockTracer tracer("native addToValueList __IC");
    DebugShowChar(v,"adding");
    JVMBRIDGEFUNC(AddCharToValueList)(ToValueList(vl),v);
    }

extern "C" void JNICALL addToValueList_Short (JNIEnv*,jclass,COpaqueAddress vl,jshort v)
    {
    DebugBlockTracer tracer("native addToValueList __IS");
    DebugShowShort(v,"adding");
    JVMBRIDGEFUNC(AddShortToValueList)(ToValueList(vl),v);
    }

extern "C" void JNICALL addToValueList_Int (JNIEnv*,jclass,COpaqueAddress vl,jint v)
    {
    DebugBlockTracer tracer("native addToValueList __II");
    DebugShowInt(v,"adding");
    JVMBRIDGEFUNC(AddIntToValueList)(ToValueList(vl),v);
    }

extern "C" void JNICALL addToValueList_Long (JNIEnv*,jclass,COpaqueAddress vl,jlong v)
    {
    DebugBlockTracer tracer("native addToValueList __IJ");
    DebugShowLong(v,"adding");
    JVMBRIDGEFUNC(AddLongToValueList)(ToValueList(vl),v);
    }

extern "C" void JNICALL addToValueList_Float (JNIEnv*,jclass,COpaqueAddress vl,jfloat v)
    {
    DebugBlockTracer tracer("native addToValueList __IF");
    DebugShowFloat(v,"adding");
    JVMBRIDGEFUNC(AddFloatToValueList)(ToValueList(vl),v);
    }

extern "C" void JNICALL addToValueList_Double (JNIEnv*,jclass,COpaqueAddress vl,jdouble v)
    {
    DebugBlockTracer tracer("native addToValueList __ID");
    DebugShowDouble(v,"adding");
    JVMBRIDGEFUNC(AddDoubleToValueList)(ToValueList(vl),v);
    }

extern "C" void JNICALL addToValueList_Object (JNIEnv* env,jclass,COpaqueAddress vl,jobject v)
    {
    DebugBlockTracer tracer("native addToValueList __ILjava_lang_Object_2");
    DebugShowRef(v,"local");
    jobject globalRef = env->NewGlobalRef(v);
    DebugShowRef(globalRef,"adding");
    JVMBRIDGEFUNC(AddObjectToValueList)(ToValueList(vl),globalRef);
    }

#define DebugOpaqueAddress DebugShowInt

// destroys argsVal
extern "C" void JNICALL executeVoidFunctionNow
(JNIEnv* env,jclass,COpaqueAddress functionVal,COpaqueAddress argsVal)
    {
    DebugBlockTracer tracer("native executeVoidFunctionNow");
    DebugOpaqueAddress(functionVal,"function");
    DebugOpaqueAddress(argsVal,"args");
    executeFunction<void>(env,functionVal,argsVal);
    }

// destroys argsVal
extern "C" jboolean JNICALL executeBooleanFunctionNow
(JNIEnv* env,jclass,COpaqueAddress functionVal,COpaqueAddress argsVal)
    {
    DebugBlockTracer tracer("native executeBooleanFunctionNow");
    DebugOpaqueAddress(functionVal,"function");
    DebugOpaqueAddress(argsVal,"args");
    return executeFunction<jboolean>(env,functionVal,argsVal);
    }

// destroys argsVal
extern "C" jbyte JNICALL executeByteFunctionNow
(JNIEnv* env,jclass,COpaqueAddress functionVal,COpaqueAddress argsVal)
    {
    DebugBlockTracer tracer("native executeByteFunctionNow");
    DebugOpaqueAddress(functionVal,"function");
    DebugOpaqueAddress(argsVal,"args");
    return executeFunction<jbyte>(env,functionVal,argsVal);
    }

// destroys argsVal
extern "C" jchar JNICALL executeCharFunctionNow
(JNIEnv* env,jclass,COpaqueAddress functionVal,COpaqueAddress argsVal)
    {
    DebugBlockTracer tracer("native executeCharFunctionNow");
    DebugOpaqueAddress(functionVal,"function");
    DebugOpaqueAddress(argsVal,"args");
    return executeFunction<jchar>(env,functionVal,argsVal);
    }

// destroys argsVal
extern "C" jshort JNICALL executeShortFunctionNow
(JNIEnv* env,jclass,COpaqueAddress functionVal,COpaqueAddress argsVal)
    {
    DebugBlockTracer tracer("native executeShortFunctionNow");
    DebugOpaqueAddress(functionVal,"function");
    DebugOpaqueAddress(argsVal,"args");
    return executeFunction<jshort>(env,functionVal,argsVal);
    }

// destroys argsVal
extern "C" jint JNICALL executeIntFunctionNow
(JNIEnv* env,jclass,COpaqueAddress functionVal,COpaqueAddress argsVal)
    {
    DebugBlockTracer tracer("native executeIntFunctionNow");
    DebugOpaqueAddress(functionVal,"function");
    DebugOpaqueAddress(argsVal,"args");
    return executeFunction<jint>(env,functionVal,argsVal);
    }

// destroys argsVal
extern "C" jlong JNICALL executeLongFunctionNow
(JNIEnv* env,jclass,COpaqueAddress functionVal,COpaqueAddress argsVal)
    {
    DebugBlockTracer tracer("native executeLongFunctionNow");
    DebugOpaqueAddress(functionVal,"function");
    DebugOpaqueAddress(argsVal,"args");
    return executeFunction<jlong>(env,functionVal,argsVal);
    }

// destroys argsVal
extern "C" jfloat JNICALL executeFloatFunctionNow
(JNIEnv* env,jclass,COpaqueAddress functionVal,COpaqueAddress argsVal)
    {
    DebugBlockTracer tracer("native executeFloatFunctionNow");
    DebugOpaqueAddress(functionVal,"function");
    DebugOpaqueAddress(argsVal,"args");
    return executeFunction<jfloat>(env,functionVal,argsVal);
    }

// destroys argsVal
extern "C" jdouble JNICALL executeDoubleFunctionNow
(JNIEnv* env,jclass,COpaqueAddress functionVal,COpaqueAddress argsVal)
    {
    DebugBlockTracer tracer("native executeDoubleFunctionNow");
    DebugOpaqueAddress(functionVal,"function");
    DebugOpaqueAddress(argsVal,"args");
    return executeFunction<jdouble>(env,functionVal,argsVal);
    }

// destroys argsVal
extern "C" jobject JNICALL executeObjectFunctionNow
(JNIEnv* env,jclass,COpaqueAddress functionVal,COpaqueAddress argsVal)
    {
    DebugBlockTracer tracer("native executeObjectFunctionNow");
    DebugOpaqueAddress(functionVal,"function");
    DebugOpaqueAddress(argsVal,"args");
    return executeFunction<jobject>(env,functionVal,argsVal);
    }

FreeFunction gFree;

extern "C" void JNICALL freeFunctionNow
(JNIEnv*,jclass,COpaqueAddress functionVal)
    {
    DebugBlockTracer tracer("native freeFunctionNow");
    DebugOpaqueAddress(functionVal,"function");
    gFree(reinterpret_cast<void*>(functionVal));
    }

const jint nNativeBindings=22;

template <typename T>
inline void* cast_f(T x)
    {
    return reinterpret_cast<void*>(reinterpret_cast<COpaqueAddress>(x));    // hack to avoid warning
    }

const JNINativeMethod nativeBindings[] =
    {
        {const_cast<char*>("createValueList"),const_cast<char*>("()" SigOpaqueAddress),cast_f(&createValueList)},
        {const_cast<char*>("destroyValueList"),const_cast<char*>("(" SigOpaqueAddress ")V"),cast_f(&destroyValueList)},
        {const_cast<char*>("addToValueList"),const_cast<char*>("(" SigOpaqueAddress "Z)V"),cast_f(&addToValueList_Boolean)},
        {const_cast<char*>("addToValueList"),const_cast<char*>("(" SigOpaqueAddress "B)V"),cast_f(&addToValueList_Byte)},
        {const_cast<char*>("addToValueList"),const_cast<char*>("(" SigOpaqueAddress "C)V"),cast_f(&addToValueList_Char)},
        {const_cast<char*>("addToValueList"),const_cast<char*>("(" SigOpaqueAddress "S)V"),cast_f(&addToValueList_Short)},
        {const_cast<char*>("addToValueList"),const_cast<char*>("(" SigOpaqueAddress "I)V"),cast_f(&addToValueList_Int)},
        {const_cast<char*>("addToValueList"),const_cast<char*>("(" SigOpaqueAddress "J)V"),cast_f(&addToValueList_Long)},
        {const_cast<char*>("addToValueList"),const_cast<char*>("(" SigOpaqueAddress "F)V"),cast_f(&addToValueList_Float)},
        {const_cast<char*>("addToValueList"),const_cast<char*>("(" SigOpaqueAddress "D)V"),cast_f(&addToValueList_Double)},
        {const_cast<char*>("addToValueList"),const_cast<char*>("(" SigOpaqueAddress "Ljava/lang/Object;)V"),cast_f(&addToValueList_Object)},
        {const_cast<char*>("executeVoidFunctionNow")    ,const_cast<char*>("(" SigOpaqueAddress SigOpaqueAddress ")V"),cast_f(&executeVoidFunctionNow)},
        {const_cast<char*>("executeBooleanFunctionNow"),const_cast<char*>("(" SigOpaqueAddress SigOpaqueAddress ")Z"),cast_f(&executeBooleanFunctionNow)},
        {const_cast<char*>("executeByteFunctionNow")    ,const_cast<char*>("(" SigOpaqueAddress SigOpaqueAddress ")B"),cast_f(&executeByteFunctionNow)},
        {const_cast<char*>("executeCharFunctionNow")    ,const_cast<char*>("(" SigOpaqueAddress SigOpaqueAddress ")C"),cast_f(&executeCharFunctionNow)},
        {const_cast<char*>("executeShortFunctionNow")    ,const_cast<char*>("(" SigOpaqueAddress SigOpaqueAddress ")S"),cast_f(&executeShortFunctionNow)},
        {const_cast<char*>("executeIntFunctionNow")    ,const_cast<char*>("(" SigOpaqueAddress SigOpaqueAddress ")I"),cast_f(&executeIntFunctionNow)},
        {const_cast<char*>("executeLongFunctionNow")    ,const_cast<char*>("(" SigOpaqueAddress SigOpaqueAddress ")J"),cast_f(&executeLongFunctionNow)},
        {const_cast<char*>("executeFloatFunctionNow")    ,const_cast<char*>("(" SigOpaqueAddress SigOpaqueAddress ")F"),cast_f(&executeFloatFunctionNow)},
        {const_cast<char*>("executeDoubleFunctionNow")    ,const_cast<char*>("(" SigOpaqueAddress SigOpaqueAddress ")D"),cast_f(&executeDoubleFunctionNow)},
        {const_cast<char*>("executeObjectFunctionNow")    ,const_cast<char*>("(" SigOpaqueAddress SigOpaqueAddress ")Ljava/lang/Object;"),cast_f(&executeObjectFunctionNow)},
        {const_cast<char*>("freeFunctionNow"),const_cast<char*>("(" SigOpaqueAddress ")V"),cast_f(&freeFunctionNow)}
    };

inline void RegisterExecuteFunctionBindings(JNIEnv* env)
    {
    DebugBlockTracer tracer("RegisterExecuteFunctionBindings");
    jclass theClass = env->FindClass(kExecuteFunctionClassName);
    if (!theClass)
     ErrorMessage("Failed to load Java Class %s: Wrong CLASSPATH?\n", kExecuteFunctionClassName);
    jint result = env->RegisterNatives(theClass,nativeBindings,nNativeBindings);
    DebugShowInt(result,"result");
    if (result)
        {
        env->ExceptionDescribe();
        ErrorMessage("Failed in JNI RegisterNatives for Class %s: %x\n", kExecuteFunctionClassName, result);
        }
    InfoDebugMessage("RegisterExecuteFunctionBindings succeeded\n");
    };

void JVMBridge_StartExecuteFunction(JNIEnv* env,FreeFunction freeF)
    {
    RegisterExecuteFunctionBindings(env);
    gFree = freeF;
    };
