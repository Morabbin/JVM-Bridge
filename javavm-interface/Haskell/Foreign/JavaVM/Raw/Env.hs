{--
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
--}

module Foreign.JavaVM.Raw.Env where
    {
    import Foreign.JavaVM.Raw.ValueList;
    import Foreign.JavaVM.Raw.Types;
    import Foreign.JavaVM.Configure;
    import Foreign;

    type RawStringPtr = Ptr Word8;

    newtype RawEnv              = MkRawEnv              (Ptr ());
    newtype RawStaticMethodID   = MkRawStaticMethodID   (Ptr ());
    newtype RawMethodID         = MkRawMethodID         (Ptr ());
    newtype RawStaticFieldID    = MkRawStaticFieldID    (Ptr ());
    newtype RawFieldID          = MkRawFieldID          (Ptr ());

    -- classes, fields and methods
    
    foreign import ccall "JVMBridge.h JVMBridge_FindClass"                        rawFindClass                    ::
        RawEnv -> RawStringPtr -> IO RawLocalRef;
    foreign import ccall "JVMBridge.h JVMBridge_GetStaticMethodID"                rawGetStaticMethodID            ::
        RawEnv -> RawGlobalRef -> RawStringPtr -> RawStringPtr -> IO RawStaticMethodID;
    foreign import ccall "JVMBridge.h JVMBridge_GetMethodID"                    rawGetMethodID                    ::
        RawEnv -> RawGlobalRef -> RawStringPtr -> RawStringPtr -> IO RawMethodID;
    foreign import ccall "JVMBridge.h JVMBridge_GetStaticFieldID"                rawGetStaticFieldID                ::
        RawEnv -> RawGlobalRef -> RawStringPtr -> RawStringPtr -> IO RawStaticFieldID;
    foreign import ccall "JVMBridge.h JVMBridge_GetFieldID"                        rawGetFieldID                    ::
        RawEnv -> RawGlobalRef -> RawStringPtr -> RawStringPtr -> IO RawFieldID;
    foreign import ccall "JVMBridge.h JVMBridge_ConvertCallback"                rawConvertCallback                ::
        FunPtr a -> IO OpaqueAddress;

    -- alloc object, free pointer

    foreign import ccall "JVMBridge.h JVMBridge_AllocObject"                    rawAllocObject                    ::
        RawEnv -> RawGlobalRef -> IO RawLocalRef;
    foreign import ccall "JVMBridge.h JVMBridge_FreePointer"                    rawFreePointer                    ::
        Ptr a -> IO ();

    -- arrays

    foreign import ccall "JVMBridge.h JVMBridge_NewBooleanArray"                rawNewBooleanArray                ::
        RawEnv -> RawSize -> IO RawLocalRef;
    foreign import ccall "JVMBridge.h JVMBridge_NewByteArray"                    rawNewByteArray                    ::
        RawEnv -> RawSize -> IO RawLocalRef;
    foreign import ccall "JVMBridge.h JVMBridge_NewCharArray"                    rawNewCharArray                    ::
        RawEnv -> RawSize -> IO RawLocalRef;
    foreign import ccall "JVMBridge.h JVMBridge_NewShortArray"                    rawNewShortArray                ::
        RawEnv -> RawSize -> IO RawLocalRef;
    foreign import ccall "JVMBridge.h JVMBridge_NewIntArray"                    rawNewIntArray                    ::
        RawEnv -> RawSize -> IO RawLocalRef;
    foreign import ccall "JVMBridge.h JVMBridge_NewLongArray"                    rawNewLongArray                    ::
        RawEnv -> RawSize -> IO RawLocalRef;
    foreign import ccall "JVMBridge.h JVMBridge_NewFloatArray"                    rawNewFloatArray                ::
        RawEnv -> RawSize -> IO RawLocalRef;
    foreign import ccall "JVMBridge.h JVMBridge_NewDoubleArray"                    rawNewDoubleArray                ::
        RawEnv -> RawSize -> IO RawLocalRef;
    foreign import ccall "JVMBridge.h JVMBridge_NewObjectArray"                    rawNewRefArray                    ::
        RawEnv -> RawSize -> RawGlobalRef -> RawGlobalRef -> IO RawLocalRef;

    foreign import ccall "JVMBridge.h JVMBridge_GetArrayLength"                    rawGetArrayLength                ::
        RawEnv -> RawGlobalRef -> IO RawSize;

    foreign import ccall "JVMBridge.h JVMBridge_GetBooleanArrayRegion"            rawGetBooleanArrayRegion        ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> IO (Ptr RawBoolean);
    foreign import ccall "JVMBridge.h JVMBridge_GetByteArrayRegion"                rawGetByteArrayRegion            ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> IO (Ptr RawByte);
    foreign import ccall "JVMBridge.h JVMBridge_GetCharArrayRegion"                rawGetCharArrayRegion            ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> IO (Ptr RawChar);
    foreign import ccall "JVMBridge.h JVMBridge_GetShortArrayRegion"            rawGetShortArrayRegion            ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> IO (Ptr RawShort);
    foreign import ccall "JVMBridge.h JVMBridge_GetIntArrayRegion"                rawGetIntArrayRegion            ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> IO (Ptr RawInt);
    foreign import ccall "JVMBridge.h JVMBridge_GetLongArrayRegion"                rawGetLongArrayRegion            ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> IO (Ptr RawLong);
    foreign import ccall "JVMBridge.h JVMBridge_GetFloatArrayRegion"            rawGetFloatArrayRegion            ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> IO (Ptr RawFloat);
    foreign import ccall "JVMBridge.h JVMBridge_GetDoubleArrayRegion"            rawGetDoubleArrayRegion            ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> IO (Ptr RawDouble);
    foreign import ccall "JVMBridge.h JVMBridge_GetObjectArrayElement"            rawGetRefArrayElement            ::
        RawEnv -> RawGlobalRef -> RawSize -> IO RawLocalRef;

    foreign import ccall "JVMBridge.h JVMBridge_SetBooleanArrayRegion"            rawSetBooleanArrayRegion        ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> Ptr RawBoolean -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetByteArrayRegion"                rawSetByteArrayRegion            ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> Ptr RawByte -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetCharArrayRegion"                rawSetCharArrayRegion            ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> Ptr RawChar -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetShortArrayRegion"            rawSetShortArrayRegion            ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> Ptr RawShort -> IO ();    
    foreign import ccall "JVMBridge.h JVMBridge_SetIntArrayRegion"                rawSetIntArrayRegion            ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> Ptr RawInt -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetLongArrayRegion"                rawSetLongArrayRegion            ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> Ptr RawLong -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetFloatArrayRegion"            rawSetFloatArrayRegion            ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> Ptr RawFloat -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetDoubleArrayRegion"            rawSetDoubleArrayRegion            ::
        RawEnv -> RawGlobalRef -> RawSize -> RawSize -> Ptr RawDouble -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetObjectArrayElement"            rawSetRefArrayElement            ::
        RawEnv -> RawGlobalRef -> RawSize -> RawGlobalRef -> IO ();

    -- field access

    foreign import ccall "JVMBridge.h JVMBridge_GetStaticBooleanField"            rawGetStaticBooleanField        ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> IO RawBoolean;
    foreign import ccall "JVMBridge.h JVMBridge_GetStaticByteField"                rawGetStaticByteField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> IO RawByte;
    foreign import ccall "JVMBridge.h JVMBridge_GetStaticCharField"                rawGetStaticCharField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> IO RawChar;
    foreign import ccall "JVMBridge.h JVMBridge_GetStaticShortField"            rawGetStaticShortField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> IO RawShort;
    foreign import ccall "JVMBridge.h JVMBridge_GetStaticIntField"                rawGetStaticIntField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> IO RawInt;
    foreign import ccall "JVMBridge.h JVMBridge_GetStaticLongField"                rawGetStaticLongField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> IO RawLong;
    foreign import ccall "JVMBridge.h JVMBridge_GetStaticFloatField"            rawGetStaticFloatField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> IO RawFloat;
    foreign import ccall "JVMBridge.h JVMBridge_GetStaticDoubleField"            rawGetStaticDoubleField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> IO RawDouble;
    foreign import ccall "JVMBridge.h JVMBridge_GetStaticObjectField"            rawGetStaticRefField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> IO RawLocalRef;

    foreign import ccall "JVMBridge.h JVMBridge_SetStaticBooleanField"            rawSetStaticBooleanField        ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> RawBoolean -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetStaticByteField"                rawSetStaticByteField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> RawByte -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetStaticCharField"                rawSetStaticCharField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> RawChar -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetStaticShortField"            rawSetStaticShortField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> RawShort -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetStaticIntField"                rawSetStaticIntField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> RawInt -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetStaticLongField"                rawSetStaticLongField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> RawLong -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetStaticFloatField"            rawSetStaticFloatField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> RawFloat -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetStaticDoubleField"            rawSetStaticDoubleField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> RawDouble -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetStaticObjectField"            rawSetStaticRefField            ::
        RawEnv -> RawGlobalRef -> RawStaticFieldID -> RawGlobalRef -> IO ();

    foreign import ccall "JVMBridge.h JVMBridge_GetBooleanField"                rawGetBooleanField                ::
        RawEnv -> RawGlobalRef -> RawFieldID -> IO RawBoolean;
    foreign import ccall "JVMBridge.h JVMBridge_GetByteField"                    rawGetByteField                    ::
        RawEnv -> RawGlobalRef -> RawFieldID -> IO RawByte;
    foreign import ccall "JVMBridge.h JVMBridge_GetCharField"                    rawGetCharField                    ::
        RawEnv -> RawGlobalRef -> RawFieldID -> IO RawChar;
    foreign import ccall "JVMBridge.h JVMBridge_GetShortField"                    rawGetShortField                ::
        RawEnv -> RawGlobalRef -> RawFieldID -> IO RawShort;
    foreign import ccall "JVMBridge.h JVMBridge_GetIntField"                    rawGetIntField                    ::
        RawEnv -> RawGlobalRef -> RawFieldID -> IO RawInt;
    foreign import ccall "JVMBridge.h JVMBridge_GetLongField"                    rawGetLongField                    ::
        RawEnv -> RawGlobalRef -> RawFieldID -> IO RawLong;
    foreign import ccall "JVMBridge.h JVMBridge_GetFloatField"                    rawGetFloatField                ::
        RawEnv -> RawGlobalRef -> RawFieldID -> IO RawFloat;
    foreign import ccall "JVMBridge.h JVMBridge_GetDoubleField"                    rawGetDoubleField                ::
        RawEnv -> RawGlobalRef -> RawFieldID -> IO RawDouble;
    foreign import ccall "JVMBridge.h JVMBridge_GetObjectField"                    rawGetRefField                    ::
        RawEnv -> RawGlobalRef -> RawFieldID -> IO RawLocalRef;

    foreign import ccall "JVMBridge.h JVMBridge_SetBooleanField"                rawSetBooleanField                ::
        RawEnv -> RawGlobalRef -> RawFieldID -> RawBoolean -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetByteField"                    rawSetByteField                    ::
        RawEnv -> RawGlobalRef -> RawFieldID -> RawByte -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetCharField"                    rawSetCharField                    ::
        RawEnv -> RawGlobalRef -> RawFieldID -> RawChar -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetShortField"                    rawSetShortField                ::
        RawEnv -> RawGlobalRef -> RawFieldID -> RawShort -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetIntField"                    rawSetIntField                    ::
        RawEnv -> RawGlobalRef -> RawFieldID -> RawInt -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetLongField"                    rawSetLongField                    ::
        RawEnv -> RawGlobalRef -> RawFieldID -> RawLong -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetFloatField"                    rawSetFloatField                ::
        RawEnv -> RawGlobalRef -> RawFieldID -> RawFloat -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetDoubleField"                    rawSetDoubleField                ::
        RawEnv -> RawGlobalRef -> RawFieldID -> RawDouble -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_SetObjectField"                    rawSetRefField                    ::
        RawEnv -> RawGlobalRef -> RawFieldID -> RawGlobalRef -> IO ();

    -- method-calling

    foreign import ccall "JVMBridge.h JVMBridge_CallStaticVoidMethodVL"            rawCallStaticVoidMethod            ::
        RawEnv -> RawGlobalRef -> RawStaticMethodID -> RawValueList -> IO RawVoid;
    foreign import ccall "JVMBridge.h JVMBridge_CallStaticBooleanMethodVL"        rawCallStaticBooleanMethod        ::
        RawEnv -> RawGlobalRef -> RawStaticMethodID -> RawValueList -> IO RawBoolean;
    foreign import ccall "JVMBridge.h JVMBridge_CallStaticByteMethodVL"            rawCallStaticByteMethod            ::
        RawEnv -> RawGlobalRef -> RawStaticMethodID -> RawValueList -> IO RawByte;
    foreign import ccall "JVMBridge.h JVMBridge_CallStaticCharMethodVL"            rawCallStaticCharMethod            ::
        RawEnv -> RawGlobalRef -> RawStaticMethodID -> RawValueList -> IO RawChar;
    foreign import ccall "JVMBridge.h JVMBridge_CallStaticShortMethodVL"        rawCallStaticShortMethod        ::
        RawEnv -> RawGlobalRef -> RawStaticMethodID -> RawValueList -> IO RawShort;
    foreign import ccall "JVMBridge.h JVMBridge_CallStaticIntMethodVL"            rawCallStaticIntMethod            ::
        RawEnv -> RawGlobalRef -> RawStaticMethodID -> RawValueList -> IO RawInt;
    foreign import ccall "JVMBridge.h JVMBridge_CallStaticLongMethodVL"            rawCallStaticLongMethod            ::
        RawEnv -> RawGlobalRef -> RawStaticMethodID -> RawValueList -> IO RawLong;
    foreign import ccall "JVMBridge.h JVMBridge_CallStaticFloatMethodVL"        rawCallStaticFloatMethod        ::
        RawEnv -> RawGlobalRef -> RawStaticMethodID -> RawValueList -> IO RawFloat;
    foreign import ccall "JVMBridge.h JVMBridge_CallStaticDoubleMethodVL"        rawCallStaticDoubleMethod        ::
        RawEnv -> RawGlobalRef -> RawStaticMethodID -> RawValueList -> IO RawDouble;
    foreign import ccall "JVMBridge.h JVMBridge_CallStaticObjectMethodVL"        rawCallStaticRefMethod            ::
        RawEnv -> RawGlobalRef -> RawStaticMethodID -> RawValueList -> IO RawLocalRef;

    foreign import ccall "JVMBridge.h JVMBridge_CallVoidMethodVL"                rawCallVoidMethod                ::
        RawEnv -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawVoid;
    foreign import ccall "JVMBridge.h JVMBridge_CallBooleanMethodVL"            rawCallBooleanMethod            ::
        RawEnv -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawBoolean;
    foreign import ccall "JVMBridge.h JVMBridge_CallByteMethodVL"                rawCallByteMethod                ::
        RawEnv -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawByte;
    foreign import ccall "JVMBridge.h JVMBridge_CallCharMethodVL"                rawCallCharMethod                ::
        RawEnv -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawChar;
    foreign import ccall "JVMBridge.h JVMBridge_CallShortMethodVL"                rawCallShortMethod                ::
        RawEnv -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawShort;
    foreign import ccall "JVMBridge.h JVMBridge_CallIntMethodVL"                rawCallIntMethod                ::
        RawEnv -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawInt;
    foreign import ccall "JVMBridge.h JVMBridge_CallLongMethodVL"                rawCallLongMethod                ::
        RawEnv -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawLong;
    foreign import ccall "JVMBridge.h JVMBridge_CallFloatMethodVL"                rawCallFloatMethod                ::
        RawEnv -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawFloat;
    foreign import ccall "JVMBridge.h JVMBridge_CallDoubleMethodVL"                rawCallDoubleMethod                ::
        RawEnv -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawDouble;
    foreign import ccall "JVMBridge.h JVMBridge_CallObjectMethodVL"                rawCallRefMethod                ::
        RawEnv -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawLocalRef;

    foreign import ccall "JVMBridge.h JVMBridge_CallNonvirtualVoidMethodVL"        rawCallNonvirtualVoidMethod        ::
        RawEnv -> RawGlobalRef -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawVoid;
    foreign import ccall "JVMBridge.h JVMBridge_CallNonvirtualBooleanMethodVL"    rawCallNonvirtualBooleanMethod    ::
        RawEnv -> RawGlobalRef -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawBoolean;
    foreign import ccall "JVMBridge.h JVMBridge_CallNonvirtualByteMethodVL"        rawCallNonvirtualByteMethod        ::
        RawEnv -> RawGlobalRef -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawByte;
    foreign import ccall "JVMBridge.h JVMBridge_CallNonvirtualCharMethodVL"        rawCallNonvirtualCharMethod        ::
        RawEnv -> RawGlobalRef -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawChar;
    foreign import ccall "JVMBridge.h JVMBridge_CallNonvirtualShortMethodVL"    rawCallNonvirtualShortMethod    ::
        RawEnv -> RawGlobalRef -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawShort;
    foreign import ccall "JVMBridge.h JVMBridge_CallNonvirtualIntMethodVL"        rawCallNonvirtualIntMethod        ::
        RawEnv -> RawGlobalRef -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawInt;
    foreign import ccall "JVMBridge.h JVMBridge_CallNonvirtualLongMethodVL"        rawCallNonvirtualLongMethod        ::
        RawEnv -> RawGlobalRef -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawLong;
    foreign import ccall "JVMBridge.h JVMBridge_CallNonvirtualFloatMethodVL"    rawCallNonvirtualFloatMethod    ::
        RawEnv -> RawGlobalRef -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawFloat;
    foreign import ccall "JVMBridge.h JVMBridge_CallNonvirtualDoubleMethodVL"    rawCallNonvirtualDoubleMethod    ::
        RawEnv -> RawGlobalRef -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawDouble;
    foreign import ccall "JVMBridge.h JVMBridge_CallNonvirtualObjectMethodVL"    rawCallNonvirtualRefMethod        ::
        RawEnv -> RawGlobalRef -> RawGlobalRef -> RawMethodID -> RawValueList -> IO RawLocalRef;

    -- object properties

    foreign import ccall "JVMBridge.h JVMBridge_IsSameObject"                    rawIsSameObject                    ::
        RawEnv -> RawGlobalRef -> RawGlobalRef -> IO RawBoolean;
    foreign import ccall "JVMBridge.h JVMBridge_IsInstanceOf"                    rawIsInstanceOf                    ::
        RawEnv -> RawGlobalRef -> RawGlobalRef -> IO RawBoolean;

    -- exceptions

    foreign import ccall "JVMBridge.h JVMBridge_ThrowException"                    rawThrowException                ::
        RawEnv -> RawGlobalRef -> IO RawInt;
    foreign import ccall "JVMBridge.h JVMBridge_GetClearException"                rawGetClearException            ::
        RawEnv -> IO RawLocalRef;
    foreign import ccall "JVMBridge.h JVMBridge_ExceptionPending"                rawExceptionPending                ::
        RawEnv -> IO RawBoolean;
        
    -- monitors

    foreign import ccall "JVMBridge.h JVMBridge_MonitorEnter"                    rawMonitorEnter                    ::
        RawEnv -> RawGlobalRef -> IO ();
    foreign import ccall "JVMBridge.h JVMBridge_MonitorExit"                    rawMonitorExit                    ::
        RawEnv -> RawGlobalRef -> IO ();

    -- global references

    foreign import ccall "JVMBridge.h JVMBridge_ConvertToGlobalRef"                rawConvertToGlobalRef            ::
        RawEnv -> RawLocalRef -> IO RawGlobalRef;
    foreign import ccall "JVMBridge.h JVMBridge_DeleteGlobalRef"                rawDeleteGlobalRef                ::
        RawEnv -> RawGlobalRef -> IO ();
    
    -- strings
    
    foreign import ccall "JVMBridge.h JVMBridge_GetStringUTFChars"                rawGetStringUTFChars            ::
        RawEnv -> RawGlobalRef -> IO RawStringPtr;
    foreign import ccall "JVMBridge.h JVMBridge_GetStringUTFLength"                rawGetStringUTFLength            ::
        RawEnv -> RawGlobalRef -> IO RawSize;

    foreign import ccall "JVMBridge.h JVMBridge_DefineClass"                    rawDefineClass                    ::
        RawEnv -> RawStringPtr -> RawGlobalRef -> (Ptr RawByte) -> RawSize -> IO RawLocalRef;
    }
