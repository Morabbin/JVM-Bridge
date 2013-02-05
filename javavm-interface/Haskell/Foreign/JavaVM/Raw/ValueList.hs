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

module Foreign.JavaVM.Raw.ValueList where
    {
    import Foreign.JavaVM.Raw.Types;
    import Foreign;

    newtype RawValueList = MkRawValueList (Ptr ());

    foreign import ccall "ValueList.h JVMBridge_CreateValueList"        rawCreateValueList            ::
        IO RawValueList;
    foreign import ccall "ValueList.h JVMBridge_DestroyValueList"        rawDestroyValueList            ::
        RawValueList -> IO ();

{--    foreign import ccall "ValueList.h JVMBridge_ValueListSize"            rawValueListSize            ::
        RawValueList -> IO Word16;
    foreign import ccall "ValueList.h JVMBridge_GetNthValueType"        rawGetNthValueType            ::
        RawValueList -> Word16 -> IO CChar;
--}
    foreign import ccall "ValueList.h JVMBridge_GetNthValueAsBoolean"    rawGetNthValueAsBoolean        ::
        RawValueList -> Word16 -> IO RawBoolean;
    foreign import ccall "ValueList.h JVMBridge_GetNthValueAsByte"        rawGetNthValueAsByte        ::
        RawValueList -> Word16 -> IO RawByte;
    foreign import ccall "ValueList.h JVMBridge_GetNthValueAsChar"        rawGetNthValueAsChar        ::
        RawValueList -> Word16 -> IO RawChar;
    foreign import ccall "ValueList.h JVMBridge_GetNthValueAsShort"        rawGetNthValueAsShort        ::
        RawValueList -> Word16 -> IO RawShort;
    foreign import ccall "ValueList.h JVMBridge_GetNthValueAsInt"        rawGetNthValueAsInt            ::
        RawValueList -> Word16 -> IO RawInt;
    foreign import ccall "ValueList.h JVMBridge_GetNthValueAsLong"        rawGetNthValueAsLong        ::
        RawValueList -> Word16 -> IO RawLong;
    foreign import ccall "ValueList.h JVMBridge_GetNthValueAsFloat"        rawGetNthValueAsFloat        ::
        RawValueList -> Word16 -> IO RawFloat;
    foreign import ccall "ValueList.h JVMBridge_GetNthValueAsDouble"    rawGetNthValueAsDouble        ::
        RawValueList -> Word16 -> IO RawDouble;
    foreign import ccall "ValueList.h JVMBridge_GetNthValueAsObject"    rawGetNthValueAsRef            ::
        RawValueList -> Word16 -> IO RawGlobalRef;    -- global, needs to be deleted eventually

    foreign import ccall "ValueList.h JVMBridge_AddBooleanToValueList"    rawAddBooleanToValueList    ::
        RawValueList -> RawBoolean -> IO ();
    foreign import ccall "ValueList.h JVMBridge_AddByteToValueList"        rawAddByteToValueList        ::
        RawValueList -> RawByte -> IO ();
    foreign import ccall "ValueList.h JVMBridge_AddCharToValueList"        rawAddCharToValueList        ::
        RawValueList -> RawChar -> IO ();
    foreign import ccall "ValueList.h JVMBridge_AddShortToValueList"    rawAddShortToValueList        ::
        RawValueList -> RawShort -> IO ();
    foreign import ccall "ValueList.h JVMBridge_AddIntToValueList"        rawAddIntToValueList        ::
        RawValueList -> RawInt -> IO ();
    foreign import ccall "ValueList.h JVMBridge_AddLongToValueList"        rawAddLongToValueList        ::
        RawValueList -> RawLong -> IO ();
    foreign import ccall "ValueList.h JVMBridge_AddFloatToValueList"    rawAddFloatToValueList        ::
        RawValueList -> RawFloat -> IO ();
    foreign import ccall "ValueList.h JVMBridge_AddDoubleToValueList"    rawAddDoubleToValueList        ::
        RawValueList -> RawDouble -> IO ();
    foreign import ccall "ValueList.h JVMBridge_AddObjectToValueList"    rawAddRefToValueList        ::
        RawValueList -> RawGlobalRef -> IO ();
    }
