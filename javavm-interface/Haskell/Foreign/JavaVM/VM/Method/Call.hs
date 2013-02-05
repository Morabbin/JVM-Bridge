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

module Foreign.JavaVM.VM.Method.Call(vmCallStaticMethod,vmCallMethod) where
{
    import Foreign.JavaVM.VM.Throwable;
    import Foreign.JavaVM.VM.ValueList;
    import Foreign.JavaVM.VM.Witness;
    import Foreign.JavaVM.VM.Ref;
    import Foreign.JavaVM.VM.Types;
    import Foreign.JavaVM.Raw;
    import Platform.JavaVM;
    import Data.Witness;

    convertOne :: 
     (RawEnv -> RawGlobalRef -> mid -> RawValueList -> IO a) ->
        (mid -> VMRef -> [Any VMType] -> VM a);
    convertOne foo method ref args = do
    {
        vl <- makeValueList args;
        checkCall (withVMRefRawGlobal ref (\rawRef -> foo ?jvmenv rawRef method vl));
    };

    vmCallStaticMethod :: (Is VMReturnType t) =>
     RawStaticMethodID -> VMClassRef -> [Any VMType] -> VM t;
    vmCallStaticMethod = vmCallStaticMethod' representative where
    {
        vmCallStaticMethod' ::
         VMReturnType t -> RawStaticMethodID -> VMClassRef -> [Any VMType] -> VM t;
        vmCallStaticMethod' Tvoid                            = convertOne rawCallStaticVoidMethod;
        vmCallStaticMethod' (Tvalue (Tprimitive Tboolean))    = \method ref args -> do
        {
            rawB <- convertOne rawCallStaticBooleanMethod method ref args;
            return (fromRawBoolean rawB);
        };
        vmCallStaticMethod' (Tvalue (Tprimitive Tbyte))        = convertOne rawCallStaticByteMethod;
        vmCallStaticMethod' (Tvalue (Tprimitive Tchar))        = convertOne rawCallStaticCharMethod;
        vmCallStaticMethod' (Tvalue (Tprimitive Tshort))    = convertOne rawCallStaticShortMethod;
        vmCallStaticMethod' (Tvalue (Tprimitive Tint))        = convertOne rawCallStaticIntMethod;
        vmCallStaticMethod' (Tvalue (Tprimitive Tlong))        = convertOne rawCallStaticLongMethod;
        vmCallStaticMethod' (Tvalue (Tprimitive Tfloat))    = convertOne rawCallStaticFloatMethod;
        vmCallStaticMethod' (Tvalue (Tprimitive Tdouble))    = convertOne rawCallStaticDoubleMethod;
        vmCallStaticMethod' (Tvalue Tref)                    = \method ref args -> do
        {
            locRef <- convertOne rawCallStaticRefMethod method ref args;
            makeVMRefFromLocal locRef;
        };
    };

    vmCallObjectMethod :: (Is VMReturnType t) =>
     RawMethodID -> VMRef -> [Any VMType] -> VM t;
    vmCallObjectMethod = vmCallMethod' representative where
    {
        vmCallMethod' ::
         VMReturnType t -> RawMethodID -> VMRef -> [Any VMType] -> VM t;
        vmCallMethod' Tvoid                            = convertOne rawCallVoidMethod;
        vmCallMethod' (Tvalue (Tprimitive Tboolean))    = \method ref args -> do
        {
            rawB <- convertOne rawCallBooleanMethod method ref args;
            return (fromRawBoolean rawB);
        };
        vmCallMethod' (Tvalue (Tprimitive Tbyte))    = convertOne rawCallByteMethod;
        vmCallMethod' (Tvalue (Tprimitive Tchar))    = convertOne rawCallCharMethod;
        vmCallMethod' (Tvalue (Tprimitive Tshort))    = convertOne rawCallShortMethod;
        vmCallMethod' (Tvalue (Tprimitive Tint))    = convertOne rawCallIntMethod;
        vmCallMethod' (Tvalue (Tprimitive Tlong))    = convertOne rawCallLongMethod;
        vmCallMethod' (Tvalue (Tprimitive Tfloat))    = convertOne rawCallFloatMethod;
        vmCallMethod' (Tvalue (Tprimitive Tdouble))    = convertOne rawCallDoubleMethod;
        vmCallMethod' (Tvalue Tref)                    = \method ref args -> do
        {
            locRef <- convertOne rawCallRefMethod method ref args;
            makeVMRefFromLocal locRef;
        };
    };

    convertTwo :: 
     (RawEnv -> RawGlobalRef -> RawGlobalRef -> RawMethodID -> RawValueList -> IO a) ->
        (RawMethodID -> VMClassRef -> VMRef -> [Any VMType] -> VM a);
    convertTwo foo method cls obj args = do
    {
        vl <- makeValueList args;
        checkCall (withVMRefRawGlobal obj (\rawObj -> 
            withVMRefRawGlobal cls (\rawCls -> foo ?jvmenv rawObj rawCls method vl)));
    };

    vmCallNonvirtualMethod :: (Is VMReturnType t) =>
     RawMethodID -> VMClassRef -> VMRef -> [Any VMType] -> VM t;
    vmCallNonvirtualMethod = vmCallNonvirtualMethod' representative where
    {
        vmCallNonvirtualMethod' :: 
         VMReturnType t -> RawMethodID -> VMClassRef -> VMRef -> [Any VMType] -> VM t;
        vmCallNonvirtualMethod' Tvoid                            = convertTwo rawCallNonvirtualVoidMethod;
        vmCallNonvirtualMethod' (Tvalue (Tprimitive Tboolean))    = \method cls obj args -> do
        {
            rawB <- convertTwo rawCallNonvirtualBooleanMethod method cls obj args;
            return (fromRawBoolean rawB);
        };
        vmCallNonvirtualMethod' (Tvalue (Tprimitive Tbyte))        = convertTwo rawCallNonvirtualByteMethod;
        vmCallNonvirtualMethod' (Tvalue (Tprimitive Tchar))        = convertTwo rawCallNonvirtualCharMethod;
        vmCallNonvirtualMethod' (Tvalue (Tprimitive Tshort))    = convertTwo rawCallNonvirtualShortMethod;
        vmCallNonvirtualMethod' (Tvalue (Tprimitive Tint))        = convertTwo rawCallNonvirtualIntMethod;
        vmCallNonvirtualMethod' (Tvalue (Tprimitive Tlong))        = convertTwo rawCallNonvirtualLongMethod;
        vmCallNonvirtualMethod' (Tvalue (Tprimitive Tfloat))    = convertTwo rawCallNonvirtualFloatMethod;
        vmCallNonvirtualMethod' (Tvalue (Tprimitive Tdouble))    = convertTwo rawCallNonvirtualDoubleMethod;
        vmCallNonvirtualMethod' (Tvalue Tref)                    = \method cls obj args -> do
        {
            locRef <- convertTwo rawCallNonvirtualRefMethod method cls obj args;
            makeVMRefFromLocal locRef;
        };
    };

    vmCallMethod :: (Is VMReturnType t) =>
     RawMethodID -> Maybe VMClassRef -> VMRef -> [Any VMType] -> VM t;
    vmCallMethod mid Nothing = vmCallObjectMethod mid;
    vmCallMethod mid (Just cls) = vmCallNonvirtualMethod mid cls;    
}
