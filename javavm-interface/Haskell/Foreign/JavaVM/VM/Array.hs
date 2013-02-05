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

module Foreign.JavaVM.VM.Array
(
    vmNewArray,vmNewRefArray,
    vmGetArrayLength,
    vmGetArrayRegion,vmGetRefArrayElement,
    vmSetArrayRegion,vmSetRefArrayElement
)
where
{
    import Foreign.JavaVM.VM.Ref;
    import Foreign.JavaVM.VM.Types;
    import Foreign.JavaVM.Raw;
    import Platform.JavaVM;
    import Foreign;
    import Data.Witness;

    convertNew :: (RawEnv -> RawSize -> IO RawLocalRef) -> (Jint -> VM VMRef);
    convertNew rawF s = do
    {
        loc <- rawF ?jvmenv s;
        makeVMRefFromLocal loc;
    };

    vmNewArray :: PrimitiveType t -> Jint -> VM VMRef;
    vmNewArray Tboolean    = convertNew rawNewBooleanArray;
    vmNewArray Tbyte    = convertNew rawNewByteArray;
    vmNewArray Tchar    = convertNew rawNewCharArray;
    vmNewArray Tshort    = convertNew rawNewShortArray;
    vmNewArray Tint        = convertNew rawNewIntArray;
    vmNewArray Tlong    = convertNew rawNewLongArray;
    vmNewArray Tfloat    = convertNew rawNewFloatArray;
    vmNewArray Tdouble    = convertNew rawNewDoubleArray;

    vmNewRefArray :: Jint -> VMClassRef -> VMRef -> VM VMRef;
    vmNewRefArray len cls obj = do
    {
        loc <-  withVMRefRawGlobal cls (\rawCls -> 
             withVMRefRawGlobal obj (\rawObj -> 
                 rawNewRefArray ?jvmenv len rawCls rawObj
                 ));
        makeVMRefFromLocal loc;
    };

    vmGetArrayLength :: VMRef -> VM Jint;
    vmGetArrayLength array = withVMRefRawGlobal array (\rawArray -> rawGetArrayLength ?jvmenv rawArray);

    convertGetRegion :: (Storable a) =>
     (RawEnv -> RawGlobalRef -> RawSize -> RawSize -> IO (Ptr a)) -> (VMRef -> Jint -> Jint -> VM [a]);
    convertGetRegion rawF array start len = do
    {
        ptr <- withVMRefRawGlobal array (\rawArray -> rawF ?jvmenv rawArray start len);
        peekArray (fromIntegral len) ptr;
    };
    
    vmGetArrayRegion    :: (Is PrimitiveType t) =>
     VMRef -> Jint -> Jint -> VM [t];
    vmGetArrayRegion = vmGetArrayRegion' representative where
    {
        vmGetArrayRegion' :: PrimitiveType t -> VMRef -> Jint -> Jint -> VM [t];    
        vmGetArrayRegion' Tboolean = \array start len -> do
        {
            region <- convertGetRegion rawGetBooleanArrayRegion array start len;
            return (fmap fromRawBoolean region);
        };
        vmGetArrayRegion' Tbyte        = convertGetRegion rawGetByteArrayRegion;
        vmGetArrayRegion' Tchar        = convertGetRegion rawGetCharArrayRegion;
        vmGetArrayRegion' Tshort    = convertGetRegion rawGetShortArrayRegion;
        vmGetArrayRegion' Tint        = convertGetRegion rawGetIntArrayRegion;
        vmGetArrayRegion' Tlong        = convertGetRegion rawGetLongArrayRegion;
        vmGetArrayRegion' Tfloat    = convertGetRegion rawGetFloatArrayRegion;
        vmGetArrayRegion' Tdouble    = convertGetRegion rawGetDoubleArrayRegion;
    };

    vmGetRefArrayElement :: VMRef -> Jint -> VM VMRef;
    vmGetRefArrayElement array i = do    
    {
        el <- withVMRefRawGlobal array (\rawArray -> rawGetRefArrayElement ?jvmenv rawArray i);
        makeVMRefFromLocal el;
    };

    convertSetRegion :: (Storable a) =>
     (RawEnv -> RawGlobalRef -> RawSize -> RawSize -> Ptr a -> IO ()) -> (VMRef -> Jint -> [a] -> VM ());
    convertSetRegion rawF array start region = withArray region (\regionPtr ->
        withVMRefRawGlobal array (\rawArray ->
            rawF ?jvmenv rawArray start (fromIntegral (length region)) regionPtr
            ));

    vmSetArrayRegion    :: (Is PrimitiveType t) =>
     VMRef -> Jint -> [t] -> VM ();
    vmSetArrayRegion = vmSetArrayRegion' representative where
    {
        vmSetArrayRegion' :: PrimitiveType t -> VMRef -> Jint -> [t] -> VM ();
        vmSetArrayRegion' Tboolean    = \array start list ->
            convertSetRegion rawSetBooleanArrayRegion array start (fmap toRawBoolean list);
        vmSetArrayRegion' Tbyte        = convertSetRegion rawSetByteArrayRegion;
        vmSetArrayRegion' Tchar        = convertSetRegion rawSetCharArrayRegion;
        vmSetArrayRegion' Tshort    = convertSetRegion rawSetShortArrayRegion;
        vmSetArrayRegion' Tint        = convertSetRegion rawSetIntArrayRegion;
        vmSetArrayRegion' Tlong        = convertSetRegion rawSetLongArrayRegion;
        vmSetArrayRegion' Tfloat    = convertSetRegion rawSetFloatArrayRegion;
        vmSetArrayRegion' Tdouble    = convertSetRegion rawSetDoubleArrayRegion;
    };

    vmSetRefArrayElement :: VMRef -> Jint -> VMRef -> VM ();
    vmSetRefArrayElement array i el = withVMRefRawGlobal array (\rawArray ->
        withVMRefRawGlobal el (\rawElem ->
            rawSetRefArrayElement ?jvmenv rawArray i rawElem
            ));
}
