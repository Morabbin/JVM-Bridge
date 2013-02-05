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

module Foreign.JavaVM.VM.Callback
(
    OpaqueAddress,
    executeFunctionClassName,
    vmStartExecuteFunction,
    vmMakeCallback,
    vmDefineClass,vmLoadClass,vmDefineCallbackClass
) where
{
    import Foreign.JavaVM.VM.Callback.Constructor;
    import Foreign.JavaVM.VM.Callback.Finalizer;
    import Foreign.JavaVM.VM.Callback.Field;
    import Foreign.JavaVM.VM.Callback.Method;
    import Foreign.JavaVM.VM.Witness;
    import Foreign.JavaVM.VM.Ref;
    import Foreign.JavaVM.VM.StringPtr;
    import Foreign.JavaVM.VM.ValueList;
    import Foreign.JavaVM.VM.Types;
    import Foreign.JavaVM.Raw;
    import Foreign.JavaVM.Configure;
    import Platform.JavaVM;
    import Data.Witness;
    import Foreign;
    
    makeCallbackClass :: Bool -> ClassName -> ClassName -> [ClassName] -> [ValueType] -> [MethodNameType] -> ClassDefinition;
    makeCallbackClass deferred classname superclassname interfaces superclassconstructor methods = MkClassDefinition
        classname True False False False
        superclassname interfaces
        (fmap fieldDefinitionForMethod methods)
        (
            [constructorMethod classname superclassname superclassconstructor methods,
            finalizerMethod deferred classname superclassname methods] ++
            (fmap (makeMethod deferred classname) methods))
        [];
    
--    makeNowCallbackClass :: ClassName -> ClassName -> [ClassName] -> [ValueType] -> [MethodNameType] -> ClassDefinition;
--    makeNowCallbackClass = makeCallbackClass False;

    makeDeferredCallbackClass :: ClassName -> ClassName -> [ClassName] -> [ValueType] -> [MethodNameType] -> ClassDefinition;
    makeDeferredCallbackClass = makeCallbackClass True;

    vmStartExecuteFunction    :: VM ();
    vmStartExecuteFunction = rawStartExecuteFunction ?jvmenv;
 
    convertCallback :: ((RawEnv -> RawValueList -> IO a) -> IO (FunPtr (RawEnv -> RawValueList -> IO a))) ->
        [AnyWitness VMType] -> ([Any VMType] -> VM a) -> IO OpaqueAddress;
    convertCallback rawF argtypes foo = do
    {
        ptr <- rawF (\env vl -> let {?jvmenv=env;} in do
        {
            args <- getValueList argtypes vl;
            foo args;
        });
        rawConvertCallback ptr;
    };

    vmMakeCallback        :: (Is VMReturnType t) =>
     [AnyWitness VMType] -> ([Any VMType] -> VM t) -> IO OpaqueAddress;
    vmMakeCallback = vmMakeCallback' representative where
    {
        vmMakeCallback'        :: VMReturnType t ->
         [AnyWitness VMType] -> ([Any VMType] -> VM t) -> IO OpaqueAddress;
        vmMakeCallback' Tvoid                            = convertCallback rawMakeVoidCallback;
        vmMakeCallback' (Tvalue (Tprimitive Tboolean))    = \argtypes foo    -> convertCallback rawMakeBooleanCallback argtypes (\args -> do
        {
            b <- foo args;
            return (toRawBoolean b);
        });
        vmMakeCallback' (Tvalue (Tprimitive Tbyte))        = convertCallback rawMakeByteCallback;    
        vmMakeCallback' (Tvalue (Tprimitive Tchar))        = convertCallback rawMakeCharCallback;    
        vmMakeCallback' (Tvalue (Tprimitive Tshort))    = convertCallback rawMakeShortCallback;    
        vmMakeCallback' (Tvalue (Tprimitive Tint))        = convertCallback rawMakeIntCallback;    
        vmMakeCallback' (Tvalue (Tprimitive Tlong))        = convertCallback rawMakeLongCallback;    
        vmMakeCallback' (Tvalue (Tprimitive Tfloat))    = convertCallback rawMakeFloatCallback;    
        vmMakeCallback' (Tvalue (Tprimitive Tdouble))    = convertCallback rawMakeDoubleCallback;    
        vmMakeCallback' (Tvalue Tref)                    = \argtypes foo -> convertCallback rawMakeObjectCallback argtypes (\args -> do
        {
            ref <- foo args;
            withVMRefRawGlobal ref return;
        });
    };

    vmDefineClass :: ClassName -> VMRef -> [Word8] -> VM VMRef;
    vmDefineClass name loader cf = do
    {
        loc <- withJString name (\pname ->
            withVMRefRawGlobal loader (\rloader ->
                withArray cf (\pcf ->
                    rawDefineClass ?jvmenv pname rloader ((castPtr :: Ptr Word8 -> Ptr RawByte) pcf) (fromIntegral (length cf))
            )));
        makeVMRefFromLocal loc;
    };

    vmLoadClass :: VMRef -> ClassDefinition -> VM VMRef;
    vmLoadClass loader cls = vmDefineClass (classDefName cls) loader (jcfWrite cls);

    vmDefineCallbackClass :: VMRef -> ClassName -> ClassName -> [ClassName] -> [ValueType] -> [MethodNameType] -> VM VMRef;
    vmDefineCallbackClass loader classname superclassname interfaces superclassconstructor methods = 
        vmDefineClass classname loader (jcfWrite (makeDeferredCallbackClass classname superclassname interfaces superclassconstructor methods));
}
