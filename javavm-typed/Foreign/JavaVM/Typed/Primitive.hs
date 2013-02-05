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

module Foreign.JavaVM.Typed.Primitive() where
    {
    import Foreign.JavaVM.Typed.Returnable;
    import Foreign.JavaVM.Typed.ArgumentList;
    import Foreign.JavaVM.Typed.Value;
    import Foreign.JavaVM.VM;
    import Platform.JavaVM;
    import Data.Witness;

    instance IsJVMReturnable Jboolean where
        {
        getReturnableType t = MkValueType (getValueType t);
        tlFromVMReturnType foo = foo return;
        tlMakeCallback = vmMakeCallback;
        };
    
    instance IsJVMReturnable Jbyte where
        {
        getReturnableType t = MkValueType (getValueType t);
        tlFromVMReturnType foo = foo return;
        tlMakeCallback = vmMakeCallback;
        };
    
    instance IsJVMReturnable Jchar where
        {
        getReturnableType t = MkValueType (getValueType t);
        tlFromVMReturnType foo = foo return;
        tlMakeCallback = vmMakeCallback;
        };
    
    instance IsJVMReturnable Jshort where
        {
        getReturnableType t = MkValueType (getValueType t);
        tlFromVMReturnType foo = foo return;
        tlMakeCallback = vmMakeCallback;
        };
    
    instance IsJVMReturnable Jint where
        {
        getReturnableType t = MkValueType (getValueType t);
        tlFromVMReturnType foo = foo return;
        tlMakeCallback = vmMakeCallback;
        };
    
    instance IsJVMReturnable Jlong where
        {
        getReturnableType t = MkValueType (getValueType t);
        tlFromVMReturnType foo = foo return;
        tlMakeCallback = vmMakeCallback;
        };
    
    instance IsJVMReturnable Jfloat where
        {
        getReturnableType t = MkValueType (getValueType t);
        tlFromVMReturnType foo = foo return;
        tlMakeCallback = vmMakeCallback;
        };
    
    instance IsJVMReturnable Jdouble where
        {
        getReturnableType t = MkValueType (getValueType t);
        tlFromVMReturnType foo = foo return;
        tlMakeCallback = vmMakeCallback;
        };
    
    getElementFromRegion :: (VMRef -> Jint -> Jint -> VM [t])
        -> VMRef -> Jint -> VM t;
    getElementFromRegion getr array i = do
        {
        region <- getr array i 1;
        case region of
            {
            (e:_)    -> return e;
            []    -> fail "";
            };
        };
    
    setElementToRegion :: (VMRef -> Jint -> [t] -> VM ())
        -> VMRef -> Jint -> t -> VM ();
    setElementToRegion setr array i el = setr array i [el];
    
    instance IsJVMValue Jboolean where
        {
        getValueType Type = MkPrimitiveType (MkAnyWitness Tboolean);
        tlConvertVMType foo = foo (return,return);

        tlGetArrayElement = getElementFromRegion tlGetArrayRegion;
        tlSetArrayElement = setElementToRegion tlSetArrayRegion;

        tlNewArray t size = vmNewArray (rerepresentative t) size;
        tlGetArrayRegion array start len = vmGetArrayRegion array start len;
        tlSetArrayRegion array i list = vmSetArrayRegion array i list;
        };
    
    instance IsJVMValue Jbyte where
        {
        getValueType Type = MkPrimitiveType (MkAnyWitness Tbyte);
        tlConvertVMType foo = foo (return,return);

        tlGetArrayElement = getElementFromRegion tlGetArrayRegion;
        tlSetArrayElement = setElementToRegion tlSetArrayRegion;

        tlNewArray t size = vmNewArray (rerepresentative t) size;
        tlGetArrayRegion array start len = vmGetArrayRegion array start len;
        tlSetArrayRegion array i list = vmSetArrayRegion array i list;
        };
    
    instance IsJVMValue Jchar where
        {
        getValueType Type = MkPrimitiveType (MkAnyWitness Tchar);
        tlConvertVMType foo = foo (return,return);

        tlGetArrayElement = getElementFromRegion tlGetArrayRegion;
        tlSetArrayElement = setElementToRegion tlSetArrayRegion;

        tlNewArray t size = vmNewArray (rerepresentative t) size;
        tlGetArrayRegion array start len = vmGetArrayRegion array start len;
        tlSetArrayRegion array i list = vmSetArrayRegion array i list;
        };
    
    instance IsJVMValue Jshort where
        {
        getValueType Type = MkPrimitiveType (MkAnyWitness Tshort);
        tlConvertVMType foo = foo (return,return);

        tlGetArrayElement = getElementFromRegion tlGetArrayRegion;
        tlSetArrayElement = setElementToRegion tlSetArrayRegion;

        tlNewArray t size = vmNewArray (rerepresentative t) size;
        tlGetArrayRegion array start len = vmGetArrayRegion array start len;
        tlSetArrayRegion array i list = vmSetArrayRegion array i list;
        };
    
    instance IsJVMValue Jint where
        {
        getValueType Type = MkPrimitiveType (MkAnyWitness Tint);
        tlConvertVMType foo = foo (return,return);

        tlGetArrayElement = getElementFromRegion tlGetArrayRegion;
        tlSetArrayElement = setElementToRegion tlSetArrayRegion;

        tlNewArray t size = vmNewArray (rerepresentative t) size;
        tlGetArrayRegion array start len = vmGetArrayRegion array start len;
        tlSetArrayRegion array i list = vmSetArrayRegion array i list;
        };
    
    instance IsJVMValue Jlong where
        {
        getValueType Type = MkPrimitiveType (MkAnyWitness Tlong);
        tlConvertVMType foo = foo (return,return);

        tlGetArrayElement = getElementFromRegion tlGetArrayRegion;
        tlSetArrayElement = setElementToRegion tlSetArrayRegion;

        tlNewArray t size = vmNewArray (rerepresentative t) size;
        tlGetArrayRegion array start len = vmGetArrayRegion array start len;
        tlSetArrayRegion array i list = vmSetArrayRegion array i list;
        };
    
    instance IsJVMValue Jfloat where
        {
        getValueType Type = MkPrimitiveType (MkAnyWitness Tfloat);
        tlConvertVMType foo = foo (return,return);

        tlGetArrayElement = getElementFromRegion tlGetArrayRegion;
        tlSetArrayElement = setElementToRegion tlSetArrayRegion;

        tlNewArray t size = vmNewArray (rerepresentative t) size;
        tlGetArrayRegion array start len = vmGetArrayRegion array start len;
        tlSetArrayRegion array i list = vmSetArrayRegion array i list;
        };
    
    instance IsJVMValue Jdouble where
        {
        getValueType Type = MkPrimitiveType (MkAnyWitness Tdouble);
        tlConvertVMType foo = foo (return,return);

        tlGetArrayElement = getElementFromRegion tlGetArrayRegion;
        tlSetArrayElement = setElementToRegion tlSetArrayRegion;

        tlNewArray t size = vmNewArray (rerepresentative t) size;
        tlGetArrayRegion array start len = vmGetArrayRegion array start len;
        tlSetArrayRegion array i list = vmSetArrayRegion array i list;
        };

{-- GHC isn't smart enough to deal with this without complaining of overlapping instances.
    instance (IsJVMValue t) => IsJVMArgumentList t where
        {
        getListType t = [getValueType t];
        tlAppendListToValueList = tlAppendArgumentToValueList;
        };
--}    

    instance IsJVMArgumentList Jboolean where
        {
        getListType t = [getValueType t];
        tlToArgList = tlArgumentToSingleValue;
        tlExtractValues = tlExtractSingleValue;
        };
    
    instance IsJVMArgumentList Jbyte where
        {
        getListType t = [getValueType t];
        tlToArgList = tlArgumentToSingleValue;
        tlExtractValues = tlExtractSingleValue;
        };
    
    instance IsJVMArgumentList Jchar where
        {
        getListType t = [getValueType t];
        tlToArgList = tlArgumentToSingleValue;
        tlExtractValues = tlExtractSingleValue;
        };
    
    instance IsJVMArgumentList Jshort where
        {
        getListType t = [getValueType t];
        tlToArgList = tlArgumentToSingleValue;
        tlExtractValues = tlExtractSingleValue;
        };
    
    instance IsJVMArgumentList Jint where
        {
        getListType t = [getValueType t];
        tlToArgList = tlArgumentToSingleValue;
        tlExtractValues = tlExtractSingleValue;
        };
    
    instance IsJVMArgumentList Jlong where
        {
        getListType t = [getValueType t];
        tlToArgList = tlArgumentToSingleValue;
        tlExtractValues = tlExtractSingleValue;
        };
    
    instance IsJVMArgumentList Jfloat where
        {
        getListType t = [getValueType t];
        tlToArgList = tlArgumentToSingleValue;
        tlExtractValues = tlExtractSingleValue;
        };
    
    instance IsJVMArgumentList Jdouble where
        {
        getListType t = [getValueType t];
        tlToArgList = tlArgumentToSingleValue;
        tlExtractValues = tlExtractSingleValue;
        };
    }
