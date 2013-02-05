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

module Foreign.JavaVM.Typed.Reference
    (
    JVMIsA(..),
    IsJVMReference(..),
    TLRef(MkTLRef),castTLRef,sameRef,
    tlFindClass,
    IsJReferenceFlavour(..)
    ) where
    {
    import Foreign.JavaVM.Typed.ArgumentList;
    import Foreign.JavaVM.Typed.Value;
    import Foreign.JavaVM.Typed.Returnable;
    import Foreign.JavaVM.VM;
    import Platform.JavaVM;
    import Data.Nothing;
    import Data.Witness;
    
    class JVMIsA to from where
    {
        jvmConvert :: from -> VM to;
    };
    
    class (IsJVMValue t) => IsJVMReference t where
        {
        findClassName    :: Type t -> ClassName;    -- name for the purposes of findClass
        tlToRef            :: t -> VM VMRef;
        tlFromRef        :: VMRef -> VM t;
        };

    tlFindClass :: (IsJVMReference t) => Type t -> VM VMRef;
    tlFindClass t = vmFindClass (findClassName t);
    
    newtype TLRef c = MkTLRef VMRef;
    
    castTLRef :: TLRef a -> TLRef b;
    castTLRef (MkTLRef r) = (MkTLRef r);

    instance MonadHasNothing IO (TLRef c) where
        {
        getNothing = do
            {
            nullRef <- getNothing;
            return (MkTLRef nullRef);
            };

        getIsNothing (MkTLRef ref) = getIsNothing ref;
        };
        
    sameRef :: TLRef a -> TLRef b -> VM Bool;
    sameRef (MkTLRef a) (MkTLRef b) = vmIsSameObject a b;
    
    class IsJReferenceFlavour t where
        {
        tlRefValueType        :: Type t -> ValueType;
        tlNameForFindClass    :: Type t -> ClassName;    -- name for the purposes of findClass
        };
    
    instance (IsJReferenceFlavour t) => IsJVMReturnable (TLRef t) where
        {
        getReturnableType t = MkValueType (getValueType t);
        tlFromVMReturnType foo = foo (return . MkTLRef);
        tlMakeCallback types foo = vmMakeCallback types (\vl -> do
            {
            ret <- foo vl;
            tlToRef ret;
            });
        };
    
    instance (IsJReferenceFlavour t) => IsJVMValue (TLRef t) where
        {
        getValueType t = tlRefValueType (tMap t) where
            {
            tMap :: Type (TLRef t) -> Type t;
            tMap Type = Type;
            };

        tlConvertVMType foo = foo (return . MkTLRef,\(MkTLRef vmref) -> return vmref);

        tlNewArray t size = do
            {
            elementClass <- tlFindClass t;
            nullRef <- getNothing;
            vmNewRefArray size elementClass nullRef;
            };

        tlGetArrayElement array i = do
            {
            ref <- vmGetRefArrayElement array i;
            tlFromRef ref;
            };

        tlGetArrayRegion array start len = if len <=0 then return [] else do
            {
            el <- tlGetArrayElement array start;
            rest <- tlGetArrayRegion array (start + 1) (len - 1);
            return (el:rest);
            };

        tlSetArrayElement array i el = do
            {
            elRef <- tlToRef el;
            vmSetRefArrayElement array i elRef;
            };

        tlSetArrayRegion _ _ [] = return ();
        tlSetArrayRegion array start (x:xs) = do
            {
            tlSetArrayElement array start x;
            tlSetArrayRegion array (start+1) xs;
            };
        };
    
    instance (IsJReferenceFlavour t) => IsJVMArgumentList (TLRef t) where
        {
        getListType t = [getValueType t];
        tlToArgList = tlArgumentToSingleValue;
        tlExtractValues = tlExtractSingleValue;
        };
        
    instance (IsJReferenceFlavour t) => IsJVMReference (TLRef t) where
        {
        findClassName t = tlNameForFindClass (tMap t) where
            {
            tMap :: Type (TLRef t) -> Type t;
            tMap Type = Type;
            };
        tlToRef (MkTLRef ref) = return ref;
        tlFromRef ref = return (MkTLRef ref);
        };
    }
