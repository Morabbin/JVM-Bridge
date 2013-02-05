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

module Foreign.JavaVM.Typed.ListArray
    (
    ) where
{
    import Foreign.JavaVM.Typed.Array;
    import Foreign.JavaVM.Typed.Reference;
    import Foreign.JavaVM.Typed.ArgumentList;
    import Foreign.JavaVM.Typed.Value;
    import Foreign.JavaVM.Typed.Returnable;
    import Foreign.JavaVM.VM;
    import Platform.JavaVM;
    import Data.Nothing;
    import Data.Witness;

    instance (IsJVMValue base) => IsJVMReturnable [base] where
        {
        getReturnableType t = MkValueType (getValueType t);

        tlFromVMReturnType foo = foo tlFromRef;

        tlMakeCallback types foo = do
            {
            vmMakeCallback types (\vl -> do
                {
                ret <- foo vl;
                tlToRef ret;
                });
            };
        };
    
    instance (IsJVMValue base) => IsJVMValue [base] where
        {
        getValueType Type = MkArrayType (getValueType (Type::Type base));

        tlConvertVMType foo = foo (tlFromRef,tlToRef);

        tlNewArray t size = do
            {
            elementClass <- tlFindClass t;
            nullRef <- getNothing;
            vmNewRefArray size elementClass nullRef;
            };

        tlGetArrayElement array i = do
            {
            (MkTLRef el :: ArrayRef base) <- tlGetArrayElement array i;
            tlFromRef el;
            };

        tlGetArrayRegion array start len = if len <=0 then return [] else do
            {
            el <- tlGetArrayElement array start;
            rest <- tlGetArrayRegion array (start + 1) (len - 1);
            return (el:rest);
            };

        tlSetArrayElement array i list = do
            {
            el <- tlToRef list;
            tlSetArrayElement array i (MkTLRef el :: ArrayRef base);
            };

        tlSetArrayRegion _ _ [] = return ();
        tlSetArrayRegion array start (x:xs) = do
            {
            tlSetArrayElement array start x;
            tlSetArrayRegion array (start+1) xs;
            };
        };
        
    instance (IsJVMValue base) => IsJVMReference [base] where
        {
        findClassName t = valueTypeSig (getValueType t);
        tlToRef list = do
            {
            let {len = fromIntegral (length list);};
            array <- tlNewArray (Type::Type base) len;
            tlSetArrayRegion array 0 list;
            return array;
            };
        tlFromRef array = do
            {
            len <- vmGetArrayLength array;
            tlGetArrayRegion array 0 len;
            };
        };

    instance (IsJVMValue base) => IsJVMArgumentList [base] where
        {
        getListType t = [getValueType t];
        tlToArgList = tlArgumentToSingleValue;
        tlExtractValues = tlExtractSingleValue;
        };
    
    instance (IsJVMValue a) => JVMIsA (ArrayRef a) [a] where
    {
        jvmConvert list = do
        {
            array <- tlToRef list;
            return (MkTLRef array);
        };
    };
    
    instance (IsJVMValue a) => JVMIsA [a] (ArrayRef a) where
    {
        jvmConvert (MkTLRef ref) = tlFromRef ref;
    };
}
