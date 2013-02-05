{-
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
-}

module Foreign.JavaVM.Typed.Value where
{
    import Foreign.JavaVM.Typed.Returnable;
    import Foreign.JavaVM.VM;
    import Platform.JavaVM;
    import Data.Witness;
    
    class (IsJVMReturnable t) => IsJVMValue t where
    {
        getValueType                :: Type t -> ValueType;
        tlConvertVMType :: (forall vmt. (Is VMType vmt) => (vmt -> VM t,t -> VM vmt) -> r) -> r;

        tlNewArray                    :: Type t -> Jint -> VM VMRef;
        tlGetArrayElement            :: VMRef -> Jint -> VM t;
        tlGetArrayRegion            :: VMRef -> Jint -> Jint -> VM [t];
        tlSetArrayElement            :: VMRef -> Jint -> t -> VM ();
        tlSetArrayRegion            :: VMRef -> Jint -> [t] -> VM ();
    };


    tlGetVMType :: (IsJVMValue t) => Type t -> (forall vmt. VMType vmt -> r) -> r;
    tlGetVMType tt foo = tlConvertVMType (\(x,_) -> foo (f tt x)) where
    {
        f :: (Is VMType vmt) => Type t -> (vmt -> VM t) -> VMType vmt;
        f _ _ = representative;
    };

    tlToVMType :: (IsJVMValue t) => t -> (forall vmt. (Is VMType vmt) => VM vmt -> r) -> r;
    tlToVMType t foo = tlConvertVMType (\(_,to) -> foo (to t));
    
    tlFromVMType :: (IsJVMValue t) => (forall vmt. (Is VMType vmt) => (vmt -> VM t) -> r) -> r;
    tlFromVMType foo = tlConvertVMType (\(from,_) -> foo from);

    tlExtractValue :: (IsJVMValue t) => Any VMType -> Maybe (VM t);    -- if the JVMValue happens to correspond to the VMType
    tlExtractValue = tlFromVMType foo where
    {
        foo :: forall vmt t. (IsJVMValue t,Is VMType vmt) => (vmt -> VM t) -> Any VMType -> Maybe (VM t);
        foo f (MkAny vt val) = do
        {
            MkEqualType <- matchWitness vt (representative :: VMType vmt);
            return (f val);
        };
    };

    tlArgumentToValue    :: (IsJVMValue t) => t -> VM (Any VMType);
    tlArgumentToValue t = tlToVMType t foo where
    {
        foo :: forall vmt. (Is VMType vmt) => VM vmt -> VM (Any VMType);
        foo mvmt = fmap mkAny mvmt;
    };

    tlArgumentToSingleValue    :: (IsJVMValue t) => t -> VM [Any VMType];
    tlArgumentToSingleValue t = do
    {
        arg <- tlArgumentToValue t;
        return [arg];
    };

    tlExtractSingleValue :: (IsJVMValue t) => [Any VMType] -> Maybe (VM t);
    tlExtractSingleValue [arg] = tlExtractValue arg;
    tlExtractSingleValue _ = Nothing;
}
