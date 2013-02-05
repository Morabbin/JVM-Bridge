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

module Foreign.JavaVM.Typed.Array where
    {
    import Foreign.JavaVM.Typed.Reference;
    import Foreign.JavaVM.Typed.Value;
    import Foreign.JavaVM.VM;
    import Platform.JavaVM;
    import Data.Subtype;
    import Data.Witness;

    data ArrayReferenceFlavour a;
    type ArrayRef a = TLRef (ArrayReferenceFlavour a);
    
    instance (IsJVMValue a) => IsJReferenceFlavour (ArrayReferenceFlavour a) where
        {
        tlRefValueType t = MkArrayType (getValueType (tMap t)) where
            {
            tMap :: Type (ArrayReferenceFlavour a) -> Type a;
            tMap Type = Type;
            };

        tlNameForFindClass t = valueTypeSig (tlRefValueType t);
        };

    instance IsA (ArrayRef t) (ArrayRef t) where
        {
        convert    = id;
        };

    instance JVMIsA (ArrayRef t) (ArrayRef t) where
        {
        jvmConvert a    = return (convert a);
        };

    newArray :: (IsJVMValue a) => Type a -> Jint -> VM (ArrayRef a);
    newArray t size = do
        {
        ref <- tlNewArray t size;
        return (MkTLRef ref);
        };
    
    getArrayLength :: (IsJVMValue a) => ArrayRef a -> VM Jint;
    getArrayLength (MkTLRef ref) = vmGetArrayLength ref;
    
    getArrayRegion :: (IsJVMValue a) => ArrayRef a -> Jint -> Jint -> VM [a];
    getArrayRegion (MkTLRef ref) = tlGetArrayRegion ref;
    
    setArrayRegion :: (IsJVMValue a) => ArrayRef a -> Jint -> [a] -> VM ();
    setArrayRegion (MkTLRef ref) = tlSetArrayRegion ref;
    }
