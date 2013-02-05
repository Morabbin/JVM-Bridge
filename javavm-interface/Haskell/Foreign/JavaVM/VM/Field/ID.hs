{-# OPTIONS -fno-warn-orphans #-}
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

module Foreign.JavaVM.VM.Field.ID where
{
    import Foreign.JavaVM.VM.StringPtr;
    import Foreign.JavaVM.VM.Ref;
    import Foreign.JavaVM.VM.Types;
    import Foreign.JavaVM.Raw;
    import Platform.JavaVM;
    import Data.Nothing;

    type VMStaticFieldID    = RawStaticFieldID;
    type VMFieldID            = RawFieldID;
    
    instance (Monad m) => MonadHasNothing m VMStaticFieldID where
    {
        getNothing = return nothing;
        getIsNothing a = return (isNothing a);
    };
    
    instance HasNothing VMStaticFieldID where
    {
        nothing = (MkRawStaticFieldID nothing);
        isNothing (MkRawStaticFieldID a) = isNothing a;
    };
    
    instance (Monad m) => MonadHasNothing m VMFieldID where
    {
        getNothing = return nothing;
        getIsNothing a = return (isNothing a);
    };
    
    instance HasNothing VMFieldID where
    {
        nothing = (MkRawFieldID nothing);
        isNothing (MkRawFieldID a) = isNothing a;
    };
    
    vmGetStaticFieldID :: VMClassRef -> FieldNameType -> VM VMStaticFieldID;
    vmGetStaticFieldID jClass (MkMemberNameType name vtype) =
        withJString name (\pname ->
            withJString (valueTypeSig vtype) (\psig ->
                withVMRefRawGlobal jClass (\rawClass ->
                    rawGetStaticFieldID ?jvmenv rawClass pname psig)));

    vmGetFieldID :: VMClassRef -> FieldNameType -> VM VMFieldID;
    vmGetFieldID jClass (MkMemberNameType name vtype) =
        withJString name (\pname ->
            withJString (valueTypeSig vtype) (\psig ->
                withVMRefRawGlobal jClass (\rawClass ->
                    rawGetFieldID ?jvmenv rawClass pname psig)));
}
