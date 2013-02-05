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

module Foreign.JavaVM.Typed.Field
(
    getStaticField,getField
) where
{
    import Foreign.JavaVM.Typed.Class;
    import Foreign.JavaVM.Typed.Object;
    import Foreign.JavaVM.Typed.Reference;
    import Foreign.JavaVM.Typed.Value;
    import Foreign.JavaVM.VM;
    import Platform.JavaVM;
    import Control.Monad.Reference;
    import Data.Witness;

    getStaticFieldByType :: (IsJVMValue v) =>
        Type v -> JClass -> MemberName -> VM (Ref IO v);
    getStaticFieldByType vType (MkTLRef jClass) name = tlConvertVMType (\(from,to) -> do
    {
        field <- vmGetStaticField jClass (MkMemberNameType name (getValueType vType));
        return (MkRef
        {
            getRef = do
            {
                vmval <- getRef field;
                from vmval;
            },
            setRef = \v -> do
            {
                vmval <- to v;
                setRef field vmval;
            }
        });
    });

    getFieldByType :: (IsJavaClassMarker c,IsJVMValue v) =>
        Type v -> JClass -> MemberName -> VM (ObjectRef c -> Ref IO v);
    getFieldByType vType (MkTLRef jClass) name = tlConvertVMType (\(from,to) -> do
    {
        field <- vmGetField jClass (MkMemberNameType name (getValueType vType));
        return (\(MkTLRef vmobj) -> MkRef
        {
            getRef = do
            {
                vmval <- getRef (field vmobj);
                from vmval;
            },
            setRef = \v -> do
            {
                vmval <- to v;
                setRef (field vmobj) vmval;
            }
        });
    });

    getStaticField :: (IsJVMValue v) =>
        JClass -> MemberName -> VM (Ref IO v);
    getStaticField = getStaticFieldByType Type;

    getField :: (IsJavaClassMarker c,IsJVMValue v) =>
        JClass -> MemberName -> VM (ObjectRef c -> Ref IO v);
    getField = getFieldByType Type;
}
