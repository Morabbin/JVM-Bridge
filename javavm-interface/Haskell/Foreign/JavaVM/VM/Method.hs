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

module Foreign.JavaVM.VM.Method(vmGetStaticMethod,vmGetMethod) where
{
    import Foreign.JavaVM.VM.Method.Call;
    import Foreign.JavaVM.VM.StringPtr;
    import Foreign.JavaVM.VM.Witness;
    import Foreign.JavaVM.VM.Ref;
    import Foreign.JavaVM.VM.Types;
    import Foreign.JavaVM.Raw;
    import Platform.JavaVM;
    import Data.Nothing;
    import Data.Witness;
    
    instance (Monad m) => MonadHasNothing m RawStaticMethodID where
    {
        getNothing = return nothing;
        getIsNothing a = return (isNothing a);
    };
    
    instance HasNothing RawStaticMethodID where
    {
        nothing = (MkRawStaticMethodID nothing);
        isNothing (MkRawStaticMethodID a) = isNothing a;
    };
    
    instance (Monad m) => MonadHasNothing m RawMethodID where
    {
        getNothing = return nothing;
        getIsNothing a = return (isNothing a);
    };
    
    instance HasNothing RawMethodID where
    {
        nothing = (MkRawMethodID nothing);
        isNothing (MkRawMethodID a) = isNothing a;
    };
    
    vmGetStaticMethodID :: VMClassRef -> MethodNameType -> VM RawStaticMethodID;
    vmGetStaticMethodID jClass (MkMemberNameType name ftype) =
        withJString name (\pname ->
            withJString (functionTypeSig ftype) (\psig ->
                withVMRefRawGlobal jClass (\rawClass ->
                    rawGetStaticMethodID ?jvmenv rawClass pname psig)));

    vmGetMethodID :: VMClassRef -> MethodNameType -> VM RawMethodID;
    vmGetMethodID jClass (MkMemberNameType name ftype) =
        withJString name (\pname ->
            withJString (functionTypeSig ftype) (\psig ->
                withVMRefRawGlobal jClass (\rawClass ->
                    rawGetMethodID ?jvmenv rawClass pname psig)));
    
    vmGetStaticMethod :: (Is VMReturnType t) => VMClassRef -> MethodNameType -> VM (VMClassRef -> [Any VMType] -> IO t);
    vmGetStaticMethod jClass nt = do
    {
        mid <- vmGetStaticMethodID jClass nt;
        if (isNothing mid)
         then fail ("static method "++(show nt)++" not found")
         else return (vmCallStaticMethod mid);
    };
    
    vmGetMethod :: (Is VMReturnType t) => VMClassRef -> MethodNameType -> VM (Maybe VMClassRef -> VMRef -> [Any VMType] -> IO t);
    vmGetMethod jClass nt = do
    {
        mid <- vmGetMethodID jClass nt;
        if (isNothing mid)
         then fail ("method "++(show nt)++" not found")
         else return (vmCallMethod mid);
    };
}
