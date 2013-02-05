{-# OPTIONS -fno-warn-orphans #-}
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

module Foreign.JavaVM.VM.Ref where
{
    import Foreign.JavaVM.VM.Types;
    import Foreign.JavaVM.Raw;
    import Foreign hiding (newForeignPtr);
    import Foreign.Concurrent (newForeignPtr);
    import Platform.JavaVM;
    import Data.Nothing;

    type VMRef = VRef (ForeignPtr ());
    
    instance MonadHasNothing IO VMRef where
    {
        getNothing = getNothing >>= (return . MkVRef);    -- null ref, often used in Java
        getIsNothing (MkVRef a) = getIsNothing a;
    };

    makeVMRefFromGlobal :: RawGlobalRef -> VM VMRef;
    makeVMRefFromGlobal (MkRawGlobalRef rgref) = do
    {
        vm <- rawGetJavaVM ?jvmenv;
        gref <- newForeignPtr rgref (rawDeleteGlobalRefGC vm (MkRawGlobalRef rgref));
        return (MkVRef gref);
    };

    -- note that this 'eats' rawLocal
    makeVMRefFromLocal :: RawLocalRef -> VM VMRef;
    makeVMRefFromLocal rawLocal = do
    {
        rawGlobal <- rawConvertToGlobalRef ?jvmenv rawLocal;
        makeVMRefFromGlobal rawGlobal;
    };
    
    withVMRefRawGlobal :: VMRef -> (RawGlobalRef -> IO a) -> IO a;
    withVMRefRawGlobal (MkVRef fp) foo = withForeignPtr fp (\p -> foo (MkRawGlobalRef p));

    type VMClassRef = VMRef;
}
