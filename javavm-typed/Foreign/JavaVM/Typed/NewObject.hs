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

module Foreign.JavaVM.Typed.NewObject
    (
    UnconstructedObject(),
    allocObject,getConstructor,getMakeNewObject,newObject
    ) where
    {
    import Foreign.JavaVM.Typed.Method;
    import Foreign.JavaVM.Typed.Class;
    import Foreign.JavaVM.Typed.Object;
    import Foreign.JavaVM.Typed.Reference;
    import Foreign.JavaVM.Typed.ArgumentList;
    import Foreign.JavaVM.VM;
    import Platform.JavaVM;

    newtype UnconstructedObject c = MkUnconstructedObject (ObjectRef c);
    
    allocObject :: (IsJavaClassMarker c) => JClass -> VM (UnconstructedObject c);
    allocObject (MkTLRef cls) = do
        {
        ref <- vmAllocObject cls;
        obj <- tlFromRef ref;
        return (MkUnconstructedObject obj);
        };

    getConstructor :: (IsJavaClassMarker c,IsJVMArgumentList args) =>
        JClass -> VM (UnconstructedObject c -> args -> IO (ObjectRef c));
    getConstructor cls = do
        {
        constructor <- getNonvirtualMethod cls objectConstructorName;
        return (\(MkUnconstructedObject obj) args -> do
            {
            () <- constructor obj args;
            return obj;
            });
        };

    getMakeNewObject :: (IsJavaClassMarker c,IsJVMArgumentList args) =>
        JClass -> VM (args -> IO (ObjectRef c));
    getMakeNewObject cls = do
        {
        constructor <- getConstructor cls;
        return (\args -> do
            {
            rawObj <- allocObject cls;
            constructor rawObj args;            
            });
        };
    
    newObject :: (IsJavaClassMarker c,IsJVMArgumentList args) =>
        JClass -> args -> VM (ObjectRef c);
    newObject cls args = do
        {
        makeNewObject <- getMakeNewObject cls;
        makeNewObject args;
        };
    }
