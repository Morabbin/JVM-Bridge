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

module Foreign.JavaVM.Typed.Class
    (
    Class_JClass,
    JClass,
    findClassByName,
    findClass,
    getClass,
    isInstanceOfClass,
    isInstanceOf,
    SubJavaClassMarker(..)
    ) where
    {
    import Foreign.JavaVM.Typed.Object;
    import Foreign.JavaVM.Typed.Reference;
    import Foreign.JavaVM.VM;
    import Platform.JavaVM;
    import Data.Subtype;
    import Data.Witness;

    data Class_JClass;
    instance IsJavaClassMarker Class_JClass where
        {
        cName Type = "java/lang/Class";
        };
    type JClass = ObjectRef Class_JClass;

    findClassByName :: ClassName -> VM JClass;
    findClassByName cname = do
        {
        classRef <- vmFindClass cname;
        return (MkTLRef classRef);
        };
    
    findClass :: (IsJVMReference t) => Type t -> VM JClass;
    findClass t = findClassByName (findClassName t);
    
    getClass :: (IsJVMReference t) => t -> VM JClass;
    getClass obj = findClass (getRepresentative obj);
    
    isInstanceOfClass :: (IsJVMReference t) => JClass -> t -> VM Bool;
    isInstanceOfClass (MkTLRef cl) obj = do
        {
        vmObj <- tlToRef obj;
        vmIsInstanceOf vmObj cl;
        };
    
    isInstanceOf :: (IsJVMReference ct, IsJVMReference ot) => Type ct -> ot -> VM Bool;
    isInstanceOf t obj = do
        {
        cl <- findClass t;
        isInstanceOfClass cl obj;
        };

    class (IsJavaClassMarker super,IsJavaClassMarker sub) => SubJavaClassMarker super sub where
        {
        objUpcast :: ObjectRef sub -> ObjectRef super;
        objUpcast sub = castTLRef sub;

        objDowncast :: ObjectRef super -> VM (Maybe (ObjectRef sub));
        objDowncast obj = do
            {
            isObj <- objIs (Type :: Type (ObjectRef sub)) obj;
            return (if isObj then (Just (castTLRef obj)) else Nothing);
            };

        objIs :: Type (ObjectRef sub) -> ObjectRef super -> VM Bool;
        objIs = isInstanceOf;
        };

    instance
        (
        SubJavaClassMarker super sub
        ) =>
     IsA (ObjectRef super) (ObjectRef sub) where
        {
        convert    = objUpcast;
        };

    instance
        (
        SubJavaClassMarker super sub
        ) =>
     JVMIsA (ObjectRef super) (ObjectRef sub) where
        {
        jvmConvert sub    = return (upcast sub);
        };
    }
