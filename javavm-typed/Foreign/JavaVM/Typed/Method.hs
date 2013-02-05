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

module Foreign.JavaVM.Typed.Method
    (
    getFunctionType,
    getStaticMethodbyType,getMethodByType,getNonvirtualMethodByType,
    getStaticMethod,getMethod,getNonvirtualMethod,
    callStaticMethod,callMethod,callNonvirtualMethod
    ) where
    {
    import Foreign.JavaVM.Typed.Class;
    import Foreign.JavaVM.Typed.Object;
    import Foreign.JavaVM.Typed.Reference;
    import Foreign.JavaVM.Typed.ArgumentList;
    import Foreign.JavaVM.Typed.Returnable;
    import Foreign.JavaVM.VM;
    import Platform.JavaVM;
    import Data.Witness;

    getFunctionType :: (IsJVMArgumentList args,IsJVMReturnable ret) => Type args -> Type ret -> FunctionType;
    getFunctionType a r = MkFunctionType (getListType a) (getReturnableType r);

    getStaticMethodbyType :: (IsJVMArgumentList args,IsJVMReturnable ret) =>
        JClass -> MemberName -> Type args -> Type ret -> VM (args -> IO ret);
    getStaticMethodbyType (MkTLRef jClass) name argType retType = tlFromVMReturnType (\fromVMRT -> do
        {
        method <- vmGetStaticMethod jClass (MkMemberNameType name (getFunctionType argType retType));
        return (\args -> do
            {
            vmargs <- tlToArgList args;
            vmrt <- method jClass vmargs;
            fromVMRT vmrt;
            });
        });

    getMethodByType :: (IsJavaClassMarker c,IsJVMArgumentList args,IsJVMReturnable ret) =>
        JClass -> MemberName -> Type args -> Type ret -> VM (ObjectRef c -> args -> IO ret);
    getMethodByType (MkTLRef jClass) name argType retType = tlFromVMReturnType (\fromVMRT -> do
        {
        method <- vmGetMethod jClass (MkMemberNameType name (getFunctionType argType retType));
        return (\obj -> \args -> do
            {
            vmargs <- tlToArgList args;
            rawObj <- tlToRef obj;
            vmrt <- method Nothing rawObj vmargs;
            fromVMRT vmrt;
            });
        });

    getNonvirtualMethodByType :: (IsJavaClassMarker c,IsJVMArgumentList args,IsJVMReturnable ret) =>
        JClass -> MemberName -> Type args -> Type ret -> VM (ObjectRef c -> args -> IO ret);
    getNonvirtualMethodByType (MkTLRef jClass) name argType retType = tlFromVMReturnType (\fromVMRT -> do
        {
        method <- vmGetMethod jClass (MkMemberNameType name (getFunctionType argType retType));
        return (\obj args -> do
            {
            vmargs <- tlToArgList args;
            rawObj <- tlToRef obj;
            vmrt <- method (Just jClass) rawObj vmargs;
            fromVMRT vmrt;
            });
        });

    getStaticMethod :: (IsJVMArgumentList args,IsJVMReturnable ret) =>
        JClass -> MemberName -> VM (args -> IO ret);
    getStaticMethod cls name =
        getStaticMethodbyType cls name Type Type;

    getMethod :: (IsJavaClassMarker c,IsJVMArgumentList args,IsJVMReturnable ret) =>
        JClass -> MemberName -> VM (ObjectRef c -> args -> IO ret);
    getMethod cls name =
        getMethodByType cls name Type Type;

    getNonvirtualMethod :: (IsJavaClassMarker c,IsJVMArgumentList args,IsJVMReturnable ret) =>
        JClass -> MemberName -> VM (ObjectRef c -> args -> IO ret);
    getNonvirtualMethod cls name =
        getNonvirtualMethodByType cls name Type Type;
    
    callStaticMethod :: (IsJVMArgumentList args,IsJVMReturnable ret) =>
        JClass -> MemberName -> args -> VM ret;
    callStaticMethod cls name args = do
        {
        method <- getStaticMethod cls name;
        method args;
        };

    callMethod :: (IsJavaClassMarker c,IsJVMArgumentList args,IsJVMReturnable ret) =>
        ObjectRef c -> MemberName -> args -> VM ret;
    callMethod obj name args = do
        {
        cls <- getClass obj;
        method <- getMethod cls name;
        method obj args;
        };

    callNonvirtualMethod :: (IsJavaClassMarker c,IsJVMArgumentList args,IsJVMReturnable ret) =>
        JClass -> ObjectRef c -> MemberName -> args -> VM ret;
    callNonvirtualMethod cls obj name args = do
        {
        method <- getNonvirtualMethod cls name;
        method obj args;
        };
    }
