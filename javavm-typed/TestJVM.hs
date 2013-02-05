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

module Main where
    {
    import TestStuff;
    import JVMBridge;
    import System;
    import MonadError;

    type JVM = JVMUser ();

    voidFunc :: (IsJVMMonad m) => () -> m Jvoid;
    voidFunc vl = do
        {
        callIO (putStrLn "This is voidFunc");
        return jVoid;
        };

    testCallback :: (IsJVMMonad m) => (() -> m Jvoid) -> m Jvoid;
    testCallback foo = do
        {
        theCallback                    <- makeStaticCallback foo;
        executeClass                <- findClassByName (toJavaString "org/semantic/jvmbridge/ExecuteFunction");
        createValueListMethod        <- getStaticMethod executeClass (toJavaString "createValueList") (Type::Type ()) returnInt;
        executeVoidFunctionMethod    <- getStaticMethod executeClass (toJavaString "executeVoidFunctionNow") (Type::Type (Jint,Jint)) returnVoid;
        emptyArgList                <- createValueListMethod ();
        debugIOLM "testCallVoidCallback" (executeVoidFunctionMethod (theCallback,emptyArgList));
        };

    callClassMain :: (IsJVMMonad m) => ClassName -> [JString] -> m ();
    callClassMain appClassName args = do
        {
        mainClass    <- findClassByName appClassName;
        mainMethod    <- getStaticMethod mainClass (toJavaString "main") (Type::Type [JString]) returnVoid;
        mainMethod args;
        };

    main :: IO ();
    main = testRunJVM (do
        {
        -- find Object class;
        objClass <- findClass (Type::Type JObject);

        -- call org.semantic.test.TestApp.main([]);
        callClassMain (toJavaString "org/semantic/test/TestApp") [];
        
        -- call callback voidFunc
        testCallback voidFunc;

        return ();
        } :: JVM ());
    }
