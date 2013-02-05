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

    type JVM = JVMUser ();

    data JHaskellClass;
    instance IsJavaClassMarker JHaskellClass where
        {
        cName Type = toJavaString "org/mypackage/Haskell";
        };
    type JHaskell = ObjectRef JHaskellClass;
    instance IsJObject JHaskell where
        {
        toJObject (MkJVMRef r) = (MkJVMRef r);
        };
    
    fooMethodName = toJavaString "foo";

    fooMethod :: JHaskell -> (Jint) -> JVM Jvoid;
    fooMethod _ i = do
        {
        callIO (putStrLn (show i));
        };

    getShowClass :: JVM (JObject -> JVM ());
    getShowClass = do
        {
        objClass    <- getObjectClass;
        getClass    <- getgetClassMethod objClass;
        toString    <- gettoStringMethod objClass;
        stringClass    <- getStringClass;
        toCharArray    <- gettoCharArrayMethod stringClass;
        return (\obj -> do
            {
            cls <- getClass obj ();
            s <- toString (toJObject cls) ();
            text <- toCharArray s ();
            callIO (putStrLn ("class is " ++ (showJavaString text)));
            });
        };

    main :: IO ();
    main = testRunJVM (do
        {
        efClass    <- findClassByName (toJavaString "org/semantic/jvmbridge/ExecuteFunction");
        efNull    <- getIsNothing efClass;
        callIO (putStrLn ("ExecuteFunction " ++ (if efNull then "not found" else "found")));
        
        classClass        <- getClassClass;
        getClassLoader    <- getgetClassLoaderMethod classClass;
        
        (MkJVMRef loader)    <- getClassLoader efClass ();
        
        hClass <- tlDefineCallbackClass loader (className (Type :: Type JHaskell)) (toJavaString "java/lang/Object")
            []
            [MkMemberNameType
                fooMethodName
                (MkFunctionType
                    [MkIntType]
                    MkVoidType
                )
            ]
            ;
        
        fooMethodVal    <- makeCallback fooMethod;

        (haskell :: JHaskell) <- debugIOLM "new Haskell" (newObject hClass (fooMethodVal));
        foo                <- getMethod hClass fooMethodName (Type :: Type (Jint)) (Type :: Type Jvoid);
        
        systemClass        <- getSystemClass;
        currentTimeMillis <- getcurrentTimeMillisMethod systemClass;        
        threadClass        <- getThreadClass;
        javaYield        <- getyieldMethod threadClass;        
        callIO (putStrLn ("Yielding to other JVM threads for 1s..."));
        withProcsThread (do
            {
            startTime <- currentTimeMillis ();
            tlYieldLoop javaYield (do
                {
                curTime <- currentTimeMillis ();
                foo haskell (fromIntegral curTime);
                return (curTime < startTime + 1000);    -- 1s
                });
            return ();
            });
        return ();
        });
    }
