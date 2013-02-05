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

-- four-space tabs
module Main where
{
    import Foreign.JavaVM.Lib.Class_java_lang_System;
    import Foreign.JavaVM.Lib.Class_java_lang_String;
    import TestCallback_JVM;
    import Foreign.JavaVM;
    import Platform.JavaVM;
    import Data.Witness;
    import Control.Concurrent;
    import System.IO;

    -- define our own Object type
    data Class_MyObject;
    instance IsJavaClassMarker Class_MyObject where
    {
        cName Type = "org/semantic/jvmbridge/example/MyObject";
    };
    instance SubJavaClassMarker Class_Jjava_lang_Object        Class_MyObject;
    type MyObject = ObjectRef Class_MyObject;

    -- the callback function written out explicitly
    callbackFunction :: MyObject -> (Jboolean) -> JVM Jvoid;
    callbackFunction _ (x) = do
    {
        putStrLn ("callbackFunction: " ++ (show x));
    };

    logMessage :: String -> JVM ();
    logMessage s = do
    {
        putStrLn s;
        hFlush stdout;
    };

    main :: IO ();
    main = run (runJVM (catchJThrowable (do
    {    
        loader    <- getStandardClassLoader;                                    -- defineCallbackClass needs this
        hcClass <- defineCallbackClass loader (Type :: Type MyObject) (Type :: Type Jjava_lang_Object)
            []                                                                -- no interfaces
            []                                                                -- no constructor arguments other than methodvals
            [MkMemberNameType                                                -- just one overridden method
                "foo"                                                        -- called 'foo'
                (MkFunctionType
                    [MkPrimitiveType (MkAnyWitness Tboolean)]                                            -- empty argument list
                    MkVoidType                                                -- void return type
                )
            ]
            ;

        callbackFunctionVal    <- makeCallback (callbackFunction);    -- convert the method to a 'val' that can be passed to a constructor
        (myObject :: MyObject) <- newObject hcClass (callbackFunctionVal);    -- create a new MyObject
        
        foo <- getMethod hcClass "foo";

        logMessage "H+ main foo call";
        () <- foo myObject (False);
        logMessage "H- main foo call";

        mvar <- newEmptyMVar;
        logMessage "H+ fork";
        forkJVMThread (do
        {
            logMessage "H+ thread foo call";
            catchJThrowable (do
            {
                logMessage "H+ thread inside catch";
                () <- foo myObject (True);
                putMVar mvar ();
                logMessage "H- thread inside catch";
            })(\jex -> do
            {
                logMessage "JVM exception in other thread";
            });
            logMessage "H- thread foo call";
        });
        logMessage "H- fork";
        
        logMessage "Yielding to other Haskell threads...";            -- tell user what's going on
        withProcsThread (yieldLoop (do                                        -- yields to other Java and Haskell threads
        {
            logMessage "yield";
            takeMVar mvar;
            logMessage "got mvar";
            return False;
        }));
        logMessage "Done!";
    })
    (\jex -> do
    {
        logMessage "JVM exception in main thread";
    })));
}
