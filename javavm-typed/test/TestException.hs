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
    import Foreign.JavaVM.Lib.Class_java_io_PrintStream;
    import Foreign.JavaVM.Lib.Class_java_lang_System;
    import Foreign.JavaVM.Lib.Class_java_lang_String;
    import Foreign.JavaVM.Lib.Class_java_lang_Throwable;
    import TestException_JVM;
    import Foreign.JavaVM;
    import Platform.JavaVM;
    import Control.Monad.Reference;
    import Data.Nothing;

    causeNullPointerException :: JVM ();
    causeNullPointerException = do
        {
        (nullString :: Jjava_lang_String) <- getNothing;    -- null string object
        getBytes_JString nullString ();                        -- try to call getBytes
        return ();
        };

    mainProg :: JVM ();
    mainProg = do
        {
        outStream <- get1 out_JSystem;
        println_JPrintStream_ArrayJchar outStream ("starting mainProg..." :: JavaString);
        catchJThrowable causeNullPointerException (\ex -> do
            {
            println_JPrintStream_ArrayJchar outStream ("caught:" :: JavaString);
            printStackTrace_JThrowable_JPrintStream ex outStream;
            });
        println_JPrintStream_ArrayJchar outStream ("mainProg done" :: JavaString);
        };

    main :: IO ();
    main = do
        {
        putStrLn "starting TestException...";
        run (runJVM mainProg);
        putStrLn "TestException done";
        };
    }
