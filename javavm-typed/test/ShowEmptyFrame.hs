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
    import Foreign.JavaVM.Lib.Class_java_awt_Frame;
    import Foreign.JavaVM.Lib.Class_java_awt_Component;
    import ShowEmptyFrame_JVM;
    import Foreign.JavaVM;
    import Platform.JavaVM;

    main :: IO ();
    main = run (runJVM (do
        {        
        title <- new_JString_ArrayJchar ("Haskell / Java VM" :: JavaString);    -- title String for the frame
        frame <- new_JFrame_JString (title);                                -- create a new Frame
        setVisible_JComponent_Jboolean frame True;                            -- show the Frame
        
        putStrLn ("Yielding to other JVM threads for 30s...");        -- tell user what's going on
        withProcsThread (do
            {
            startTime <- currentTimeMillis_JSystem ();
            yieldLoop (do                                                    -- yields to other Java and Haskell threads
                {
                curTime <- currentTimeMillis_JSystem ();
                return (curTime < startTime + 30000);                        -- 30s yet?
                });
            });
        }));
    }
