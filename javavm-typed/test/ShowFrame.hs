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
    import Foreign.JavaVM.Lib.Class_java_awt_Container;
    import Foreign.JavaVM.Lib.Class_java_awt_Component;
    import Foreign.JavaVM.Lib.Class_java_awt_Graphics;
    import ShowFrame_JVM;
    import Foreign.JavaVM;
    import Platform.JavaVM;
    import Data.Witness;


    -- define our own Component type
    data Class_MyComponent;
    instance IsJavaClassMarker Class_MyComponent where
        {
        cName Type = "org/semantic/jvmbridge/example/MyComponent";
        };
    instance SubJavaClassMarker Class_Jjava_awt_Canvas        Class_MyComponent;
    instance SubJavaClassMarker Class_Jjava_awt_Component    Class_MyComponent;
    instance SubJavaClassMarker Class_Jjava_lang_Object        Class_MyComponent;
    type MyComponent = ObjectRef Class_MyComponent;

    -- the paint method written out explicitly
    paintMethod :: MyComponent -> (JGraphics) -> JVM Jvoid;
    paintMethod frame (graphics) = do
        {
        width <- getWidth_JComponent frame ();
        height <- getHeight_JComponent frame ();
        drawOval_JGraphics_Jint_Jint_Jint_Jint graphics (0,0,width,height);
        };

    main :: IO ();
    main = run (runJVM (do
        {
        loader    <- getStandardClassLoader;                                    -- defineCallbackClass needs this
        hcClass <- defineCallbackClass loader (Type :: Type MyComponent) (Type :: Type Jjava_awt_Canvas)
            []                                                                -- no interfaces
            []                                                                -- no constructor arguments other than methodvals
            [MkMemberNameType                                                -- just one overridden method
                "paint"                                                        -- called 'paint'
                (MkFunctionType
                    [MkObjectType "java/awt/Graphics"]                        -- argument list is just a java.awt.Graphics
                    MkVoidType                                                -- void return type
                )
            ]
            ;

        paintMethodVal    <- makeCallback paintMethod;                        -- convert the method to a 'val' that can be passed to a constructor

        (myComponent :: MyComponent) <- newObject hcClass (paintMethodVal);    -- create a new MyComponent

        title <- new_JString_ArrayJchar ("Haskell / Java VM" :: JavaString);-- title String for the frame
        frame <- new_JFrame_JString (title);                                -- create a new Frame
        add_JContainer_JComponent frame (myComponent);                        -- put the MyComponent in the Frame
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
