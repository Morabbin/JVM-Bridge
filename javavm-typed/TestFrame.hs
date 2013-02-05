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
    import Class_java_awt_Component;
    import Class_java_awt_Graphics;
    import Class_java_lang_Class;
    import Class_java_lang_Thread;
    import Class_java_lang_System;
    import JVMBridge;

    data Preload = MkPreload
        {
        cPreload_JGraphics    :: Preload_JGraphics JVM,
        cPreload_JClass        :: Preload_JClass JVM,
        cPreload_JComponent    :: Preload_JComponent JVM,
        cPreload_JThread        :: Preload_JThread JVM,
        cPreload_JSystem        :: Preload_JSystem JVM
        };

    type JVM = JVMUser Preload;

    instance IsJVMLoadable JVMStandard Preload where
        {
        jvmLoad = do
            {
            preload_JGraphics    <- jvmLoad;
            preload_JClass        <- jvmLoad;
            preload_JComponent    <- jvmLoad;
            preload_JThread        <- jvmLoad;
            preload_JSystem        <- jvmLoad;
            return (MkPreload
                preload_JGraphics
                preload_JClass
                preload_JComponent
                preload_JThread
                preload_JSystem
                );
            };
        };

    instance HasPreload_JGraphics JVM where
        {getPreload_JGraphics    = getJVMUserContext >>= (\preload -> return (cPreload_JGraphics preload));};
    instance HasPreload_JClass JVM where
        {getPreload_JClass        = getJVMUserContext >>= (\preload -> return (cPreload_JClass preload));};
    instance HasPreload_JComponent JVM where
        {getPreload_JComponent    = getJVMUserContext >>= (\preload -> return (cPreload_JComponent preload));};
    instance HasPreload_JThread JVM where
        {getPreload_JThread        = getJVMUserContext >>= (\preload -> return (cPreload_JThread preload));};
    instance HasPreload_JSystem JVM where
        {getPreload_JSystem        = getJVMUserContext >>= (\preload -> return (cPreload_JSystem preload));};

    data JHaskellFrameClass;
    instance IsJavaClassMarker JHaskellFrameClass where
        {
        cName Type = toJavaString "org/mypackage/HaskellFrame";
        };
    type JHaskellFrame = ObjectRef JHaskellFrameClass;
    instance IsJjava_awt_Frame JHaskellFrame where
        {
        toJjava_awt_Frame = castTLRef;
        };
    instance IsJjava_awt_Component JHaskellFrame where
        {
        toJjava_awt_Component = castTLRef;
        };
    instance IsJjava_lang_Object JHaskellFrame where
        {
        toJjava_lang_Object = castTLRef;
        };

    paintMethod :: JHaskellFrame -> JGraphics -> JVM Jvoid;
    paintMethod frame graphics = do
        {
        drawOval_JGraphics_Jint_Jint_Jint_Jint graphics (0,0,100,100);
        };
{--
    showClass :: JObject -> JVM ();
    showClass obj = do
        {
        cls <- getClass_JObject obj ();
        s <- toString_JObject (toJObject cls) ();
        text <- toCharArray_JString s ();
        callIO (putStrLn ("class is " ++ (showJavaString text)));
        };
--}
    main :: IO ();
    main = runJVMFromArg (do
        {
        loader    <- getStandardClassLoader;        
        hfClass <- defineCallbackClass loader (Type :: Type JHaskellFrame) (Type :: Type Jjava_awt_Frame)
            []
            [MkMemberNameType
                (toJavaString "paint")
                (MkFunctionType
                    [MkObjectType (toJavaString "java/awt/Graphics")]
                    MkVoidType
                )
            ]
            ;
        
        paintMethodVal    <- makeCallback paintMethod;

        (haskellFrame :: JHaskellFrame) <- debugIOLM "new HaskellFrame" (newObject hfClass (paintMethodVal));
        debugIOLM "setVisible" (setVisible_JComponent_Jboolean haskellFrame True);
        
        callIO (putStrLn ("Yielding to other JVM threads for 20s..."));
        withProcsThread (do
            {
            startTime <- currentTimeMillis_JSystem ();
            yieldLoop (do
                {
                curTime <- currentTimeMillis_JSystem ();
                return (curTime < startTime + 20000);    -- 20s
                });
            return ();
            });
        });
    }
