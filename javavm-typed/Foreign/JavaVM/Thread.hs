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

module Foreign.JavaVM.Thread where
{
    import Foreign.JavaVM.Lib.Class_java_lang_Thread;
    import Foreign.JavaVM.Lib.Class_java_lang_Class(Preload_Class);
    import Foreign.JavaVM.Callback;
    import Foreign.JavaVM.Lib.Header;
    import Foreign.JavaVM.Typed;
    import Platform.JavaVM;
    import Data.Witness;
    
    data Class_JHaskellThread;
    instance IsJavaClassMarker Class_JHaskellThread where
    {
        cName Type = "org/semantic/jvmbridge/HaskellThread";
    };
    instance SubJavaClassMarker Class_Jjava_lang_Thread    Class_JHaskellThread;
    instance SubJavaClassMarker Class_Jjava_lang_Object    Class_JHaskellThread;

    type JHaskellThread = ObjectRef Class_JHaskellThread;

    getRunnableClass :: (?preload_java_lang_Thread :: Preload_Thread,
    ?preload_java_lang_Class :: Preload_Class) => 
     VM JClass;
    getRunnableClass = do
    {        
        loader    <- getStandardClassLoader;
        makeRunnableClass loader;
    };
    
    makeRunnableClass :: (?preload_java_lang_Thread :: Preload_Thread) => 
     Jjava_lang_ClassLoader -> VM JClass;
    makeRunnableClass loader = defineCallbackClass loader (Type :: Type JHaskellThread) (Type :: Type JThread) []
        []
        [
        MkMemberNameType "run" (MkFunctionType [] MkVoidType)
        ]
        ;
    
    forkJVMThread :: (?preload_java_lang_Thread :: Preload_Thread,
    ?preload_java_lang_Class :: Preload_Class) => 
     VM () -> VM JThread;
    forkJVMThread foo = do
    {
        htClass <- getRunnableClass;
        fooVal    <- makeCallback (\(_::JHaskellThread) () -> foo);
        thread <- newObject htClass (fooVal);
        start_JThread thread ();
        return thread;
    };
}
