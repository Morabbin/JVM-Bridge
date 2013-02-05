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

module Foreign.JavaVM.Callback where
{
    import Foreign.JavaVM.Lib.Class_java_lang_Class;
    import Foreign.JavaVM.Lib.Class_java_lang_Thread;
    import Foreign.JavaVM.Lib.Header;
    import Foreign.JavaVM.Typed;
    import Platform.JavaVM;
    import Data.Witness;

    getStandardClassLoader :: (?preload_java_lang_Class :: Preload_Class) => VM Jjava_lang_ClassLoader;
    getStandardClassLoader = do
    {
        efClass    <- getExecuteFunctionClass;
        getClassLoader_JClass efClass ();
    };

    defineCallbackClass :: (IsJavaClassMarker c,IsJavaClassMarker base) =>
        Jjava_lang_ClassLoader -> Type (ObjectRef c) -> Type (ObjectRef base) -> [ClassName] -> [ValueType] -> [MethodNameType] -> VM Foreign.JavaVM.Typed.JClass;
    defineCallbackClass (MkTLRef loader) tClass tBase = tlDefineCallbackClass loader (className tClass) (className tBase);

    yieldLoop :: (?preload_java_lang_Thread::Preload_Thread) => VM Bool -> VM ();
    yieldLoop = tlYieldLoop yield_JThread;    
}
