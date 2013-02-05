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

module Foreign.JavaVM.Typed.Invocation(VM,module Foreign.JavaVM.Typed.Invocation) where
{
    import Foreign.JavaVM.Typed.Method;
    import Foreign.JavaVM.Typed.ListArray();
    import Foreign.JavaVM.Typed.String;
    import Foreign.JavaVM.Typed.Tuple();
    import Foreign.JavaVM.Typed.Throwable;
    import Foreign.JavaVM.Typed.Class;
    import Foreign.JavaVM.VM;
    import Platform.JavaVM;
    import Data.Witness;

    runJavaClass :: ClassName -> [JString] -> VM ();
    runJavaClass cname args = do
    {
        mainClass <- findClassByName cname;
        callStaticMethod mainClass "main" args;
    };
    
    reportThrowable :: JThrowable -> VM ();
    reportThrowable ex = do
    {
        throwClass        <- findClass (Type::Type JThrowable);
        toStringMethod    <- getMethod throwClass "toString";
        jstr            <- toStringMethod ex ();
        str                <- getStringUTF jstr;
        putStrLn ("Java throwable: " ++ (showUTF8 str));
    };
    
    runReport :: VM () -> VM ();
    runReport foo = (catchJThrowable (catchJThrowable foo reportThrowable) (\(_ :: JThrowable) -> 
        putStrLn "caught Java throwable in catch code!"
        ));

    runVMReport :: [String] -> VM () -> IO ();
    runVMReport classPath foo = vmRun classPath (runReport foo);
}
