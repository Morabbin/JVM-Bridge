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
    import Foreign.JavaVM.Lib.Class_MyClass;
    import InterfaceMyClass_JVM;
    import Foreign.JavaVM;
    import Platform.JavaVM;

    main :: IO ();
    main = runWithClasspath ["./"] (runJVM (do
    {
        sayHello_JMyClass ();
        d <- square_JMyClass_Jdouble (57.2);
        putStrLn ("57.2 ^ 2 = " ++ (show d));
    }));
}
