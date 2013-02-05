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

module Foreign.JavaVM.Typed.Throwable
(
    Class_JThrowable,JThrowable
    ,throwJThrowable,catchJThrowable
) where
{
    import Foreign.JavaVM.Typed.Object;
    import Foreign.JavaVM.Typed.Reference;
    import Foreign.JavaVM.VM;
    import Platform.JavaVM;
    import Data.Witness;

    data Class_JThrowable;
    instance IsJavaClassMarker Class_JThrowable where
    {
        cName Type = "java/lang/Throwable";
    };
    type JThrowable = ObjectRef Class_JThrowable;

    throwJThrowable :: JThrowable -> VM a;
    throwJThrowable (MkTLRef th) = vmThrow th;

    catchJThrowable :: VM a -> (JThrowable -> VM a) -> VM a;
    catchJThrowable foo catchClause = vmCatch foo (\exRef -> catchClause (MkTLRef exRef));
}
