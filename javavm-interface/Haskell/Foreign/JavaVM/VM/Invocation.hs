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

module Foreign.JavaVM.VM.Invocation
(
    vmCreateJavaVM,vmRun
) where
{
    import Foreign.JavaVM.VM.Thread;
    import Foreign.JavaVM.VM.Types;
    import Foreign.JavaVM.Raw;
    import Foreign.C;
    import Foreign;

    withCStringArray :: [String] -> ([CString] -> IO a) -> IO a;
    withCStringArray [] f = f [];
    withCStringArray (s:ss) f = 
     withCString s (\c ->
     withCStringArray ss (\cc ->
     f (c:cc)
     ));

    vmCreateJavaVM :: [String] -> IO JavaVM;
    vmCreateJavaVM classPath = do
    {
        vm <- withCStringArray classPath (\cc -> withArray (cc ++ [nullPtr]) rawCreateJavaVM);
        if (badVM vm) then (fail "couldn't create Java VM") else (return vm);
    };

    vmRun :: [String] -> VM a -> IO a;
    vmRun classPath foo = do
    {
        vm <- vmCreateJavaVM classPath;
        vmAttachedThread vm foo;
    };
}
