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

-- primitive types converted
-- raw and local refs converted with finalizers
-- StringPtrs converted
module Foreign.JavaVM.VM
(
    VMEnv,VM,JavaVM(..),badVM,
    module Foreign.JavaVM.VM.Witness,
    VMRef(),VMClassRef,
    module Foreign.JavaVM.VM.Object,
    module Foreign.JavaVM.VM.String,
    module Foreign.JavaVM.VM.Class,
    module Foreign.JavaVM.VM.Thread,
    vmThrow,vmCatch,
    module Foreign.JavaVM.VM.Field,
    module Foreign.JavaVM.VM.Method,
    module Foreign.JavaVM.VM.NewObject,
    module Foreign.JavaVM.VM.Array,
    module Foreign.JavaVM.VM.Callback,
    module Foreign.JavaVM.VM.Invocation
) where
{
    import Foreign.JavaVM.VM.Invocation;
    import Foreign.JavaVM.VM.Callback;
    import Foreign.JavaVM.VM.Array;
    import Foreign.JavaVM.VM.NewObject;
    import Foreign.JavaVM.VM.Method;
    import Foreign.JavaVM.VM.Field;
    import Foreign.JavaVM.VM.Throwable;
    import Foreign.JavaVM.VM.Thread;
    import Foreign.JavaVM.VM.Class;
    import Foreign.JavaVM.VM.String;
    import Foreign.JavaVM.VM.Object;
    import Foreign.JavaVM.VM.Ref;
    import Foreign.JavaVM.VM.Witness;
    import Foreign.JavaVM.VM.Types;
}
