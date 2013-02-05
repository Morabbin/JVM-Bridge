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

module Foreign.JavaVM.VM.Class where
    {
    import Foreign.JavaVM.VM.Throwable;
    import Foreign.JavaVM.VM.StringPtr;
    import Foreign.JavaVM.VM.Ref;
    import Foreign.JavaVM.VM.Types;
    import Foreign.JavaVM.Raw;
    import Platform.JavaVM;

    vmFindClass :: ClassName -> VM VMClassRef;
    vmFindClass className = checkCall (do
        {
        raw <- withJString className (rawFindClass ?jvmenv);
        makeVMRefFromLocal raw;
        });
    }
