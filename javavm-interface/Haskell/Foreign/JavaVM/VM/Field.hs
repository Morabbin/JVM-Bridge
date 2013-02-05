{--
JVM-Bridge -- bridge from FP languages and others to the Java VM
Copyright (C) 2007 Ashley Yakeley <ashley@semantic.org>

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
module Foreign.JavaVM.VM.Field where
{
    import Foreign.JavaVM.VM.Field.Access;
    import Foreign.JavaVM.VM.Field.ID;
    import Foreign.JavaVM.VM.Witness;
    import Foreign.JavaVM.VM.Ref;
    import Foreign.JavaVM.VM.Types;
    import Platform.JavaVM;
    import Control.Monad.Reference;
    import Data.Witness;
    import Data.Nothing;
    
    vmGetStaticField :: (Is VMType t) =>
     VMClassRef -> FieldNameType -> VM (Ref IO t);
    vmGetStaticField cls nt = do
    {
        fid <- vmGetStaticFieldID cls nt;
        if (isNothing fid)
         then fail ("static field "++(show nt)++" not found")
         else return (vmStaticField fid cls);
    };
    
    vmGetField :: (Is VMType t) =>
     VMClassRef -> FieldNameType -> VM (VMRef -> Ref IO t);
    vmGetField cls nt = do
    {
        fid <- vmGetFieldID cls nt;
        if (isNothing fid)
         then fail ("field "++(show nt)++" not found")
         else return (vmField fid);
    };
}
