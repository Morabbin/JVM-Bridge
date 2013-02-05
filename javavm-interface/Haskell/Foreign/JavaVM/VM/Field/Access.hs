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

module Foreign.JavaVM.VM.Field.Access(vmStaticField,vmField) where
{
    import Foreign.JavaVM.VM.Field.ID;
    import Foreign.JavaVM.VM.Witness;
    import Foreign.JavaVM.VM.Ref;
    import Foreign.JavaVM.VM.Types;
    import Foreign.JavaVM.Raw;
    import Platform.JavaVM;
    import Control.Monad.Reference;
    import Data.Witness;

    convertGet :: (RawEnv -> RawGlobalRef -> fid -> IO a) ->
        (fid -> VMRef -> VM a);
    convertGet foo field ref = withVMRefRawGlobal ref (\rawRef -> foo ?jvmenv rawRef field);

    convertSet :: (RawEnv -> RawGlobalRef -> fid -> a -> IO ()) ->
        (fid -> VMRef -> a -> VM ());
    convertSet foo field ref val = withVMRefRawGlobal ref (\rawRef -> foo ?jvmenv rawRef field val);

    vmGetStaticField :: (Is VMType t) =>
     VMStaticFieldID -> VMClassRef -> VM t;
    vmGetStaticField = vmGetStaticField' representative where
    {
        vmGetStaticField' ::
         VMType t -> VMStaticFieldID -> VMClassRef -> VM t;
        vmGetStaticField' (Tprimitive Tboolean)    = \field ref -> do
        {
            rawB <- convertGet rawGetStaticBooleanField field ref;
            return (fromRawBoolean rawB);
        };
        vmGetStaticField' (Tprimitive Tbyte)    = convertGet rawGetStaticByteField;
        vmGetStaticField' (Tprimitive Tchar)    = convertGet rawGetStaticCharField;
        vmGetStaticField' (Tprimitive Tshort)    = convertGet rawGetStaticShortField;
        vmGetStaticField' (Tprimitive Tint)        = convertGet rawGetStaticIntField;
        vmGetStaticField' (Tprimitive Tlong)    = convertGet rawGetStaticLongField;
        vmGetStaticField' (Tprimitive Tfloat)    = convertGet rawGetStaticFloatField;
        vmGetStaticField' (Tprimitive Tdouble)    = convertGet rawGetStaticDoubleField;
        vmGetStaticField' Tref        = \field ref -> do
        {
            locRef <- convertGet rawGetStaticRefField field ref;
            makeVMRefFromLocal locRef;
        };
    };

    vmSetStaticField :: (Is VMType t) =>
     VMStaticFieldID -> VMClassRef -> t -> VM ();
    vmSetStaticField = vmSetStaticField' representative where
    {
        vmSetStaticField' ::
         VMType t -> VMStaticFieldID -> VMClassRef -> t -> VM ();
        vmSetStaticField' (Tprimitive Tboolean)    = \field ref val ->
            convertSet rawSetStaticBooleanField field ref (toRawBoolean val);
        vmSetStaticField' (Tprimitive Tbyte)    = convertSet rawSetStaticByteField;
        vmSetStaticField' (Tprimitive Tchar)    = convertSet rawSetStaticCharField;
        vmSetStaticField' (Tprimitive Tshort)    = convertSet rawSetStaticShortField;
        vmSetStaticField' (Tprimitive Tint)        = convertSet rawSetStaticIntField;
        vmSetStaticField' (Tprimitive Tlong)    = convertSet rawSetStaticLongField;
        vmSetStaticField' (Tprimitive Tfloat)    = convertSet rawSetStaticFloatField;
        vmSetStaticField' (Tprimitive Tdouble)    = convertSet rawSetStaticDoubleField;
        vmSetStaticField' Tref = \field ref val -> withVMRefRawGlobal val (\rawVal -> 
            convertSet rawSetStaticRefField field ref rawVal
            );
    };

    vmStaticField :: (?jvmenv :: VMEnv,Is VMType t) =>
     VMStaticFieldID -> VMClassRef -> Ref IO t;
    vmStaticField fid cls = MkRef (vmGetStaticField fid cls) (vmSetStaticField fid cls)    ;

    vmGetField :: (Is VMType t) =>
     VMFieldID -> VMRef -> VM t;
    vmGetField = vmGetField' representative where
    {
        vmGetField' ::
         VMType t -> VMFieldID -> VMRef -> VM t;
        vmGetField' (Tprimitive Tboolean)    = \field ref -> do
        {
            rawB <- convertGet rawGetBooleanField field ref;
            return (fromRawBoolean rawB);
        };
        vmGetField' (Tprimitive Tbyte)        = convertGet rawGetByteField;
        vmGetField' (Tprimitive Tchar)        = convertGet rawGetCharField;
        vmGetField' (Tprimitive Tshort)        = convertGet rawGetShortField;
        vmGetField' (Tprimitive Tint)        = convertGet rawGetIntField;
        vmGetField' (Tprimitive Tlong)        = convertGet rawGetLongField;
        vmGetField' (Tprimitive Tfloat)        = convertGet rawGetFloatField;
        vmGetField' (Tprimitive Tdouble)    = convertGet rawGetDoubleField;
        vmGetField' Tref    = \field ref -> do
        {
            locRef <- convertGet rawGetRefField field ref;
            makeVMRefFromLocal locRef;
        };
    };

    vmSetField :: (Is VMType t) =>
     VMFieldID -> VMRef -> t -> VM ();
    vmSetField = vmSetField' representative where
    {
        vmSetField' ::
         VMType t -> VMFieldID -> VMRef -> t -> VM ();
        vmSetField' (Tprimitive Tboolean)    = \field ref val->
            convertSet rawSetBooleanField field ref (toRawBoolean val);
        vmSetField' (Tprimitive Tbyte)        = convertSet rawSetByteField;
        vmSetField' (Tprimitive Tchar)        = convertSet rawSetCharField;
        vmSetField' (Tprimitive Tshort)        = convertSet rawSetShortField;
        vmSetField' (Tprimitive Tint)        = convertSet rawSetIntField;
        vmSetField' (Tprimitive Tlong)        = convertSet rawSetLongField;
        vmSetField' (Tprimitive Tfloat)        = convertSet rawSetFloatField;
        vmSetField' (Tprimitive Tdouble)    = convertSet rawSetDoubleField;
        vmSetField' Tref    = \field ref val-> withVMRefRawGlobal val (\rawVal -> 
            convertSet rawSetRefField field ref rawVal
            );
    };

    vmField :: (?jvmenv :: VMEnv,Is VMType t) =>
     VMFieldID -> VMRef -> Ref IO t;
    vmField fid obj = MkRef (vmGetField fid obj) (vmSetField fid obj);
}
