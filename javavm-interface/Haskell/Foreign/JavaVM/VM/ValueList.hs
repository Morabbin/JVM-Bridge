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

module Foreign.JavaVM.VM.ValueList(makeValueList,getValueList) where
{
    import Foreign.JavaVM.VM.Witness;
    import Foreign.JavaVM.VM.Ref;
    import Foreign.JavaVM.VM.Types;
    import Foreign.JavaVM.Raw;
    import Platform.JavaVM;
    import Data.Witness;
    import Data.Word;

    makeValueList :: [Any VMType] -> IO RawValueList;
    makeValueList args = do
    {
        vl <- rawCreateValueList;
        mapM_ (addArg vl) args;
        return vl;
    } where
    {
        addArg :: RawValueList -> Any VMType -> IO ();
        addArg vl (MkAny rep arg) = vmAddToValueList rep vl arg;

        vmAddToValueList :: VMType t -> RawValueList -> t -> IO ();
        vmAddToValueList (Tprimitive Tboolean)    = \vl x -> rawAddBooleanToValueList vl (toRawBoolean x);
        vmAddToValueList (Tprimitive Tbyte)        = rawAddByteToValueList;
        vmAddToValueList (Tprimitive Tchar)        = rawAddCharToValueList;
        vmAddToValueList (Tprimitive Tshort)    = rawAddShortToValueList;
        vmAddToValueList (Tprimitive Tint)        = rawAddIntToValueList;
        vmAddToValueList (Tprimitive Tlong)        = rawAddLongToValueList;
        vmAddToValueList (Tprimitive Tfloat)    = rawAddFloatToValueList;
        vmAddToValueList (Tprimitive Tdouble)    = rawAddDoubleToValueList;
        vmAddToValueList Tref         = \vl ref -> withVMRefRawGlobal ref (\rawRef -> rawAddRefToValueList vl rawRef);
    };

    getValueList :: [AnyWitness VMType] -> RawValueList -> VM [Any VMType];
    getValueList = getValueList' 0 where
    {
        getValueList' :: Word16 -> [AnyWitness VMType] -> RawValueList -> VM [Any VMType];
        getValueList' _ [] _ = return [];
        getValueList' i (MkAnyWitness t:ws) vl = withRepresentative (\t' -> do
        {
            v <- vmGetNthValue t' vl i;
            vs <- getValueList' (i + 1) ws vl;
            return (MkAny t' v:vs);
        }) t;

        vmGetNthValue :: VMType t -> RawValueList -> Word16 -> VM t;    
        vmGetNthValue (Tprimitive Tboolean)    = \vl i -> do
        {
            raw <- rawGetNthValueAsBoolean vl i;
            return (fromRawBoolean raw);
        };
        vmGetNthValue (Tprimitive Tbyte)    = rawGetNthValueAsByte;
        vmGetNthValue (Tprimitive Tchar)    = rawGetNthValueAsChar;
        vmGetNthValue (Tprimitive Tshort)    = rawGetNthValueAsShort;
        vmGetNthValue (Tprimitive Tint)        = rawGetNthValueAsInt;
        vmGetNthValue (Tprimitive Tlong)    = rawGetNthValueAsLong;
        vmGetNthValue (Tprimitive Tfloat)    = rawGetNthValueAsFloat;
        vmGetNthValue (Tprimitive Tdouble)    = rawGetNthValueAsDouble;
        vmGetNthValue Tref        = \vl i -> do
        {
            rawGlobal <- rawGetNthValueAsRef vl i;
            makeVMRefFromGlobal rawGlobal;
        };
    };
}
