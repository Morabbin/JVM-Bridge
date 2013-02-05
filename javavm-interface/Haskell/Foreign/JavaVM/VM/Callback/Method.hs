{-
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
-}

module Foreign.JavaVM.VM.Callback.Method
    (
    makeMethod
    ) where
    {
    import Foreign.JavaVM.VM.Callback.Field;
    import Foreign.JavaVM.Configure;
    import Platform.JavaVM;
    import Data.Witness;
    import Data.Word;
    
    insCreateValueList :: Instruction;
    insCreateValueList = insINVOKESTATIC (MkMemberRef executeFunctionClassName 
        (MkMemberNameType "createValueList" (MkFunctionType [] (MkValueType opaqueAddressType)))
        );
    
    objectValueType :: ValueType;
    objectValueType = MkObjectType (objectClassName);
    
    atvValArgument :: ValueType -> ValueType; -- addToValueList takes an Object for all ref types
    atvValArgument (MkObjectType _) = objectValueType;
    atvValArgument (MkArrayType _) = objectValueType;
    atvValArgument t = t;
    
    insAddToValueList :: ValueType -> Instruction;
    insAddToValueList valType = insINVOKESTATIC (MkMemberRef executeFunctionClassName 
        (MkMemberNameType "addToValueList" (MkFunctionType [opaqueAddressType,atvValArgument valType] MkVoidType))
        );
    
    executeFunctionNameRoot :: ReturnType -> MemberName;
    executeFunctionNameRoot MkVoidType                                                = "executeVoidFunction";
    executeFunctionNameRoot (MkValueType (MkPrimitiveType (MkAnyWitness Tboolean)))    = "executeBooleanFunction";
    executeFunctionNameRoot (MkValueType (MkPrimitiveType (MkAnyWitness Tbyte)))    = "executeByteFunction";
    executeFunctionNameRoot (MkValueType (MkPrimitiveType (MkAnyWitness Tchar)))    = "executeCharFunction";
    executeFunctionNameRoot (MkValueType (MkPrimitiveType (MkAnyWitness Tshort)))    = "executeShortFunction";
    executeFunctionNameRoot (MkValueType (MkPrimitiveType (MkAnyWitness Tint)))        = "executeIntFunction";
    executeFunctionNameRoot (MkValueType (MkPrimitiveType (MkAnyWitness Tlong)))    = "executeLongFunction";
    executeFunctionNameRoot (MkValueType (MkPrimitiveType (MkAnyWitness Tfloat)))    = "executeFloatFunction";
    executeFunctionNameRoot (MkValueType (MkPrimitiveType (MkAnyWitness Tdouble)))    = "executeDoubleFunction";
    executeFunctionNameRoot (MkValueType (MkObjectType _))                            = "executeObjectFunction";
    executeFunctionNameRoot (MkValueType (MkArrayType _))                            = "executeObjectFunction";

    executeFunctionName :: Bool -> ReturnType -> MemberName;
    executeFunctionName deferred t = nameFromRoot deferred (executeFunctionNameRoot t);
    
    insExecuteFunction :: Bool -> ReturnType -> Instruction;
    insExecuteFunction deferred retType = insINVOKESTATIC (MkMemberRef executeFunctionClassName 
        (MkMemberNameType (executeFunctionName deferred retType) (MkFunctionType [opaqueAddressType,opaqueAddressType] retType))
        );
    
    addToValueList :: Word8 -> ValueType -> Word8 -> [Instruction];
    addToValueList locVList valType locParam = case valueMachineType valType of
    {
        MkAnyWitness valMType ->
        [
        insLOAD opaqueAddressMachineType locVList,
        insLOAD valMType locParam,
        insAddToValueList valType
        ]
    };
    
    argAdds :: Word8 -> Word8 -> [ValueType] -> [Instruction];
    argAdds _ _ [] = [];
    argAdds locVList locStart (a:as) =
        (addToValueList locVList a locStart) ++
        (argAdds locVList (locStart + (valueSize a)) as);

    methodCode :: Bool -> ClassName -> MethodNameType -> AttributeDefinition;
    methodCode deferred cname method =
        codeAttributeDef args
            (
                [
                insCreateValueList,
                insSTORE opaqueAddressMachineType locVList
                ] ++
            (argAdds locVList 0 ([MkObjectType cname] ++ args)) ++    -- include 'this' as first arg.
                [
                insLOAD MT_A 0,    -- this
                insGETFIELD (MkMemberRef cname (fieldNameTypeForMethod method)),
                insLOAD opaqueAddressMachineType locVList,
                insExecuteFunction deferred ret,
                case returnMachineType ret of {MkAnyWitness vmret -> insRETURN vmret}
                ]
            ) [] [] where
        {
        (MkMemberNameType _ (MkFunctionType args ret)) = method;
        locVList = (valueListSize args) + 1;
        };
    
    makeMethod :: Bool -> ClassName -> MethodNameType -> MethodDefinition;
    makeMethod deferred cname method = MkMethodDefinition
        MkPublic MkVirtual False False False False
        method [methodCode deferred cname method];
    }
