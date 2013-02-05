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

module Foreign.JavaVM.VM.Callback.Constructor
    (
    constructorMethod
    ) where
    {
    import Foreign.JavaVM.VM.Callback.Field;
    import Foreign.JavaVM.Configure;
    import Platform.JavaVM;
    import Data.Witness;
    import Data.Word;
    
    consPutField :: ClassName -> Word8 -> MethodNameType -> [Instruction];
    consPutField cname i method = 
        [
        insLOAD MT_A 0,    -- this
        insLOAD opaqueAddressMachineType i,
        insPUTFIELD (MkMemberRef cname (fieldNameTypeForMethod method))
        ];
    
    consPutFields :: ClassName -> Word8 -> [MethodNameType] -> [Instruction];
    consPutFields _ _ [] = [];
    consPutFields cname locStart (method:ms) =
        (consPutField cname locStart method) ++ (consPutFields cname (locStart + opaqueAddressSize) ms);
    
    superconsLoads  :: [ValueType] -> [Instruction];
    superconsLoads vt = scl 0 vt where
        {
        scl _ [] = [];
        scl i (t:ts) = (case valueMachineType t of {MkAnyWitness vmt -> insLOAD vmt i}) : (scl (i + (valueSize t)) ts);
        };
    
    constructorCode :: ClassName -> ClassName -> [ValueType] -> [MethodNameType] -> AttributeDefinition;
    constructorCode cname supercname superclassconstructor methods = 
        codeAttributeDef superclassconstructor
            (
                [
                insLOAD MT_A 0    -- this
                ] ++
                (superconsLoads superclassconstructor) ++
                [
                insINVOKESPECIAL
                    (MkMemberRef supercname (MkMemberNameType objectConstructorName (MkFunctionType superclassconstructor MkVoidType)))
                ] ++
                (consPutFields cname 1 methods) ++
                [
                insRETURN MR_Void
                ]
            ) [] [];
    
    constructorMethod :: ClassName -> ClassName -> [ValueType] -> [MethodNameType] -> MethodDefinition;
    constructorMethod cname supercname superclassconstructor methods = MkMethodDefinition
        MkPublic MkVirtual False False False False
        (MkMemberNameType objectConstructorName 
        (MkFunctionType ((fmap (const opaqueAddressType) methods) ++ superclassconstructor) MkVoidType))
        [constructorCode cname supercname superclassconstructor methods];
    }
