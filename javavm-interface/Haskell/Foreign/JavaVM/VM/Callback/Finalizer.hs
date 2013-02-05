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

module Foreign.JavaVM.VM.Callback.Finalizer
    (
    finalizerMethod
    ) where
    {
    import Foreign.JavaVM.VM.Callback.Field;
    import Foreign.JavaVM.Configure;
    import Platform.JavaVM;
    
    throwableClassName :: ClassName;
    throwableClassName = "java/lang/Throwable";
    
    insFreeFunction :: Bool -> Instruction;
    insFreeFunction deferred = insINVOKESTATIC (MkMemberRef executeFunctionClassName 
        (MkMemberNameType (nameFromRoot deferred "freeFunction") (MkFunctionType [opaqueAddressType] MkVoidType))
        );
    
    freeField :: Bool -> FieldRef -> [Instruction];
    freeField deferred fref = 
        [
        insLOAD MT_A 0,    -- this
        insGETFIELD fref,
        insFreeFunction deferred
        ];
    
    finalizerFreeFields :: Bool -> ClassName -> [FieldNameType] -> [Instruction];
    finalizerFreeFields _ _ [] = [];
    finalizerFreeFields deferred classname (f:fs) = (freeField deferred (MkMemberRef classname f)) ++
        (finalizerFreeFields deferred classname fs);
    
    finalizerCode :: Bool -> ClassName -> ClassName -> [FieldNameType] -> AttributeDefinition;
    finalizerCode deferred classname superclassname fieldNameTypes = codeAttributeDef []
        (
            [
            insLOAD MT_A 0,    -- this
            insINVOKESPECIAL (MkMemberRef superclassname objectFinalizerNameType) -- call superclass finalizer
            ] ++
            (finalizerFreeFields deferred classname fieldNameTypes) ++
            [
            insRETURN MR_Void
            ]
        ) [] [];
    
    finalizerThrowables :: AttributeDefinition;
    finalizerThrowables = exceptionsAttributeDef [throwableClassName];
    
    finalizerMethod :: Bool -> ClassName -> ClassName -> [MethodNameType] -> MethodDefinition;
    finalizerMethod deferred classname superclassname methods = MkMethodDefinition
        MkProtected MkVirtual False False False False
        objectFinalizerNameType
        [finalizerCode deferred classname superclassname (fmap fieldNameTypeForMethod methods),
        finalizerThrowables];
    }
