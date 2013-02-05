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

module Platform.JavaVM.Types.TypeSig where
    {
    import Platform.JavaVM.Types.Text;
    import Platform.JavaVM.Types.Witness;
    import Data.Witness;
    
    type TypeSig    = JavaString;
    
    data ValueType = 
        MkPrimitiveType (AnyWitness PrimitiveType) | 
        MkObjectType ClassName |
        MkArrayType ValueType;
    
    instance Show ValueType where
    {
        show (MkPrimitiveType (MkAnyWitness w))    = show w;
        show (MkObjectType name)    = showJavaString name;
        show (MkArrayType baseType)    = (show baseType) ++ "[]";
    };
    
    valueTypeSig :: ValueType -> TypeSig;
    valueTypeSig (MkPrimitiveType (MkAnyWitness w))    = [primitiveTypeSig w];
    valueTypeSig (MkObjectType name)    = [toJChar 'L'] ++ name ++ [toJChar ';'];
    valueTypeSig (MkArrayType baseType)    = [toJChar '['] ++ (valueTypeSig baseType);

    javaToJNIValueType :: ValueType -> AnyWitness (VType p);
    javaToJNIValueType (MkPrimitiveType (MkAnyWitness w)) = MkAnyWitness (Tprimitive w);
    javaToJNIValueType _            = MkAnyWitness Tref;

    data MemberNameType t = MkMemberNameType
        {
        ntName    :: MemberName,
        ntType    :: t
        };

    instance (Show t) => Show (MemberNameType t) where
        {
        show (MkMemberNameType name t) = (show name) ++ ":" ++ (show t);
        };

    data MemberRef t = MkMemberRef
        {
        rClassName    :: ClassName,
        rNameType    :: MemberNameType t
        };

    type FieldNameType = MemberNameType ValueType;
    type FieldRef = MemberRef ValueType;
    
    data ReturnType = MkVoidType | MkValueType ValueType;

    javaToJNIReturnType :: ReturnType -> AnyWitness (VReturnType p);
    javaToJNIReturnType MkVoidType        = MkAnyWitness Tvoid;
    javaToJNIReturnType (MkValueType t)    = (\(MkAnyWitness w) -> MkAnyWitness (Tvalue w)) (javaToJNIValueType t);
    
    instance Show ReturnType where
        {
        show MkVoidType = "void";
        show (MkValueType vt) = show vt;
        };
    
    returnTypeSig :: ReturnType -> TypeSig;
    returnTypeSig MkVoidType = [toJChar 'V'];
    returnTypeSig (MkValueType v) = valueTypeSig v;
    
    data FunctionType = MkFunctionType
        {
        ftArgs        :: [ValueType],
        ftReturns    :: ReturnType
        };
    
    instance Show FunctionType where
        {
        show (MkFunctionType args returns) = (show returns) ++ " (" ++ (show args) ++ ")";
        };
    
    argumentsTypeSig :: [ValueType] -> TypeSig;
    argumentsTypeSig args = concat (fmap valueTypeSig args);
    
    functionTypeSig :: FunctionType -> TypeSig;
    functionTypeSig (MkFunctionType args ret) = [toJChar '('] ++ (argumentsTypeSig args) ++ [toJChar ')'] ++ (returnTypeSig ret);

    type MethodNameType = MemberNameType FunctionType;
    type MethodRef = MemberRef FunctionType;
    }
