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

module Foreign.JavaVM.VM.Callback.Field where
    {
    import Foreign.JavaVM.Configure;
    import Platform.JavaVM;

    executeFunctionClassName :: ClassName;
    executeFunctionClassName = "org/semantic/jvmbridge/ExecuteFunction";

    nameFromRoot :: Bool -> JavaString -> MemberName;
    nameFromRoot deferred s = s ++ (if deferred then "Deferred" else "Now");
    
    allowedIdentifierChar :: Jchar -> Bool;
    allowedIdentifierChar c = (c /= toJChar ';') && (c /= toJChar '/');
    
    cleanChar :: Jchar -> Jchar;
    cleanChar x = if (allowedIdentifierChar x) then x else (toJChar '_');
    
    cleanIdentifier :: JavaString -> JavaString;
    cleanIdentifier = fmap cleanChar;
    
    fieldNameForMethod :: MethodNameType -> MemberName;
    fieldNameForMethod (MkMemberNameType name (MkFunctionType args _)) =
        "mh_" ++ name ++ "_" ++ (cleanIdentifier (argumentsTypeSig args));
    
    fieldNameTypeForMethod :: MethodNameType -> FieldNameType;
    fieldNameTypeForMethod method = MkMemberNameType (fieldNameForMethod method) opaqueAddressType;
    
    fieldDefinitionForMethod :: MethodNameType -> FieldDefinition;
    fieldDefinitionForMethod method = 
        MkFieldDefinition MkPrivate MkVirtual False False False (fieldNameTypeForMethod method) [];
    }
