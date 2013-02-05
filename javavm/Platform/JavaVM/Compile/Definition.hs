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

module Platform.JavaVM.Compile.Definition where
    {
    import Platform.JavaVM.Compile.Attribute;
    import Platform.JavaVM.Compile.ConstantPool;
    import Platform.JavaVM.Compile.Format;
    import Platform.JavaVM.Compile.Writable;
    import Platform.JavaVM.Types;
    
    data Access = MkPublic | MkPrivate | MkProtected | MkPackage;
    
    accessToRaw :: Access -> AccessFlags;
    accessToRaw MkPublic    = afPublic;
    accessToRaw MkPrivate    = afPrivate;
    accessToRaw MkProtected    = afProtected;
    accessToRaw MkPackage    = afNone;
    
    publicToRaw :: Bool -> AccessFlags;
    publicToRaw True        = afPublic;
    publicToRaw False    = afNone;
    
    data Staticness = MkStatic | MkVirtual;
    
    staticnessToRaw :: Staticness -> AccessFlags;
    staticnessToRaw MkStatic    = afStatic;
    staticnessToRaw MkVirtual    = afNone;
    
    finalToRaw :: Bool -> AccessFlags;
    finalToRaw True        = afFinal;
    finalToRaw False    = afNone;
    
    volatileToRaw :: Bool -> AccessFlags;
    volatileToRaw True    = afVolatile;
    volatileToRaw False    = afNone;
    
    transientToRaw :: Bool -> AccessFlags;
    transientToRaw True        = afTransient;
    transientToRaw False    = afNone;
    
    synchronizedToRaw :: Bool -> AccessFlags;
    synchronizedToRaw True    = afSynchronized;
    synchronizedToRaw False    = afNone;
    
    nativeToRaw :: Bool -> AccessFlags;
    nativeToRaw True    = afNative;
    nativeToRaw False    = afNone;
    
    interfaceToRaw :: Bool -> AccessFlags;
    interfaceToRaw True        = afInterface;
    interfaceToRaw False    = afNone;
    
    abstractToRaw :: Bool -> AccessFlags;
    abstractToRaw True    = afAbstract;
    abstractToRaw False    = afNone;
    
    data FieldDefinition = MkFieldDefinition
        {
        fAccess        :: Access,
        fStatic        :: Staticness,
        fFinal        :: Bool,
        fVolatile    :: Bool,
        fTransient    :: Bool,
        fNameType    :: FieldNameType,
        fAttributes    :: [AttributeDefinition]
        };
    
    fieldToRaw :: FieldDefinition -> Pooler RawMember;
    fieldToRaw (MkFieldDefinition acc stat fin vol trans (MkMemberNameType fname ftype) attrs) = do
        {
        let {
        aflags = flagOr (accessToRaw acc)
            (flagOr (staticnessToRaw stat)
                (flagOr (finalToRaw fin)
                    (flagOr (volatileToRaw vol)
                        (transientToRaw trans)
                    )))};
        rname <- addTextConstant fname;
        rtype <- addTextConstant (valueTypeSig ftype);
        rattrs <- mapM attributeDefToRaw attrs;
        return (MkRawMember aflags rname rtype rattrs);
        };
    
    data MethodDefinition = MkMethodDefinition
        {
        mAccess            :: Access,
        mStatic            :: Staticness,
        mFinal            :: Bool,
        mSynchronized    :: Bool,
        mNative            :: Bool,
        mAbstract        :: Bool,
        mNameType        :: MethodNameType,
        mAttributes        :: [AttributeDefinition]
        };
    
    methodToRaw :: MethodDefinition -> Pooler RawMember;
    methodToRaw (MkMethodDefinition acc stat fin syn nat abst (MkMemberNameType mname mtype) attrs) = do
        {
        let {
        aflags = flagOr (accessToRaw acc)
            (flagOr (staticnessToRaw stat)
                (flagOr (finalToRaw fin)
                    (flagOr (synchronizedToRaw syn)
                        (flagOr (nativeToRaw nat)
                            (abstractToRaw abst)
                    ))))};
        rname <- addTextConstant mname;
        rtype <- addTextConstant (functionTypeSig mtype);
        rattrs <- mapM attributeDefToRaw attrs;
        return (MkRawMember aflags rname rtype rattrs);
        };
    
    data ClassDefinition = MkClassDefinition
        {
        classDefName        :: ClassName,
        classDefIsPublic    :: Bool,
        classDefIsFinal        :: Bool,
        classDefIsInterface    :: Bool,
        classDefIsAbstract    :: Bool,
        classDefSuper        :: ClassName,
        classDefInterfaces    :: [ClassName],
        classDefFields        :: [FieldDefinition],
        classDefMethods        :: [MethodDefinition],
        classDefAttributes    :: [AttributeDefinition]
        };
    
    classDefToRaw1 :: ClassDefinition -> Pooler RawClass;
    classDefToRaw1 (MkClassDefinition cname cispub cisfin cisint cisabs csuper cints cfields cmeths cattrs) = do
        {
        rname <- addClassConstant cname;
        let {
        rflags = flagOr afSuper
            (flagOr (publicToRaw cispub)
                (flagOr (finalToRaw cisfin)
                    (flagOr (interfaceToRaw cisint)
                        (abstractToRaw cisabs)
                    )))};
        rsuper <- addClassConstant csuper;
        rints <- mapM addClassConstant cints;
        rfields <- mapM fieldToRaw cfields;
        rmeths <- mapM methodToRaw cmeths;
        rattrs <- mapM attributeDefToRaw cattrs;
        constPool <- getPool;    -- must be done last, of course
        return (MkRawClass constPool rname rflags rsuper rints rfields rmeths rattrs);
        };

    classDefToRaw :: ClassDefinition -> RawClass;
    classDefToRaw cls = runPooler (classDefToRaw1 cls);
    
    instance JCFWritable ClassDefinition where
        {
        jcfWrite cls = jcfWrite (classDefToRaw cls);
        };
    
    data InterfaceDefinition = MkInterfaceDefinition
        {
        intName            :: ClassName,
        intPublic        :: Bool,
        intInterfaces    :: [ClassName],
        intMethods        :: [MethodDefinition],
        intAttributes    :: [AttributeDefinition]
        };
    
    interfaceDefToClassDef :: InterfaceDefinition -> ClassDefinition;
    interfaceDefToClassDef (MkInterfaceDefinition name pub ints meths attrs) =
        MkClassDefinition name pub False True True "java/lang/Object" ints [] meths attrs;
    
    instance JCFWritable InterfaceDefinition where
        {
        jcfWrite cls = jcfWrite (interfaceDefToClassDef cls);
        };
    }
