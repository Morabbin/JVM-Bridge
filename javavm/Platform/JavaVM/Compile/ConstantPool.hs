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

module Platform.JavaVM.Compile.ConstantPool where
    {
    import Platform.JavaVM.Compile.Format;
    import Platform.JavaVM.Types;
    import Data.List;
    
    addToNub :: (Eq a) => [a] -> a -> ([a],Int);
    addToNub pool c = case (elemIndex c pool) of
        {
        Nothing -> (pool ++ [c],length pool);
        Just i -> (pool,i);
        };
    
    data Pooler a = MkPooler ([Constant] -> ([Constant],a));
    
    runPooler :: Pooler a -> a;
    runPooler (MkPooler p) = snd (p []);

    getPool :: Pooler [Constant];
    getPool = MkPooler (\pool -> (pool,pool));

    instance Monad Pooler where
        {
        (MkPooler amap) >>= bf = MkPooler (\pool -> let
            {
            (poolA,a) = amap pool;
            (MkPooler bmap) = bf a;
            }
            in bmap poolA);

        return a = MkPooler (\pool -> (pool,a));
        };
    
    -- index will be one-based, as per Platform.JavaVM.Compile. spec.
    addConstant :: Constant -> Pooler Index;
    addConstant c = MkPooler(\pool -> (\(newPool,i) -> (newPool,fromIntegral (i+1))) (addToNub pool c));
    
    addIntegerConstant :: Jint -> Pooler Index;
    addIntegerConstant i = addConstant (MkIntegerConstant i);
    
    addFloatConstant :: Jfloat -> Pooler Index;
    addFloatConstant i = addConstant (MkFloatConstant i);
    
    addLongConstant :: Jlong -> Pooler Index;
    addLongConstant i = addConstant (MkLongConstant i);
    
    addDoubleConstant :: Jdouble -> Pooler Index;
    addDoubleConstant i = addConstant (MkDoubleConstant i);
    
    addTextConstant :: JavaString -> Pooler Index;
    addTextConstant str = addConstant (MkUTF8Constant (encodeUTF8 str));    
    
    addStringConstant :: JavaString -> Pooler Index;
    addStringConstant str = do
        {
        textIndex <- addTextConstant str;
        addConstant (MkStringConstant textIndex);
        };    
    
    addFieldNameAndTypeConstant :: FieldNameType -> Pooler Index;
    addFieldNameAndTypeConstant (MkMemberNameType cname ctype) = do
        {
        nameIndex <- addTextConstant cname;
        typeIndex <- addTextConstant (valueTypeSig ctype);
        addConstant (MkNameAndTypeConstant nameIndex typeIndex);
        };
    
    addMethodNameAndTypeConstant :: MethodNameType -> Pooler Index;
    addMethodNameAndTypeConstant (MkMemberNameType cname ctype) = do
        {
        nameIndex <- addTextConstant cname;
        typeIndex <- addTextConstant (functionTypeSig ctype);
        addConstant (MkNameAndTypeConstant nameIndex typeIndex);
        };
    
    addClassConstant :: ClassName -> Pooler Index;
    addClassConstant str = do
        {
        textIndex <- addTextConstant str;
        addConstant (MkClassConstant textIndex);
        };    
    
    addMethodRefConstant :: MethodRef -> Pooler Index;
    addMethodRefConstant (MkMemberRef cname mnametype) = do
        {
        classIndex <- addClassConstant cname;
        nameTypeIndex <- addMethodNameAndTypeConstant mnametype;
        addConstant (MkMethodRefConstant classIndex nameTypeIndex);
        };
    
    addInterfaceMethodRefConstant :: MethodRef -> Pooler Index;
    addInterfaceMethodRefConstant (MkMemberRef cname mnametype) = do
        {
        classIndex <- addClassConstant cname;
        nameTypeIndex <- addMethodNameAndTypeConstant mnametype;
        addConstant (MkInterfaceMethodRefConstant classIndex nameTypeIndex);
        };
    
    addFieldRefConstant :: FieldRef -> Pooler Index;
    addFieldRefConstant (MkMemberRef cname fnametype) = do
        {
        classIndex <- addClassConstant cname;
        nameTypeIndex <- addFieldNameAndTypeConstant fnametype;
        addConstant (MkFieldRefConstant classIndex nameTypeIndex);
        };
    }
