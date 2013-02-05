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

module Platform.JavaVM.Compile.Attribute where
    {
    import Platform.JavaVM.Compile.CodeInstruction;
    import Platform.JavaVM.Compile.ConstantPool;
    import Platform.JavaVM.Compile.Format;
    import Platform.JavaVM.Compile.Writable;
    import Platform.JavaVM.Types;
    import Control.Monad;
    import Data.Word;
    
    data AttributeDefinition = MkAttributeDefinition
        {
        attrTypeName    :: JavaString,
        attrData        :: Pooler [Word8]
        };
    
    attributeDefToRaw :: AttributeDefinition -> Pooler RawAttribute;
    attributeDefToRaw attr = do
        {
        typeIndex <- addTextConstant (attrTypeName attr);
        ad <- attrData attr;
        return (MkRawAttribute typeIndex ad);
        };
    
    syntheticAttributeDef :: JavaString -> [Word8] -> AttributeDefinition;
    syntheticAttributeDef typeName aData = MkAttributeDefinition typeName (return aData);
    
    data LineNumberEntry = MkLineNumberEntry
        {
        lnStartPC        :: Word16,
        lnLineNumber    :: Word16
        };
    
    instance JCFWritable LineNumberEntry where
        {
        jcfWrite (MkLineNumberEntry spc ln) = (jcfWrite spc) ++ (jcfWrite ln);
        };
    
    lineNumberEntryAttributeDef :: [LineNumberEntry] -> AttributeDefinition;
    lineNumberEntryAttributeDef table = MkAttributeDefinition
        [0x4C,0x69,0x6E,0x65,0x4E,0x75,0x6D,0x62,0x65,0x72,0x54,0x61,0x62,0x6C,0x65]
        (return (jcfWriteList table));
    
    data ExceptionEntry = MkExceptionEntry
        {
        exStartPC    :: Word16,
        exEndPC        :: Word16,
        exHandlerPC    :: Word16,
        exCatchType    :: Word16    -- ?
        };
    
    instance JCFWritable ExceptionEntry where
        {
        jcfWrite (MkExceptionEntry spc epc hpc ct) =
            (jcfWrite spc)    ++
            (jcfWrite epc)    ++
            (jcfWrite hpc)    ++
            (jcfWrite ct)    ;
        };
    
    codeAttributeDef :: [ValueType] -> CodeBlock -> [ExceptionEntry] -> [AttributeDefinition] -> AttributeDefinition;
    codeAttributeDef args code exceptions attrs = MkAttributeDefinition
        [0x43,0x6F,0x64,0x65]
        (do
            {            
            attrBytes <- liftM concat (mapM (\attr -> do
                {
                rawA <- attributeDefToRaw attr;
                return (jcfWrite rawA);
                }) attrs);
            
            codeBytes <- writeCodeBytes code;
            
            return (
                (jcfWrite (maxStackDepth code)) ++
                (jcfWrite (localsSize args code)) ++
                (jcfWrite (size32 codeBytes)) ++
                codeBytes ++
                (jcfWriteWithSize2 exceptions) ++
                (jcfWrite (size16 attrs)) ++
                attrBytes);
            });
    
    addClassConstants :: [ClassName] -> Pooler [Word8];
    addClassConstants [] = return [];
    addClassConstants (c:cs) = do
        {
        index <- addClassConstant c;
        rest <- addClassConstants cs;
        return ((jcfWrite index) ++ rest);
        };
    
    exceptionsAttributeDef :: [ClassName] -> AttributeDefinition;
    exceptionsAttributeDef classnames = MkAttributeDefinition
        [0x45,0x78,0x63,0x65,0x70,0x74,0x69,0x6F,0x6E,0x73]
        (do
            {
            classBytes <- addClassConstants classnames;
            return (
                (jcfWrite (size16 classnames)) ++
                classBytes);
            });
    }
