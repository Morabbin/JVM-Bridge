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

module Platform.JavaVM.Compile.Format where
    {
    import Platform.JavaVM.Compile.Writable;
    import Platform.JavaVM.Types;
    import Data.Word;
    import Data.Bits;
    
    data AccessFlags = MkAccessFlags Word16;
    
    afNone :: AccessFlags;
    afNone            = MkAccessFlags 0;
    
    afPublic :: AccessFlags;
    afPublic        = MkAccessFlags 0x0001;
    afPrivate :: AccessFlags;
    afPrivate        = MkAccessFlags 0x0002;
    afProtected :: AccessFlags;
    afProtected        = MkAccessFlags 0x0004;

    afStatic :: AccessFlags;
    afStatic        = MkAccessFlags 0x0008;

    afFinal :: AccessFlags;
    afFinal            = MkAccessFlags 0x0010;

    afSynchronized :: AccessFlags;
    afSynchronized    = MkAccessFlags 0x0020;
    afNative :: AccessFlags;
    afNative        = MkAccessFlags 0x0100;

    afSuper :: AccessFlags;
    afSuper            = MkAccessFlags 0x0020;

    afVolatile :: AccessFlags;
    afVolatile        = MkAccessFlags 0x0040;
    afTransient :: AccessFlags;
    afTransient        = MkAccessFlags 0x0080;

    afInterface :: AccessFlags;
    afInterface        = MkAccessFlags 0x0200;
    afAbstract :: AccessFlags;
    afAbstract        = MkAccessFlags 0x0400;
    
    flagOr :: AccessFlags -> AccessFlags -> AccessFlags;
    flagOr (MkAccessFlags a) (MkAccessFlags b) = (MkAccessFlags (a .|. b));

    instance JCFWritable AccessFlags where
        {
        jcfWrite (MkAccessFlags flags) = jcfWrite flags;
        };
    
    type Index = Word16;
    
    data Constant =
        MkClassConstant                    Index        | -- name
        MkFieldRefConstant                Index Index    | -- class (name and type)
        MkMethodRefConstant                Index Index    | -- class (name and type)
        MkInterfaceMethodRefConstant    Index Index    | -- class (name and type)
        MkStringConstant                Index        | -- contents
        MkIntegerConstant                Jint        |
        MkFloatConstant                    Jfloat        |
        MkLongConstant                    Jlong        |
        MkDoubleConstant                Jdouble        |
        MkNameAndTypeConstant            Index Index    | -- name type
        MkUTF8Constant                    [Word8]        ;
    
    instance Eq Constant where
        {
        (MkClassConstant a)                        == (MkClassConstant b)                        = a == b;
        (MkFieldRefConstant ac ant)                == (MkFieldRefConstant bc bnt)                = (ac == bc) && (ant == bnt);
        (MkMethodRefConstant ac ant)            == (MkMethodRefConstant bc bnt)                = (ac == bc) && (ant == bnt);
        (MkInterfaceMethodRefConstant ac ant)    == (MkInterfaceMethodRefConstant bc bnt)    = (ac == bc) && (ant == bnt);
        (MkStringConstant a)                    == (MkStringConstant b)                        = a == b;
        (MkIntegerConstant a)                    == (MkIntegerConstant b)                    = a == b;
        (MkFloatConstant a)                        == (MkFloatConstant b)                        = a == b;
        (MkLongConstant a)                        == (MkLongConstant b)                        = a == b;
        (MkDoubleConstant a)                    == (MkDoubleConstant b)                        = a == b;
        (MkNameAndTypeConstant an at)            == (MkNameAndTypeConstant bn bt)            = (an == bn) && (at == bt);
        (MkUTF8Constant a)                        == (MkUTF8Constant b)                        = a == b;
        _ == _ = False;
        };
    
    instance JCFWritable Constant where
        {
        jcfWrite (MkUTF8Constant                c)        = [ 1] ++ (jcfWriteWithSize2 c);
        jcfWrite (MkIntegerConstant                v)        = [ 3] ++ (jcfWrite v);
        jcfWrite (MkFloatConstant                v)        = [ 4] ++ (jcfWrite v);
        jcfWrite (MkLongConstant                v)        = [ 5] ++ (jcfWrite v);
        jcfWrite (MkDoubleConstant                v)        = [ 6] ++ (jcfWrite v);
        jcfWrite (MkClassConstant                i)        = [ 7] ++ (jcfWrite i);
        jcfWrite (MkStringConstant                s)        = [ 8] ++ (jcfWrite s);
        jcfWrite (MkFieldRefConstant            c nt)    = [ 9] ++ (jcfWrite c) ++ (jcfWrite nt);
        jcfWrite (MkMethodRefConstant            c nt)    = [10] ++ (jcfWrite c) ++ (jcfWrite nt);
        jcfWrite (MkInterfaceMethodRefConstant    c nt)    = [11] ++ (jcfWrite c) ++ (jcfWrite nt);
        jcfWrite (MkNameAndTypeConstant            n t)    = [12] ++ (jcfWrite n) ++ (jcfWrite t);
        };
    
    data RawAttribute = MkRawAttribute
        {
        attributeType    :: Index,
        attributeData    :: [Word8]
        };
    
    instance JCFWritable RawAttribute where
        {
        jcfWrite (MkRawAttribute aType aData) =
            (jcfWrite aType) ++
            (jcfWriteWithSize4 aData);
        };
    
    data RawMember = MkRawMember
        {
        rmAccess        :: AccessFlags,
        rmName            :: Index,
        rmType            :: Index,
        rmAttributes    :: [RawAttribute]
        };
    
    instance JCFWritable RawMember where
        {
        jcfWrite (MkRawMember access name mType attributes) =
            (jcfWrite access) ++
            (jcfWrite name) ++
            (jcfWrite mType) ++
            (jcfWriteWithSize2 attributes);
        };
    
    data RawClass = MkRawClass
        {
        rcConstants        :: [Constant],
        rcName            :: Index,
        rcAccess        :: AccessFlags,
        rcSuper            :: Index,
        rcInterfaces    :: [Index],
        rcFields        :: [RawMember],
        rcMethods        :: [RawMember],
        rcAttributes    :: [RawAttribute]
        };
    
    instance JCFWritable RawClass where
        {
        jcfWrite (MkRawClass constants name access super interfaces fields methods attributes) =
            [0xCA,0xFE,0xBA,0xBE] ++
            [0x00,0x03] ++
            [0x00,0x2D] ++
            (jcfWrite ((size16 constants) + 1)) ++
            (jcfWriteList constants) ++
            (jcfWrite access) ++
            (jcfWrite name) ++
            (jcfWrite super) ++
            (jcfWriteWithSize2 interfaces) ++
            (jcfWriteWithSize2 fields) ++
            (jcfWriteWithSize2 methods) ++
            (jcfWriteWithSize2 attributes);
        };
    }
