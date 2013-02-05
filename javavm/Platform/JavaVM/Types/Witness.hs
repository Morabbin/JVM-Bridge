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

module Platform.JavaVM.Types.Witness where
{
    import Platform.JavaVM.Types.Types;
    import Platform.JavaVM.Types.Text;
    import Data.Witness;
    import Data.Maybe;
    
    data PrimitiveType t where
    {
        Tboolean :: PrimitiveType Jboolean;
        Tbyte :: PrimitiveType Jbyte;
        Tchar :: PrimitiveType Jchar;
        Tshort :: PrimitiveType Jshort;
        Tint:: PrimitiveType Jint;
        Tlong :: PrimitiveType Jlong;
        Tfloat :: PrimitiveType Jfloat;
        Tdouble :: PrimitiveType Jdouble;
    };
    
    instance Show (PrimitiveType t) where
    {
        show Tboolean    = "boolean";
        show Tbyte        = "byte";
        show Tchar        = "char";
        show Tshort        = "short";
        show Tint        = "int";
        show Tlong        = "long";
        show Tfloat        = "float";
        show Tdouble    = "double";
    };
    
    primitiveTypeSig :: PrimitiveType t -> Jchar;
    primitiveTypeSig Tboolean    = toJChar 'Z';
    primitiveTypeSig Tbyte        = toJChar 'B';
    primitiveTypeSig Tchar        = toJChar 'C';
    primitiveTypeSig Tshort        = toJChar 'S';
    primitiveTypeSig Tint        = toJChar 'I';
    primitiveTypeSig Tlong        = toJChar 'J';
    primitiveTypeSig Tfloat        = toJChar 'F';
    primitiveTypeSig Tdouble    = toJChar 'D';
    
    instance Eq1 PrimitiveType where
    {
        equals1 pa pb = isJust (matchWitness pa pb);
    };
    
    instance Representative PrimitiveType where
    {
        getRepWitness Tboolean = MkRepWitness;
        getRepWitness Tbyte    = MkRepWitness;
        getRepWitness Tchar    = MkRepWitness;
        getRepWitness Tshort   = MkRepWitness;
        getRepWitness Tint     = MkRepWitness;
        getRepWitness Tlong    = MkRepWitness;
        getRepWitness Tfloat   = MkRepWitness;
        getRepWitness Tdouble  = MkRepWitness;
    };
    
    instance SimpleWitness PrimitiveType where
    {
        matchWitness Tboolean Tboolean = Just MkEqualType;
        matchWitness Tbyte Tbyte = Just MkEqualType;
        matchWitness Tchar Tchar = Just MkEqualType;
        matchWitness Tshort Tshort = Just MkEqualType;
        matchWitness Tint Tint = Just MkEqualType;
        matchWitness Tlong Tlong = Just MkEqualType;
        matchWitness Tfloat Tfloat = Just MkEqualType;
        matchWitness Tdouble Tdouble = Just MkEqualType;
        matchWitness _ _ = Nothing;
    };
    
    instance Is PrimitiveType Jboolean where
    {
        representative = Tboolean;
    };
    
    instance Is PrimitiveType Jbyte where
    {
        representative = Tbyte;
    };
    
    instance Is PrimitiveType Jchar where
    {
        representative = Tchar;
    };
    
    instance Is PrimitiveType Jshort where
    {
        representative = Tshort;
    };
    
    instance Is PrimitiveType Jint where
    {
        representative = Tint;
    };
    
    instance Is PrimitiveType Jlong where
    {
        representative = Tlong;
    };
    
    instance Is PrimitiveType Jfloat where
    {
        representative = Tfloat;
    };
    
    instance Is PrimitiveType Jdouble where
    {
        representative = Tdouble;
    };
    
    newtype VRef p = MkVRef p;

    data VType p t where
    {
        Tprimitive :: PrimitiveType t -> VType p t;
        Tref :: VType p (VRef p);
    };
    
    instance Eq1 (VType p) where
    {
        equals1 pa pb = isJust (matchWitness pa pb);
    };
    
    instance Representative (VType p) where
    {
        getRepWitness (Tprimitive Tboolean) = MkRepWitness;
        getRepWitness (Tprimitive Tbyte) = MkRepWitness;
        getRepWitness (Tprimitive Tchar) = MkRepWitness;
        getRepWitness (Tprimitive Tshort) = MkRepWitness;
        getRepWitness (Tprimitive Tint) = MkRepWitness;
        getRepWitness (Tprimitive Tlong) = MkRepWitness;
        getRepWitness (Tprimitive Tfloat) = MkRepWitness;
        getRepWitness (Tprimitive Tdouble) = MkRepWitness;
        getRepWitness Tref = MkRepWitness;
    };
    
    instance SimpleWitness (VType p) where
    {
        matchWitness (Tprimitive ta) (Tprimitive tb) = matchWitness ta tb;
        matchWitness Tref Tref = Just MkEqualType;
        matchWitness _ _ = Nothing;
    };
    
    instance Is (VType p) Jboolean where
    {
        representative = Tprimitive Tboolean;
    };
    
    instance Is (VType p) Jbyte where
    {
        representative = Tprimitive Tbyte;
    };
    
    instance Is (VType p) Jchar where
    {
        representative = Tprimitive Tchar;
    };
    
    instance Is (VType p) Jshort where
    {
        representative = Tprimitive Tshort;
    };
    
    instance Is (VType p) Jint where
    {
        representative = Tprimitive Tint;
    };
    
    instance Is (VType p) Jlong where
    {
        representative = Tprimitive Tlong;
    };
    
    instance Is (VType p) Jfloat where
    {
        representative = Tprimitive Tfloat;
    };
    
    instance Is (VType p) Jdouble where
    {
        representative = Tprimitive Tdouble;
    };
    
    instance Is (VType p) (VRef p) where
    {
        representative = Tref;
    };

    data VReturnType p t where
    {
        Tvalue :: VType p t -> VReturnType p t;
        Tvoid :: VReturnType p Jvoid;
    };
    
    instance Eq1 (VReturnType p) where
    {
        equals1 pa pb = isJust (matchWitness pa pb);
    };
    
    instance Representative (VReturnType p) where
    {
        getRepWitness (Tvalue (Tprimitive Tboolean)) = MkRepWitness;
        getRepWitness (Tvalue (Tprimitive Tbyte)) = MkRepWitness;
        getRepWitness (Tvalue (Tprimitive Tchar)) = MkRepWitness;
        getRepWitness (Tvalue (Tprimitive Tshort)) = MkRepWitness;
        getRepWitness (Tvalue (Tprimitive Tint)) = MkRepWitness;
        getRepWitness (Tvalue (Tprimitive Tlong)) = MkRepWitness;
        getRepWitness (Tvalue (Tprimitive Tfloat)) = MkRepWitness;
        getRepWitness (Tvalue (Tprimitive Tdouble)) = MkRepWitness;
        getRepWitness (Tvalue Tref) = MkRepWitness;
        getRepWitness Tvoid = MkRepWitness;
    };
    
    instance SimpleWitness (VReturnType p) where
    {
        matchWitness (Tvalue ta) (Tvalue tb) = matchWitness ta tb;
        matchWitness Tvoid Tvoid = Just MkEqualType;
        matchWitness _ _ = Nothing;
    };
    
    instance Is (VReturnType p) Jboolean where
    {
        representative = Tvalue (Tprimitive Tboolean);
    };
    
    instance Is (VReturnType p) Jbyte where
    {
        representative = Tvalue (Tprimitive Tbyte);
    };
    
    instance Is (VReturnType p) Jchar where
    {
        representative = Tvalue (Tprimitive Tchar);
    };
    
    instance Is (VReturnType p) Jshort where
    {
        representative = Tvalue (Tprimitive Tshort);
    };
    
    instance Is (VReturnType p) Jint where
    {
        representative = Tvalue (Tprimitive Tint);
    };
    
    instance Is (VReturnType p) Jlong where
    {
        representative = Tvalue (Tprimitive Tlong);
    };
    
    instance Is (VReturnType p) Jfloat where
    {
        representative = Tvalue (Tprimitive Tfloat);
    };
    
    instance Is (VReturnType p) Jdouble where
    {
        representative = Tvalue (Tprimitive Tdouble);
    };
    
    instance Is (VReturnType p) (VRef p) where
    {
        representative = Tvalue Tref;
    };
    
    instance Is (VReturnType p) Jvoid where
    {
        representative = Tvoid;
    };
}
