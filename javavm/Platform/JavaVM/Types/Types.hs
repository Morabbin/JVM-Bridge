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

module Platform.JavaVM.Types.Types where
{
    import Data.Word;
    import Data.Int;
    
    type Jvoid        = ();        -- void
    type Jboolean    = Bool;        -- HsBool    = int                    != unsigned char    = jboolean
    type Jbyte        = Int8;        -- HsInt8    = signed char            = signed char        = jbyte
    type Jchar        = Word16;    -- HsWord16    = unsigned short        = unsigned short    = jchar
    type Jshort        = Int16;    -- HsInt16    = signed short            = short                = jshort
    type Jint        = Int32;    -- HsInt32    = signed int            = long                = jint
    type Jlong        = Int64;    -- HsInt64    = signed long long int    = long long            = jlong
    type Jfloat        = Float;    -- HsFloat    = float                    = float                = jfloat
    type Jdouble    = Double;    -- HsDouble    = double                = double            = jdouble
    
    jVoid :: Jvoid;
    jVoid = ();
    
    jFalse :: Jboolean;
    jFalse = False;
    
    jTrue :: Jboolean;
    jTrue = True;
}
