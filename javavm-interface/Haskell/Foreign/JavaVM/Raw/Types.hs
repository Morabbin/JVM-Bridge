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

module Foreign.JavaVM.Raw.Types where
    {
    import Foreign;
    -- import Data.Word;
    -- import Data.Int;

    type RawVoid    = ();        -- void
    type RawBoolean    = Word8;    -- HsWord8    = unsigned char            = unsigned char        = jboolean
    type RawByte    = Int8;        -- HsInt8    = signed char            = signed char        = jbyte
    type RawChar    = Word16;    -- HsWord16    = unsigned short        = unsigned short    = jchar
    type RawShort    = Int16;    -- HsInt16    = signed short            = short                = jshort
    type RawInt        = Int32;    -- HsInt32    = signed int            = long                = jint
    type RawLong    = Int64;    -- HsInt64    = signed long long int    = long long            = jlong
    type RawFloat    = Float;    -- HsFloat    = float                    = float                = jfloat
    type RawDouble    = Double;    -- HsDouble    = double                = double            = jdouble

    type RawSize    = RawInt;

    rawFalse :: RawBoolean;
    rawFalse = 0;
    
    rawTrue :: RawBoolean;
    rawTrue = 1;

    newtype RawGlobalRef    = MkRawGlobalRef (Ptr ());
    newtype RawLocalRef        = MkRawLocalRef (Ptr ());
    }
