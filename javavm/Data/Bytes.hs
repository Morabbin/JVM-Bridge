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

module Data.Bytes where
    {
    import Data.Word;
    import Data.Int;
    import Data.Bits;
    
    lo8 :: Word16 -> Word8;
    lo8 = fromIntegral;
    
    hi8 :: Word16 -> Word8;
    hi8 x = fromIntegral (shiftR x 8);
    
    lo16 :: Word32 -> Word16;
    lo16 = fromIntegral;
    
    hi16 :: Word32 -> Word16;
    hi16 x = fromIntegral (shiftR x 16);
    
    lo32 :: Word64 -> Word32;
    lo32 = fromIntegral;
    
    hi32 :: Word64 -> Word32;
    hi32 x = fromIntegral (shiftR x 32);
    
    toWord8 :: Int8 -> Word8;
    toWord8 = fromIntegral;
    
    toWord16 :: Int16 -> Word16;
    toWord16 = fromIntegral;
    
    toInt16 :: Word16 -> Int16;
    toInt16 = fromIntegral;
    
    toWord32 :: Int32 -> Word32;
    toWord32 = fromIntegral;
    
    toWord64 :: Int64 -> Word64;
    toWord64 = fromIntegral;
    
    downInt8 :: Int16 -> Maybe Int8;
    downInt8 n = if ((hib == 0) || (hib == 0xFF80)) then (Just (fromIntegral n)) else Nothing where
        {
        hib = n .&. 0xFF80;
        };
    
    upInt16 :: Int8 -> Int16;
    upInt16 = fromIntegral;
    
    upWord16 :: Word8 -> Word16;
    upWord16 = fromIntegral;
    
    downInt16 :: Int32 -> Maybe Int16;
    downInt16 n = if ((hib == 0) || (hib == 0xFFFF8000)) then (Just (fromIntegral n)) else Nothing where
        {
        hib = n .&. 0xFFFF8000;
        };
    }
