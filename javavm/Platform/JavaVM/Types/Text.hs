{-# OPTIONS -fno-warn-orphans #-}
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

module Platform.JavaVM.Types.Text where
    {
    import Platform.JavaVM.Types.Types;
    import Data.String;
    import Data.Bytes;
    import Data.Word;
    import Data.Bits;

    {--
    0000 0000-0000 007F   0xxxxxxx
    0000 0080-0000 07FF   110xxxxx 10xxxxxx
    0000 0800-0000 FFFF   1110xxxx 10xxxxxx 10xxxxxx
    0001 0000-001F FFFF   11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    0020 0000-03FF FFFF   111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
    0400 0000-7FFF FFFF   1111110x 10xxxxxx ... 10xxxxxx
    --}
    
    type JavaString = [Jchar];

    type ClassName    = JavaString;
    type MemberName    = JavaString;
    
    toJChar :: Char -> Jchar;
    toJChar c = fromIntegral (fromEnum c);
    
    showJChar :: Jchar -> Char;
    showJChar c = toEnum (fromIntegral c);
    
    instance IsString JavaString where
        {
        fromString = fmap toJChar;
        };
    
    showJavaString :: JavaString -> String;
    showJavaString = fmap showJChar;
    
    showUTF8 :: [Word8] -> String;
    showUTF8 = fmap (\c -> showJChar (upWord16 c));
    
    shiftToByte :: Int -> Jchar -> Word8;
    shiftToByte i c = lo8 (shiftR c i);
    
    trailingByte :: Int -> Jchar -> Word8;
    trailingByte i c = 0x80 .|. (0x3F .&. (shiftToByte i c));
    
    encodeSingleUTF8 :: Jchar -> [Word8];
    encodeSingleUTF8 c = if (c < 0x0080) then [lo8 c]
        else if (c < 0x800) then
            [
            0xC0 .|. (shiftToByte 6 c),
            trailingByte 0 c
            ]
        else
            [
            0xE0 .|. (shiftToByte 12 c),
            trailingByte 6 c,
            trailingByte 0 c
            ];

    -- note: doesn't handle UTF-16 surrogates correctly
    encodeUTF8 :: JavaString -> [Word8];
    encodeUTF8 s = foldr prependOne [] s where
        {
        prependOne c bytes = (encodeSingleUTF8 c) ++ bytes;
        };
    }
