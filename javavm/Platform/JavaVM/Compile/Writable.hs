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

module Platform.JavaVM.Compile.Writable where
    {
    import Data.Bytes;
    import Data.Word;
    import Data.Int;

    size16 :: [a] -> Word16;
    size16 list = fromIntegral (length list);
    
    size32 :: [a] -> Word32;
    size32 list = fromIntegral (length list);

    -- "Java Class File" Writable
    class JCFWritable a where
        {
        jcfWrite :: a -> [Word8];
        };
    
    instance JCFWritable Word8 where
        {
        jcfWrite x = [x];
        };
    
    instance JCFWritable Word16 where
        {
        jcfWrite x = [hi8 x,lo8 x];
        };
    
    instance JCFWritable Int16 where
        {
        jcfWrite x = jcfWrite (toWord16 x);
        };
    
    instance JCFWritable Word32 where
        {
        jcfWrite x =
            jcfWrite (hi16 x) ++
            jcfWrite (lo16 x) ;
        };
    
    instance JCFWritable Int32 where
        {
        jcfWrite x = jcfWrite (toWord32 x);
        };
    
    instance JCFWritable Word64 where
        {
        jcfWrite x =
            jcfWrite (hi32 x) ++
            jcfWrite (lo32 x) ;
        };
    
    instance JCFWritable Int64 where
        {
        jcfWrite x = jcfWrite (toWord64 x);
        };
    
    instance JCFWritable Float where
        {
        -- NYI: don't know how to write Floats
        jcfWrite _ = error "don't know how to write Floats";
        };
    
    instance JCFWritable Double where
        {
        -- NYI: don't know how to write Doubles
        jcfWrite _ = error "don't know how to write Doubles";
        };
    
    jcfWriteList :: (JCFWritable a) => [a] -> [Word8];
    jcfWriteList [] = [];
    jcfWriteList (a:as) = (jcfWrite a) ++ (jcfWriteList as);
    
    jcfWriteWithSize2 :: (JCFWritable a) => [a] -> [Word8];
    jcfWriteWithSize2 list = (jcfWrite (size16 list)) ++ (jcfWriteList list);
    
    jcfWriteWithSize4 :: (JCFWritable a) => [a] -> [Word8];
    jcfWriteWithSize4 list = (jcfWrite (size32 list)) ++ (jcfWriteList list);
    }
