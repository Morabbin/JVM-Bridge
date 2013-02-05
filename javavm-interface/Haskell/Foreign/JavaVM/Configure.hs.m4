{-# OPTIONS -Wall -Werror #-}
{-
JVM-Bridge -- bridge from FP languages and others to the Java VM
Copyright (C) 2007 Ashley Yakeley <ashley@semantic.org>

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

module Foreign.JavaVM.Configure where
{
    import Platform.JavaVM;
    import Data.Witness;
    import Data.Word;
    
    type OpaqueAddress = ifelse(ADDRESSSIZE,32,Jint,ADDRESSSIZE,64,Jlong);
    
    opaqueAddressWitness :: PrimitiveType OpaqueAddress;
    opaqueAddressWitness = representative;
    
    opaqueAddressType :: ValueType;
    opaqueAddressType = MkPrimitiveType (MkAnyWitness opaqueAddressWitness);

    opaqueAddressMachineType :: MachineValueType OpaqueAddress;
    opaqueAddressMachineType = ifelse(ADDRESSSIZE,32,MT_I,ADDRESSSIZE,64,MT_L);

    opaqueAddressSize :: Word8;
    opaqueAddressSize = machineValueSize opaqueAddressMachineType;
}
