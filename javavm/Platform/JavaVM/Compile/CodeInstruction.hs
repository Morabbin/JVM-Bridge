{-# OPTIONS -fno-warn-missing-signatures -fno-warn-unused-binds #-}
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

module Platform.JavaVM.Compile.CodeInstruction
    (
    Jaddr(..),
    MachineValueType(..),MachineReturnType(..),
    machineValueSize,machineReturnSize,
    valueMachineType,returnMachineType,
    
    valueSize,
    valueListSize,

    Instruction(),LocalVar,

    insNOP,

    insCONST,
    insLOAD,
    insSTORE,

    insDUP,
    insDUP_X1,
    insDUP_X2,
    insDUP2,
    insDUP2_X1,
    insDUP2_X2,
    insSWAP,

    insRETURN,

    insINVOKEVIRTUAL,
    insINVOKESPECIAL,
    insINVOKESTATIC,
    insINVOKEINTERFACE,

    insGETFIELD,
    insPUTFIELD,

    insNEW,
    insCHECKCAST,

    CodeBlock(),
    writeCodeBytes,
    localsSize,
    maxStackDepth
    ) where
    {
    import Platform.JavaVM.Compile.ConstantPool;
    import Platform.JavaVM.Compile.Format;
    import Platform.JavaVM.Compile.Writable;
    import Platform.JavaVM.Types;
    import Data.Witness;
    import Data.Bytes;
    import Control.Monad;
    import Data.Word;
    import Data.Int;

    data Jaddr = NullJaddr;

    data MachineValueType t where
    {
        MT_I :: MachineValueType Jint;
        MT_L :: MachineValueType Jlong;
        MT_F :: MachineValueType Jfloat;
        MT_D :: MachineValueType Jdouble;
        MT_A :: MachineValueType Jaddr;
    };

    data MachineReturnType t where
    {
        MR_Void :: MachineReturnType ();
        MR_Value :: MachineValueType t -> MachineReturnType t;
    };

    instance SimpleWitness MachineValueType where
    {
        matchWitness MT_I MT_I = Just MkEqualType;
        matchWitness MT_L MT_L = Just MkEqualType;
        matchWitness MT_F MT_F = Just MkEqualType;
        matchWitness MT_D MT_D = Just MkEqualType;
        matchWitness MT_A MT_A = Just MkEqualType;
        matchWitness _ _ = Nothing;
    };

    instance SimpleWitness MachineReturnType where
    {
        matchWitness MR_Void MR_Void = Just MkEqualType;
        matchWitness (MR_Value va) (MR_Value vb) = do
        {
            MkEqualType <- matchWitness va vb;
            return MkEqualType;
        };
        matchWitness _ _ = Nothing;
    };
{-
    instance Eq1 MachineValueType where
    {
        equals1 t1 t2 = isJust (matchWitness t1 t2);
    };

    instance Is MachineValueType Jint where
    {
        representative = MT_I;
    };

    instance Is MachineValueType Jlong where
    {
        representative = MT_I;
    };

    instance Is MachineValueType Jfloat where
    {
        representative = MT_I;
    };

    instance Is MachineValueType Jdouble where
    {
        representative = MT_I;
    };

    instance Is MachineValueType Jaddr where
    {
        representative = MT_I;
    };

    instance Representative MachineValueType where
    {
        getRepWitness MT_I = MkRepWitness;
        getRepWitness MT_L = MkRepWitness;
        getRepWitness MT_F = MkRepWitness;
        getRepWitness MT_D = MkRepWitness;
        getRepWitness MT_A = MkRepWitness;
    };
    
    class (Is MachineValueType (PrimitiveVMType p)) => HasPrimitiveVMType p where
    {
        type PrimitiveVMType p :: *;
        
        toVMType :: p -> PrimitiveVMType p;
    }; 
    
    instance HasPrimitiveVMType 
-}   

    machineValueSize :: MachineValueType t -> Word8;
    machineValueSize MT_I = 1;
    machineValueSize MT_L = 2;
    machineValueSize MT_F = 1;
    machineValueSize MT_D = 2;
    machineValueSize MT_A = 1;

    machineReturnSize :: MachineReturnType t -> Word8;
    machineReturnSize MR_Void = 0;
    machineReturnSize (MR_Value vt) = machineValueSize vt;

    valueMachineType :: ValueType -> AnyWitness MachineValueType;
    valueMachineType (MkPrimitiveType (MkAnyWitness Tboolean)) = MkAnyWitness MT_I;
    valueMachineType (MkPrimitiveType (MkAnyWitness Tbyte))    = MkAnyWitness MT_I;
    valueMachineType (MkPrimitiveType (MkAnyWitness Tchar))    = MkAnyWitness MT_I;
    valueMachineType (MkPrimitiveType (MkAnyWitness Tshort))   = MkAnyWitness MT_I;
    valueMachineType (MkPrimitiveType (MkAnyWitness Tint))     = MkAnyWitness MT_I;
    valueMachineType (MkPrimitiveType (MkAnyWitness Tlong))    = MkAnyWitness MT_L;
    valueMachineType (MkPrimitiveType (MkAnyWitness Tfloat))   = MkAnyWitness MT_F;
    valueMachineType (MkPrimitiveType (MkAnyWitness Tdouble))  = MkAnyWitness MT_D;
    valueMachineType (MkObjectType _)                          = MkAnyWitness MT_A;
    valueMachineType (MkArrayType _)                           = MkAnyWitness MT_A;

    returnMachineType :: ReturnType -> AnyWitness MachineReturnType;
    returnMachineType MkVoidType = MkAnyWitness MR_Void;
    returnMachineType (MkValueType vt) = case valueMachineType vt of
    {
        MkAnyWitness v -> MkAnyWitness (MR_Value v);
    };

    valueSize :: ValueType -> Word8;
    valueSize vt = case valueMachineType vt of
    {
        MkAnyWitness t -> machineValueSize t;
    };

    returnSize :: ReturnType -> Word8;
    returnSize MkVoidType = 0;
    returnSize (MkValueType vt) = valueSize vt;

    valueListSize :: [ValueType] -> Word8;
    valueListSize [] = 0;
    valueListSize (a:as) = (valueSize a) + (valueListSize as);

    type LocalVar = Word8;

    data BasicInstruction =
        MkFixedBasicInstruction         [Word8]                 |
        MkVarFixedBasicInstruction      [Word8] LocalVar Word8  |
        MkClassBasicInstruction         Word8 ClassName         |
        MkMethodBasicInstruction        Word8 MethodRef         |
        MkIntfMethodBasicInstruction    Word8 MethodRef Word8   |
        MkFieldBasicInstruction         Word8 FieldRef          |
        MkIntegerConstBasicInstruction  Jint                    |
        MkFloatConstBasicInstruction    Jfloat                  |
        MkLongConstBasicInstruction     Jlong                   |
        MkDoubleConstBasicInstruction   Jdouble                 ;

    data Instruction = MkInstruction
        {
        insBasic :: BasicInstruction,
        insTakes :: Word16,
        insGives :: Word16
        };

    mkFixedInstruction c t g = MkInstruction (MkFixedBasicInstruction c) t g;
    mkVarFixedInstruction c w r t g = MkInstruction (MkVarFixedBasicInstruction c w r) t g;
    mkIntegerConstInstruction c t g = MkInstruction (MkIntegerConstBasicInstruction c) t g;
    mkFloatConstInstruction c t g = MkInstruction (MkFloatConstBasicInstruction c) t g;
    mkLongConstInstruction c t g = MkInstruction (MkLongConstBasicInstruction c) t g;
    mkDoubleConstInstruction c t g = MkInstruction (MkDoubleConstBasicInstruction c) t g;
    mkClassInstruction opcode c t g = MkInstruction (MkClassBasicInstruction opcode c) t g;


    -- 00 NOP

    insNOP              = mkFixedInstruction    [0x00] 0 0;


    -- 01 - 11 CONST

    insACONST_NULL      = mkFixedInstruction    [0x01] 0 1;

    insACONST :: Jaddr -> Instruction;
    insACONST NullJaddr = insACONST_NULL;

    insICONST_M1        = mkFixedInstruction    [0x02] 0 1;
    insICONST_0         = mkFixedInstruction    [0x03] 0 1;
    insICONST_1         = mkFixedInstruction    [0x04] 0 1;
    insICONST_2         = mkFixedInstruction    [0x05] 0 1;
    insICONST_3         = mkFixedInstruction    [0x06] 0 1;
    insICONST_4         = mkFixedInstruction    [0x07] 0 1;
    insICONST_5         = mkFixedInstruction    [0x08] 0 1;

    insBIPUSH :: Jbyte -> Instruction; -- pushes one
    insBIPUSH (-1)      = insICONST_M1;
    insBIPUSH 0         = insICONST_0;
    insBIPUSH 1         = insICONST_1;
    insBIPUSH 2         = insICONST_2;
    insBIPUSH 3         = insICONST_3;
    insBIPUSH 4         = insICONST_4;
    insBIPUSH 5         = insICONST_5;
    insBIPUSH n8        = mkFixedInstruction    [0x10,toWord8 n8] 0 1;

    insSIPUSH :: Jshort  -> Instruction;    -- pushes one, will use bipush if it can
    insSIPUSH n16        = case (downInt8 n16) of
        {
        Just n8 -> insBIPUSH n8;
        Nothing -> mkFixedInstruction    ([0x11] ++ (jcfWrite n16)) 0 1;
        };
    
    insICONST :: Jint -> Instruction; -- pushes one, will use bipush or sipush if it can
    insICONST n32        = case (downInt16 n32) of
        {
        Just n16 -> insSIPUSH n16;
        Nothing -> mkIntegerConstInstruction n32 0 1;
        };

    insLCONST_0            = mkFixedInstruction    [0x09] 0 2;
    insLCONST_1            = mkFixedInstruction    [0x0A] 0 2;

    insLCONST :: Jlong -> Instruction; -- pushes two
    insLCONST 0            = insLCONST_0;
    insLCONST 1            = insLCONST_1;
    insLCONST n            = mkLongConstInstruction n 0 2;

    insFCONST_0            = mkFixedInstruction    [0x0B] 0 1;
    insFCONST_1            = mkFixedInstruction    [0x0C] 0 1;
    insFCONST_2            = mkFixedInstruction    [0x0D] 0 1;

    insFCONST :: Jfloat -> Instruction; -- pushes one
    insFCONST 0            = insFCONST_0;
    insFCONST 1            = insFCONST_1;
    insFCONST 2            = insFCONST_2;
    insFCONST n            = mkFloatConstInstruction n 0 1;

    insDCONST_0            = mkFixedInstruction    [0x0E] 0 2;
    insDCONST_1            = mkFixedInstruction    [0x0F] 0 2;

    insDCONST :: Jdouble -> Instruction; -- pushes two
    insDCONST 0            = insDCONST_0;
    insDCONST 1            = insDCONST_1;
    insDCONST n            = mkDoubleConstInstruction n 0 2;
    
    insCONST :: MachineValueType t -> t -> Instruction;
    insCONST MT_I = insICONST;
    insCONST MT_L = insLCONST;
    insCONST MT_F = insFCONST;
    insCONST MT_D = insDCONST;
    insCONST MT_A = insACONST;


    -- 12 - 14 LDC

    codeLDC1 :: Index -> [Word8];
    codeLDC1 ind = if (ind < 0x100)
        then ([0x12] ++ [lo8 ind])                -- ldc
        else ([0x13] ++ (jcfWrite ind))    ;        -- ldc_w

    codeLDC2 :: Index -> [Word8];
    codeLDC2 ind = [0x14] ++ (jcfWrite ind);    -- ldc2_w


    -- 15 - 2D LOAD

    mkLoadInstruction :: [Word8] -> LocalVar -> Word8 -> Instruction;
    mkLoadInstruction bytes locid size = mkVarFixedInstruction bytes locid size 0 (upWord16 size);

    insILOAD_0            = mkLoadInstruction    [0x1A] 0 1;
    insILOAD_1            = mkLoadInstruction    [0x1B] 1 1;
    insILOAD_2            = mkLoadInstruction    [0x1C] 2 1;
    insILOAD_3            = mkLoadInstruction    [0x1D] 3 1;

    insLLOAD_0            = mkLoadInstruction    [0x1E] 0 2;
    insLLOAD_1            = mkLoadInstruction    [0x1F] 1 2;
    insLLOAD_2            = mkLoadInstruction    [0x20] 2 2;
    insLLOAD_3            = mkLoadInstruction    [0x21] 3 2;

    insFLOAD_0            = mkLoadInstruction    [0x22] 0 1;
    insFLOAD_1            = mkLoadInstruction    [0x23] 1 1;
    insFLOAD_2            = mkLoadInstruction    [0x24] 2 1;
    insFLOAD_3            = mkLoadInstruction    [0x25] 3 1;

    insDLOAD_0            = mkLoadInstruction    [0x26] 0 2;
    insDLOAD_1            = mkLoadInstruction    [0x27] 1 2;
    insDLOAD_2            = mkLoadInstruction    [0x28] 2 2;
    insDLOAD_3            = mkLoadInstruction    [0x29] 3 2;

    insALOAD_0            = mkLoadInstruction    [0x2A] 0 1;
    insALOAD_1            = mkLoadInstruction    [0x2B] 1 1;
    insALOAD_2            = mkLoadInstruction    [0x2C] 2 1;
    insALOAD_3            = mkLoadInstruction    [0x2D] 3 1;

    insILOAD  0            = insILOAD_0;
    insILOAD  1            = insILOAD_1;
    insILOAD  2            = insILOAD_2;
    insILOAD  3            = insILOAD_3;
    insILOAD  i            = mkLoadInstruction    [0x15,i] i 1;

    insLLOAD  0            = insLLOAD_0;
    insLLOAD  1            = insLLOAD_1;
    insLLOAD  2            = insLLOAD_2;
    insLLOAD  3            = insLLOAD_3;
    insLLOAD  i            = mkLoadInstruction    [0x16,i] i 2;

    insFLOAD  0            = insFLOAD_0;
    insFLOAD  1            = insFLOAD_1;
    insFLOAD  2            = insFLOAD_2;
    insFLOAD  3            = insFLOAD_3;
    insFLOAD  i            = mkLoadInstruction    [0x17,i] i 1;

    insDLOAD  0            = insDLOAD_0;
    insDLOAD  1            = insDLOAD_1;
    insDLOAD  2            = insDLOAD_2;
    insDLOAD  3            = insDLOAD_3;
    insDLOAD  i            = mkLoadInstruction    [0x18,i] i 2;

    insALOAD  0            = insALOAD_0;
    insALOAD  1            = insALOAD_1;
    insALOAD  2            = insALOAD_2;
    insALOAD  3            = insALOAD_3;
    insALOAD  i            = mkLoadInstruction    [0x19,i] i 1;

    insLOAD :: MachineValueType t -> Word8 -> Instruction;
    insLOAD MT_I   = insILOAD;
    insLOAD MT_L   = insLLOAD;
    insLOAD MT_F   = insFLOAD;
    insLOAD MT_D   = insDLOAD;
    insLOAD MT_A   = insALOAD;


    -- 36 - 4E STORE

    mkStoreInstruction :: [Word8] -> LocalVar -> Word8 -> Instruction;
    mkStoreInstruction bytes locid size = mkVarFixedInstruction bytes locid size (upWord16 size) 0;

    insISTORE_0            = mkStoreInstruction    [0x3B] 0 1;
    insISTORE_1            = mkStoreInstruction    [0x3C] 1 1;
    insISTORE_2            = mkStoreInstruction    [0x3D] 2 1;
    insISTORE_3            = mkStoreInstruction    [0x3E] 3 1;

    insLSTORE_0            = mkStoreInstruction    [0x3F] 0 2;
    insLSTORE_1            = mkStoreInstruction    [0x40] 1 2;
    insLSTORE_2            = mkStoreInstruction    [0x41] 2 2;
    insLSTORE_3            = mkStoreInstruction    [0x42] 3 2;

    insFSTORE_0            = mkStoreInstruction    [0x43] 0 1;
    insFSTORE_1            = mkStoreInstruction    [0x44] 1 1;
    insFSTORE_2            = mkStoreInstruction    [0x45] 2 1;
    insFSTORE_3            = mkStoreInstruction    [0x46] 3 1;

    insDSTORE_0            = mkStoreInstruction    [0x47] 0 2;
    insDSTORE_1            = mkStoreInstruction    [0x48] 1 2;
    insDSTORE_2            = mkStoreInstruction    [0x49] 2 2;
    insDSTORE_3            = mkStoreInstruction    [0x4A] 3 2;

    insASTORE_0            = mkStoreInstruction    [0x4B] 0 1;
    insASTORE_1            = mkStoreInstruction    [0x4C] 1 1;
    insASTORE_2            = mkStoreInstruction    [0x4D] 2 1;
    insASTORE_3            = mkStoreInstruction    [0x4E] 3 1;

    insISTORE  0        = insISTORE_0;
    insISTORE  1        = insISTORE_1;
    insISTORE  2        = insISTORE_2;
    insISTORE  3        = insISTORE_3;
    insISTORE  i        = mkStoreInstruction    [0x36,i] i 1;

    insLSTORE  0        = insLSTORE_0;
    insLSTORE  1        = insLSTORE_1;
    insLSTORE  2        = insLSTORE_2;
    insLSTORE  3        = insLSTORE_3;
    insLSTORE  i        = mkStoreInstruction    [0x37,i] i 2;

    insFSTORE  0        = insFSTORE_0;
    insFSTORE  1        = insFSTORE_1;
    insFSTORE  2        = insFSTORE_2;
    insFSTORE  3        = insFSTORE_3;
    insFSTORE  i        = mkStoreInstruction    [0x38,i] i 1;

    insDSTORE  0        = insDSTORE_0;
    insDSTORE  1        = insDSTORE_1;
    insDSTORE  2        = insDSTORE_2;
    insDSTORE  3        = insDSTORE_3;
    insDSTORE  i        = mkStoreInstruction    [0x39,i] i 2;

    insASTORE  0        = insASTORE_0;
    insASTORE  1        = insASTORE_1;
    insASTORE  2        = insASTORE_2;
    insASTORE  3        = insASTORE_3;
    insASTORE  i        = mkStoreInstruction    [0x3A,i] i 1;

    insSTORE :: MachineValueType t -> Word8 -> Instruction;
    insSTORE MT_I  = insISTORE;
    insSTORE MT_L  = insLSTORE;
    insSTORE MT_F  = insFSTORE;
    insSTORE MT_D  = insDSTORE;
    insSTORE MT_A  = insASTORE;


    -- 59 - 5F DUP/SWAP

    insDUP              = mkFixedInstruction [0x59] 1 2;
    insDUP_X1           = mkFixedInstruction [0x5A] 2 3;
    insDUP_X2           = mkFixedInstruction [0x5B] 3 4;
    insDUP2             = mkFixedInstruction [0x5C] 2 4;
    insDUP2_X1          = mkFixedInstruction [0x5D] 3 5;
    insDUP2_X2          = mkFixedInstruction [0x5E] 4 6;
    insSWAP             = mkFixedInstruction [0x5F] 2 2;


    -- AC - B1 RETURN

    insIRETURN          = mkFixedInstruction    [0xAC] 1 0;
    insLRETURN          = mkFixedInstruction    [0xAD] 2 0;
    insFRETURN          = mkFixedInstruction    [0xAE] 1 0;
    insDRETURN          = mkFixedInstruction    [0xAF] 2 0;
    insARETURN          = mkFixedInstruction    [0xB0] 1 0;
    insVRETURN          = mkFixedInstruction    [0xB1] 0 0;

    insRETURN :: MachineReturnType t -> Instruction;
    insRETURN MR_Void          = insVRETURN;
    insRETURN (MR_Value (MT_I)) = insIRETURN;
    insRETURN (MR_Value (MT_L)) = insLRETURN;
    insRETURN (MR_Value (MT_F)) = insFRETURN;
    insRETURN (MR_Value (MT_D)) = insDRETURN;
    insRETURN (MR_Value (MT_A)) = insARETURN;


    -- B4 - B5 FIELD

    insGETFIELD fref    = MkInstruction
        (MkFieldBasicInstruction 0xB4 fref)
        1                -- objectref
        (upWord16 (valueSize (ntType (rNameType fref))));

    insPUTFIELD fref    = MkInstruction
        (MkFieldBasicInstruction 0xB5 fref)
        (1 + (upWord16 (valueSize (ntType (rNameType fref))))) -- objectref, value
        0;


    -- B6 - B9 INVOKE

    methodArgListSize :: FunctionType -> Word8;
    methodArgListSize ftype = valueListSize (ftArgs ftype);

    methodReturnSize :: FunctionType -> Word8;
    methodReturnSize ftype = returnSize (ftReturns ftype);

    insINVOKEVIRTUAL mref    = MkInstruction
        (MkMethodBasicInstruction 0xB6 mref)
        (upWord16 (1 + (methodArgListSize ftype)))
        (upWord16 (methodReturnSize ftype))
        where
        {
        ftype = ntType (rNameType mref);
        };

    insINVOKESPECIAL mref    = MkInstruction
        (MkMethodBasicInstruction 0xB7 mref)
        (upWord16 (1 + (methodArgListSize ftype)))
        (upWord16 (methodReturnSize ftype))
        where
        {
        ftype = ntType (rNameType mref);
        };

    insINVOKESTATIC mref    = MkInstruction
        (MkMethodBasicInstruction 0xB8 mref)
        (upWord16 (methodArgListSize ftype))
        (upWord16 (methodReturnSize ftype))
        where
        {
        ftype = ntType (rNameType mref);
        };

    insINVOKEINTERFACE mref    = MkInstruction
        (MkIntfMethodBasicInstruction 0xB9 mref objargsize)
        (upWord16 objargsize)
        (upWord16 (methodReturnSize ftype))
        where
        {
        ftype = ntType (rNameType mref);
        objargsize = 1 + (methodArgListSize ftype);
        };

    insNEW cl        = mkClassInstruction 0xBB cl 0 1;

    insCHECKCAST cl    = mkClassInstruction 0xC0 cl 1 1;

    writeBasicInstruction :: BasicInstruction -> Pooler [Word8];
    writeBasicInstruction (MkFixedBasicInstruction ops) = return ops;
    writeBasicInstruction (MkVarFixedBasicInstruction ops _ _) = return ops;
    writeBasicInstruction (MkClassBasicInstruction op cname) = do
        {
        ind <- addClassConstant cname;
        return ([op] ++ (jcfWrite ind));
        };
    writeBasicInstruction (MkMethodBasicInstruction op mref) = do
        {
        ind <- addMethodRefConstant mref;
        return ([op] ++ (jcfWrite ind));
        };
    writeBasicInstruction (MkIntfMethodBasicInstruction op mref count) = do
        {
        ind <- addInterfaceMethodRefConstant mref;
        return ([op] ++ (jcfWrite ind) ++ [count,0]);
        };
    writeBasicInstruction (MkFieldBasicInstruction op fref) = do
        {
        ind <- addFieldRefConstant fref;
        return ([op] ++ (jcfWrite ind));
        };
    writeBasicInstruction (MkIntegerConstBasicInstruction n) = do
        {
        ind <- addIntegerConstant n;
        return (codeLDC1 ind);
        };
    writeBasicInstruction (MkFloatConstBasicInstruction n) = do
        {
        ind <- addFloatConstant n;
        return (codeLDC1 ind);
        };
    writeBasicInstruction (MkLongConstBasicInstruction n) = do
        {
        ind <- addLongConstant n;
        return (codeLDC2 ind);
        };
    writeBasicInstruction (MkDoubleConstBasicInstruction n) = do
        {
        ind <- addDoubleConstant n;
        return (codeLDC2 ind);
        };

    writeInstruction :: Instruction -> Pooler [Word8];
    writeInstruction (MkInstruction ins _ _) = writeBasicInstruction ins;

    type CodeBlock = [Instruction];

    writeCodeBytes :: CodeBlock -> Pooler [Word8];
    writeCodeBytes block = liftM concat (mapM writeInstruction block);

    insLocalsSize :: CodeBlock -> LocalVar;
    insLocalsSize [] = 0;
    insLocalsSize ((MkInstruction (MkVarFixedBasicInstruction _ v vs) _ _):cs) = max (v + vs) (insLocalsSize cs);
    insLocalsSize (_:cs) = insLocalsSize cs;

    localsSize :: [ValueType] -> CodeBlock -> Word16;
    localsSize args ins = fromIntegral (max (valueListSize args) (insLocalsSize ins));

    makePositive16 :: Int16 -> Word16;
    makePositive16 i | i < 0 = 0;
    makePositive16 i = toWord16 i;

    trackStack :: CodeBlock -> (Int16,Word16);
    trackStack [] = (0,0);
    trackStack ((MkInstruction _ t g):cs) = (newadds,newmax) where
        {
        (csadds,csmax) = trackStack cs;
        takes = toInt16 t;
        gives = toInt16 g;
        cadds = gives - takes;
        newadds = cadds + csadds;
        newmax = makePositive16 (cadds + (toInt16 csmax));
        };

    maxStackDepth :: CodeBlock -> Word16;
    maxStackDepth block = snd (trackStack block);
    }
