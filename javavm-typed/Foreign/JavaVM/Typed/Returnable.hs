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

module Foreign.JavaVM.Typed.Returnable where
{
    import Foreign.JavaVM.VM;
    import Platform.JavaVM;
    import Data.Witness;

    class IsJVMReturnable t where
    {
        getReturnableType    :: Type t -> ReturnType;
        tlFromVMReturnType    :: forall r. (forall vmrt. (Is VMReturnType vmrt) => (vmrt -> VM t) -> r) -> r;
        tlMakeCallback        :: [AnyWitness VMType] -> ([Any VMType] -> VM t) -> VM OpaqueAddress;
    };

    instance IsJVMReturnable Jvoid where
    {
        getReturnableType Type = MkVoidType;
        tlFromVMReturnType foo = foo return;

        tlMakeCallback = vmMakeCallback;
    };

    returnVoid        = Type::Type Jvoid;
    returnBoolean    = Type::Type Jboolean;
    returnByte        = Type::Type Jbyte;
    returnChar        = Type::Type Jchar;
    returnShort        = Type::Type Jshort;
    returnInt        = Type::Type Jint;
    returnLong        = Type::Type Jlong;
    returnFloat        = Type::Type Jfloat;
    returnDouble    = Type::Type Jdouble;
}
