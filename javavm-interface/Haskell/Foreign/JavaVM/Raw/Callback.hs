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

module Foreign.JavaVM.Raw.Callback where
    {
    import Foreign.JavaVM.Raw.Env;
    import Foreign.JavaVM.Raw.ValueList;
    import Foreign.JavaVM.Raw.Types;
    import Foreign;

    foreign import ccall "HsExecuteFunction.h StartExecuteFunction" rawStartExecuteFunction    :: RawEnv -> IO ();

    type WrapperCall t = t -> IO (FunPtr t);

    foreign import ccall "wrapper" rawMakeVoidCallback      :: WrapperCall (RawEnv -> RawValueList -> IO RawVoid);
    foreign import ccall "wrapper" rawMakeBooleanCallback   :: WrapperCall (RawEnv -> RawValueList -> IO RawBoolean);
    foreign import ccall "wrapper" rawMakeByteCallback      :: WrapperCall (RawEnv -> RawValueList -> IO RawByte);
    foreign import ccall "wrapper" rawMakeCharCallback      :: WrapperCall (RawEnv -> RawValueList -> IO RawChar);
    foreign import ccall "wrapper" rawMakeShortCallback     :: WrapperCall (RawEnv -> RawValueList -> IO RawShort);
    foreign import ccall "wrapper" rawMakeIntCallback       :: WrapperCall (RawEnv -> RawValueList -> IO RawInt);
    foreign import ccall "wrapper" rawMakeLongCallback      :: WrapperCall (RawEnv -> RawValueList -> IO RawLong);
    foreign import ccall "wrapper" rawMakeFloatCallback     :: WrapperCall (RawEnv -> RawValueList -> IO RawFloat);
    foreign import ccall "wrapper" rawMakeDoubleCallback    :: WrapperCall (RawEnv -> RawValueList -> IO RawDouble);
    foreign import ccall "wrapper" rawMakeObjectCallback    :: WrapperCall (RawEnv -> RawValueList -> IO RawGlobalRef);
    }
