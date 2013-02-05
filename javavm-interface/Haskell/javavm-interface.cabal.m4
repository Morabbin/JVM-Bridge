name:			javavm-interface
version:		1.0
license-file:	LICENSE
author:			Ashley Yakeley <ashley@semantic.org>
maintainer:		Ashley Yakeley <ashley@semantic.org>
build-depends:	base >= 4.0, witness, javavm
synopsis:		bridge to a running Java Virtual Machine
extensions:
	ForeignFunctionInterface
	ImplicitParams
	RankNTypes
	MultiParamTypeClasses
	TypeSynonymInstances
	GADTs
	FlexibleContexts
	FlexibleInstances
	OverloadedStrings
ghc-options:	-Wall -Werror -O0
exposed-modules:
	Foreign.JavaVM.VM
other-modules:
	Foreign.JavaVM.Configure
	Foreign.JavaVM.Raw.Callback
	Foreign.JavaVM.Raw.Env
	Foreign.JavaVM.Raw.Types
	Foreign.JavaVM.Raw.VM
	Foreign.JavaVM.Raw.ValueList
	Foreign.JavaVM.Raw.Invocation
	Foreign.JavaVM.Raw
	Foreign.JavaVM.VM.Witness
	Foreign.JavaVM.VM.Array
	Foreign.JavaVM.VM.Callback.Constructor
	Foreign.JavaVM.VM.Callback.Field
	Foreign.JavaVM.VM.Callback.Finalizer
	Foreign.JavaVM.VM.Callback.Method
	Foreign.JavaVM.VM.Callback
	Foreign.JavaVM.VM.Class
	Foreign.JavaVM.VM.Field.Access
	Foreign.JavaVM.VM.Field.ID
	Foreign.JavaVM.VM.Field
	Foreign.JavaVM.VM.Method.Call
	Foreign.JavaVM.VM.Method
	Foreign.JavaVM.VM.NewObject
	Foreign.JavaVM.VM.Object
	Foreign.JavaVM.VM.Ref
	Foreign.JavaVM.VM.String
	Foreign.JavaVM.VM.StringPtr
	Foreign.JavaVM.VM.Thread
	Foreign.JavaVM.VM.Throwable
	Foreign.JavaVM.VM.Types
	Foreign.JavaVM.VM.ValueList
	Foreign.JavaVM.VM.Invocation
c-sources:			cbits/HsExecuteFunction.c
include-dirs:		include/ INCLUDEDIRS
extra-lib-dirs:		patsubst(LIBDIRS,`:',` ')
extra-libraries:	JVMBridge JVMInvocation
ld-options:			JVM_LDFLAGS ifelse(OSNAME,`Darwin',`',`patsubst(patsubst(LIBDIRS,`:',` '),`[^ 	]+',`"-Wl,-rpath" "-Wl,\&"')')
-- framework-dirs:		JVM_FRAMEWORKDIRS
-- frameworks:			JVM_FRAMEWORKS
