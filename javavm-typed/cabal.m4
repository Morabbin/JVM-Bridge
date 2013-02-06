Name:			ifelse(PROGRAM,,javavm-bridge,PROGRAM)
Version:		1.0
License-File:	LICENSE
Author:			Ashley Yakeley <ashley@semantic.org>
Maintainer:		Ashley Yakeley <ashley@semantic.org>
Build-Depends:	base >= 4.0, witness, javavm, javavm-interface
Build-type:     Simple
ifelse(PROGRAM,,`Synopsis:		bridge to a running Java Virtual Machine
Exposed-Modules:
	Foreign.JavaVM
	Foreign.JavaVM.Typed
	Foreign.JavaVM.Callback
	Foreign.JavaVM.Thread
	Foreign.JavaVM.Invocation
	Foreign.JavaVM.Lib.Header
	Foreign.JavaVM.Lib.Class_java_lang_Thread
	Foreign.JavaVM.Lib.Class_java_lang_Object
	Foreign.JavaVM.Lib.Class_java_lang_Class
Other-Modules:
	Foreign.JavaVM.Lib.Header.Java
	Foreign.JavaVM.Lib.Header.JavaX
	Foreign.JavaVM.Lib.Header.OMG
	Foreign.JavaVM.Lib.Header.Misc
',`Synopsis:		PROGRAM program

Executable:		PROGRAM
Main-Is:		PROGRAM.hs
Other-Modules:
	Foreign.JavaVM.Typed
')dnl
	Foreign.JavaVM.Typed.ArgumentList
	Foreign.JavaVM.Typed.Array
	Foreign.JavaVM.Typed.Callback
	Foreign.JavaVM.Typed.Class
	Foreign.JavaVM.Typed.Field
	Foreign.JavaVM.Typed.ListArray
	Foreign.JavaVM.Typed.Method
	Foreign.JavaVM.Typed.NewObject
	Foreign.JavaVM.Typed.Object
	Foreign.JavaVM.Typed.Primitive
	Foreign.JavaVM.Typed.Reference
	Foreign.JavaVM.Typed.Returnable
	Foreign.JavaVM.Typed.String
	Foreign.JavaVM.Typed.Thread
	Foreign.JavaVM.Typed.Throwable
	Foreign.JavaVM.Typed.Tuple
	Foreign.JavaVM.Typed.Value
	Foreign.JavaVM.Typed.Invocation
	Foreign.JavaVM.Typed.Loadable
Extensions:
	RankNTypes 
	FlexibleContexts
	MultiParamTypeClasses
	TypeSynonymInstances 
	EmptyDataDecls
	ScopedTypeVariables
	ImplicitParams
	GADTs
	OverloadedStrings
	ImpredicativeTypes
