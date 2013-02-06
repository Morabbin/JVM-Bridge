JVM-Bridge
==========

A fork of Ashley Yakeley's JVM-Bridge, available via `darcs get http://code.haskell.org/JVM-Bridge/`.

License is GNU LGPL 2.1. It consists of four pieces:
 * `javavm`: Haskell code with no use of FFI or dependency on the JNI. It's basic types plus some bytecode assembly.

 * `javavm-interface/Native/`: thin-ish layer over the JNI and some other native stuff in C/C++. This makes no use of Haskell.

 * `javavm-interface/Haskell/`: representation of the JNI in Haskell.

 * `javavm-typed/`: representation of the JVM in Haskell, basically a better typed layer over javavm-interface. This code does not build. Currently, it does a great deal of type-level computation auto-generating Java class type names from the Java libraries. It ought to be completely overhauled, but serves as a detailed design.

Status: 2013-02-06
------------------

The last of these, `javavm-typed` is not building at the moment
(under GHC 7.6.1, Mac OS X x86\_64). The first three are building, but
have not been extensively tested.

