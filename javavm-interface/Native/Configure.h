/*
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
*/

// preprocessor only in this file, it's used for both C++ and Java.
#ifndef __Configure_h__
#define __Configure_h__

#ifdef _STDINT_H_
#error <stdint.h> already included
#endif
#define __STDC_LIMIT_MACROS 1
#include <stdint.h>

#include "autoheader.h"
// fix this for jni_md.h, which checks WIN32 by def rather than by value
#ifdef WIN32
#if !WIN32
#undef WIN32
#endif
#endif

#ifndef DEBUG
#define DEBUG 0
#endif

#ifndef DEBUGINFO
#define DEBUGINFO DEBUG
#endif

#ifndef DEBUGCALLS
#define DEBUGCALLS DEBUG
#endif

#ifndef DEBUGVM
#define DEBUGVM DEBUG
#endif

#ifndef DEBUGEXCEPTION
#define DEBUGEXCEPTION DEBUGCALLS
#endif

#ifndef DEBUGGC
#define DEBUGGC DEBUGCALLS
#endif

#ifndef DEBUGTHREAD
//#define DEBUGTHREAD DEBUGCALLS
#define DEBUGTHREAD 0
#endif

#if DEBUGCALLS
#define JDEBUGMESSAGE(s) System.err.println(s)
#else
#define JDEBUGMESSAGE(s)
#endif

#ifndef INTF_CNI
#define INTF_CNI 0
#endif

#ifndef VM_SUN
#define VM_SUN 0
#endif

#ifndef VM_IBM
#define VM_IBM 0
#endif

#ifndef VM_KAFFE
#define VM_KAFFE 0
#endif

#ifndef VM_GCJ
#define VM_GCJ 0
#endif

#ifndef UINTPTR_MAX
#error UINTPTR_MAX not defined
#endif

#ifndef UINT64_MAX
#error UINT64_MAX not defined
#endif

#ifndef UINT32_MAX
#error UINT32_MAX not defined
#endif

#ifndef JVM_ADDRESSSIZE

#error JVM_ADDRESSSIZE not defined

#elif JVM_ADDRESSSIZE == 64

#if UINTPTR_MAX == UINT64_MAX
#define ARCH_64BIT 1
#else
#error UINTPTR_MAX not 64 bit
#endif

#elif JVM_ADDRESSSIZE == 32

#if UINTPTR_MAX == UINT32_MAX
#define ARCH_64BIT 0
#else
#error UINTPTR_MAX not 32 bit
#endif

#else
#error unrecognised JVM_ADDRESSSIZE
#endif


#ifndef JOpaqueAddress
#if ARCH_64BIT
// Java long is 64 bits
#define JOpaqueAddress long
#else
// Java int is 32 bits
#define JOpaqueAddress int
#endif
#endif

#ifndef COpaqueAddress
// C equivalent of JOpaqueAddress
#if ARCH_64BIT
#define COpaqueAddress jlong
#else
#define COpaqueAddress jint
#endif
#endif

#ifndef SigOpaqueAddress
// signature of JOpaqueAddress
#if ARCH_64BIT
#define SigOpaqueAddress "J"
#else
#define SigOpaqueAddress "I"
#endif
#endif

#ifndef SYNCH
// synchronise all function executions
#define SYNCH 1
#endif

#endif
