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
#ifndef __Debug_hpp__
#define __Debug_hpp__

#include "Configure.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <jni.h>

#if DEBUGTHREAD
#include <pthread.h>
#endif

inline void ExceptionDebugMessage(__attribute__((unused)) const char* format,...)
    {
#if DEBUGEXCEPTION
    va_list args;
    va_start(args,format);
    vfprintf(stderr,format,args);
    va_end (args);
#endif
    }

inline void DebugMessage(__attribute__((unused)) const char* format,...)
    {
#if DEBUGCALLS
    va_list args;
    va_start(args,format);
    vfprintf(stderr,format,args);
    va_end (args);
#endif
    }

inline void InfoDebugMessage(__attribute__((unused)) const char* format,...)
    {
#if DEBUGINFO
    va_list args;
    va_start(args,format);
    vfprintf(stderr,format,args);
    va_end (args);
#endif
    }

inline void ErrorMessage(const char* format,...)
    {
    va_list args;
    va_start(args,format);
    vfprintf(stderr,format,args);
    va_end (args);
    exit(EXIT_FAILURE);
    }

#if DEBUGTHREAD
inline pthread_t GetThreadID()
    {
    return pthread_self();
    }
#endif

inline void DebugShowThread()
    {
#if DEBUGTHREAD
    DebugMessage("0x%8.8X: ",GetThreadID());
#endif
    }

inline void ThreadedDebugMessage(__attribute__((unused)) const char* format,...)
    {
    DebugShowThread();
#if DEBUGCALLS
    va_list args;
    va_start(args,format);
    vfprintf(stderr,format,args);
    va_end (args);
#endif
    }

class DebugBlockTracer
    {
#if DEBUGCALLS
    const char* mName;
#endif

    public:

    inline DebugBlockTracer(__attribute__((unused)) const char* name)
#if DEBUGCALLS
    :mName(name)
#endif
        {
#if DEBUGCALLS
        ThreadedDebugMessage("+++ %s\n",mName);
#endif
        }

    inline ~DebugBlockTracer()
        {
#if DEBUGCALLS
        ThreadedDebugMessage("--- %s\n",mName);
#endif
        }
    };

inline void DebugShowPointer(void* p,const char* name)
    {
    if (p == 0)
        DebugMessage("! %s is null\n",name);
    else DebugMessage(
#if ARCH_64BIT
    "%s = 0x%16.16X\n"
#else
    "%s = 0x%8.8X\n"
#endif
    ,name,reinterpret_cast<COpaqueAddress>(p));
    }

inline void DebugShowBoolean(jboolean v,const char* name)
    {
    DebugMessage("%s %s\n",name,v? "true": "false");
    }

inline void DebugShowByte(jbyte v,const char* name)
    {
    DebugMessage("%s %d (0x%2.2X)\n",name,v,v);
    }

inline void DebugShowChar(jchar v,const char* name)
    {
    DebugMessage("%s %u (0x%4.4X)\n",name,v,v);
    }

inline void DebugShowShort(jshort v,const char* name)
    {
    DebugMessage("%s %hd (0x%4.4X)\n",name,v,v);
    }

inline void DebugShowInt(jint v,const char* name)
    {
    DebugMessage("%s %ld (0x%8.8lX)\n",name,v,v);
    }

inline void DebugShowLong(jlong v,const char* name)
    {
    // 'L' modifier doesn't work with Darwin for some reason
    DebugMessage("%s %lld (0x%16.16llX)\n",name,v,v);
    }

inline void DebugShowFloat(jfloat v,const char* name)
    {
    DebugMessage("%s %G\n",name,v);
    }

inline void DebugShowDouble(jdouble v,const char* name)
    {
    DebugMessage("%s %G\n",name,v);
    }

inline void DebugShowRef(jobject p,const char* name)
    {
    DebugMessage("%s 0x%8.8X\n",name,reinterpret_cast<COpaqueAddress>(p));
    }

const int kRowBytes = 16;

inline void DebugShowBytes(__attribute__((unused)) const unsigned char* p,__attribute__((unused)) int n)
    {
#if DEBUGCALLS
    int rows = (n-1)/kRowBytes + 1;
    for (int row=0;row<rows;row++)
        {
        int nRow = n-(row*kRowBytes);
        if (nRow > kRowBytes)
            nRow = kRowBytes;
        for (int i=0;i<nRow;i++)
            DebugMessage("%2.2X ",p[row*kRowBytes+i]);
        DebugMessage("\n");
        }
#endif
    }

inline void DebugShowBytes(const jbyte* p,int n)
    {
    DebugShowBytes(reinterpret_cast<const unsigned char*>(p),n);
    }

#endif // ndef __Debug_hpp__
