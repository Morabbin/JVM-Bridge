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
#include "Debug.hpp"
#include "ValueListPrivate.hpp"

using namespace ::std;

jvaluelist JVMBRIDGEFUNC(CreateValueList)()
    {
    DebugBlockTracer tracer("CreateValueList");
    jvaluelist vl = reinterpret_cast<jvaluelist>(new _jvaluelist());
    DebugShowPointer(vl,"value-list");
    return vl;
    }

void JVMBRIDGEFUNC(DestroyValueList)(jvaluelist vl)
    {
    DebugBlockTracer tracer("DestroyValueList");
    DebugShowPointer(vl,"value-list");
    delete reinterpret_cast<_jvaluelist*>(vl);
    }

void JVMBRIDGEFUNC(AddBooleanToValueList) (jvaluelist vl,jboolean v)
    {
    DebugBlockTracer tracer("AddBooleanToValueList");
    DebugShowPointer(vl,"value-list");
    DebugShowBoolean(v,"adding");
    jvalue value;
    value.z = v;
    GetValueList(vl).push_back(value);
    }

void JVMBRIDGEFUNC(AddByteToValueList) (jvaluelist vl,jbyte v)
    {
    DebugBlockTracer tracer("AddByteToValueList");
    DebugShowPointer(vl,"value-list");
    DebugShowByte(v,"adding");
    jvalue value;
    value.b = v;
    GetValueList(vl).push_back(value);
    }

void JVMBRIDGEFUNC(AddCharToValueList) (jvaluelist vl,jchar v)
    {
    DebugBlockTracer tracer("AddCharToValueList");
    DebugShowPointer(vl,"value-list");
    DebugShowChar(v,"adding");
    jvalue value;
    value.c = v;
    GetValueList(vl).push_back(value);
    }

void JVMBRIDGEFUNC(AddShortToValueList) (jvaluelist vl,jshort v)
    {
    DebugBlockTracer tracer("AddShortToValueList");
    DebugShowPointer(vl,"value-list");
    DebugShowShort(v,"adding");
    jvalue value;
    value.s = v;
    GetValueList(vl).push_back(value);
    }

void JVMBRIDGEFUNC(AddIntToValueList) (jvaluelist vl,jint v)
    {
    DebugBlockTracer tracer("AddIntToValueList");
    DebugShowPointer(vl,"value-list");
    DebugShowInt(v,"adding");
    jvalue value;
    value.i = v;
    GetValueList(vl).push_back(value);
    }

void JVMBRIDGEFUNC(AddLongToValueList) (jvaluelist vl,jlong v)
    {
    DebugBlockTracer tracer("AddLongToValueList");
    DebugShowPointer(vl,"value-list");
    DebugShowLong(v,"adding");
    jvalue value;
    value.j = v;
    GetValueList(vl).push_back(value);
    }

void JVMBRIDGEFUNC(AddFloatToValueList) (jvaluelist vl,jfloat v)
    {
    DebugBlockTracer tracer("AddFloatToValueList");
    DebugShowPointer(vl,"value-list");
    DebugShowFloat(v,"adding");
    jvalue value;
    value.f = v;
    GetValueList(vl).push_back(value);
    }

void JVMBRIDGEFUNC(AddDoubleToValueList) (jvaluelist vl,jdouble v)
    {
    DebugBlockTracer tracer("AddDoubleToValueList");
    DebugShowPointer(vl,"value-list");
    DebugShowDouble(v,"adding");
    jvalue value;
    value.d = v;
    GetValueList(vl).push_back(value);
    }

void JVMBRIDGEFUNC(AddObjectToValueList) (jvaluelist vl,jobject v)
    {
    DebugBlockTracer tracer("AddObjectToValueList");
    DebugShowPointer(vl,"value-list");
    DebugShowRef(v,"adding");
    jvalue value;
    value.l = v;
    GetValueList(vl).push_back(value);
    }

jboolean JVMBRIDGEFUNC(GetNthValueAsBoolean) (jvaluelist vl,jshort i)
    {
    DebugBlockTracer tracer("GetNthValueAsBoolean");
    DebugShowPointer(vl,"value-list");
    DebugShowShort(i,"index");
    jboolean v = GetValueList(vl)[i].z;
    DebugShowBoolean(v,"returns");
    return v;
    }

jbyte JVMBRIDGEFUNC(GetNthValueAsByte) (jvaluelist vl,jshort i)
    {
    DebugBlockTracer tracer("GetNthValueAsByte");
    DebugShowPointer(vl,"value-list");
    DebugShowShort(i,"index");
    jbyte v = GetValueList(vl)[i].b;
    DebugShowByte(v,"returns");
    return v;
    }

jchar JVMBRIDGEFUNC(GetNthValueAsChar) (jvaluelist vl,jshort i)
    {
    DebugBlockTracer tracer("GetNthValueAsChar");
    DebugShowPointer(vl,"value-list");
    DebugShowShort(i,"index");
    jchar v = GetValueList(vl)[i].c;
    DebugShowChar(v,"returns");
    return v;
    }

jshort JVMBRIDGEFUNC(GetNthValueAsShort) (jvaluelist vl,jshort i)
    {
    DebugBlockTracer tracer("GetNthValueAsShort");
    DebugShowPointer(vl,"value-list");
    DebugShowShort(i,"index");
    jshort v = GetValueList(vl)[i].s;
    DebugShowShort(v,"returns");
    return v;
    }

jint JVMBRIDGEFUNC(GetNthValueAsInt) (jvaluelist vl,jshort i)
    {
    DebugBlockTracer tracer("GetNthValueAsInt");
    DebugShowPointer(vl,"value-list");
    DebugShowShort(i,"index");
    jint v = GetValueList(vl)[i].i;
    DebugShowInt(v,"returns");
    return v;
    }

jlong JVMBRIDGEFUNC(GetNthValueAsLong) (jvaluelist vl,jshort i)
    {
    DebugBlockTracer tracer("GetNthValueAsLong");
    DebugShowPointer(vl,"value-list");
    DebugShowShort(i,"index");
    jlong v = GetValueList(vl)[i].j;
    DebugShowLong(v,"returns");
    return v;
    }

jfloat JVMBRIDGEFUNC(GetNthValueAsFloat) (jvaluelist vl,jshort i)
    {
    DebugBlockTracer tracer("GetNthValueAsFloat");
    DebugShowPointer(vl,"value-list");
    DebugShowShort(i,"index");
    jfloat v = GetValueList(vl)[i].f;
    DebugShowFloat(v,"returns");
    return v;
    }

jdouble JVMBRIDGEFUNC(GetNthValueAsDouble) (jvaluelist vl,jshort i)
    {
    DebugBlockTracer tracer("GetNthValueAsDouble");
    DebugShowPointer(vl,"value-list");
    DebugShowShort(i,"index");
    jdouble v = GetValueList(vl)[i].d;
    DebugShowDouble(v,"returns");
    return v;
    }

jobject JVMBRIDGEFUNC(GetNthValueAsObject) (jvaluelist vl,jshort i)
    {
    DebugBlockTracer tracer("GetNthValueAsObject");
    DebugShowPointer(vl,"value-list");
    DebugShowShort(i,"index");
    jobject v = GetValueList(vl)[i].l;
    DebugShowRef(v,"returns");
    return v;
    }
