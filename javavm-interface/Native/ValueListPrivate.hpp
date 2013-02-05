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
#ifndef __ValueListPrivate_hpp__
#define __ValueListPrivate_hpp__

#include "ValueList.h"
#include <vector>

typedef ::std::vector<jvalue> _jvaluelist;

inline _jvaluelist& GetValueList(jvaluelist vl)
    {
    return (*reinterpret_cast<_jvaluelist*>(vl));
    }

#endif // ndef __ValueListPrivate_hpp__
