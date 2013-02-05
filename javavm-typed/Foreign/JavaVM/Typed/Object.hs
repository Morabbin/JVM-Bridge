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

module Foreign.JavaVM.Typed.Object
    (
    IsJavaClassMarker(cName),
    ObjectRef,
    className
    ) where
    {
    import Foreign.JavaVM.Typed.Reference;
    import Platform.JavaVM;
    import Data.Witness;
    
    class IsJavaClassMarker c where
        {
        cName    :: Type c -> ClassName;
        };

    data ObjectReferenceFlavour a;
    type ObjectRef c = TLRef (ObjectReferenceFlavour c);
    
    instance (IsJavaClassMarker c) => IsJReferenceFlavour (ObjectReferenceFlavour c) where
        {
        tlRefValueType t = MkObjectType (tlNameForFindClass t);

        tlNameForFindClass t = (cName (tMap t)) where
            {
            tMap :: Type (ObjectReferenceFlavour c) -> Type c;
            tMap Type = Type;
            };
        };
    
    className :: (IsJavaClassMarker c) => Type (ObjectRef c) -> ClassName;
    className t = cName (tMap t) where
        {
        tMap :: Type (ObjectRef c) -> Type c;
        tMap Type = Type;
        };
    }
