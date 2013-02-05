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

module Data.Subtype where
    {
    import Data.Witness;
    
    
    -- pure
    
    class IsA to from where
        {
        convert    :: from -> to;
        };
    
    class MaybeA to from where
        {
        maybeConvert    :: from -> Maybe to;
        is                :: Type to -> from -> Bool;

        is _ from = case ((maybeConvert from) :: Maybe to) of
            {
            Just _ -> True;
            Nothing -> False;
            };
        };

    class
        (
        IsA super sub,
        MaybeA sub super
        ) =>
     Subtype super sub;


    -- Monad

    class (Monad m) => MonadIsA m to from where
        {
        getConvert    :: from -> m to;
        };

    class (Monad m) => MonadMaybeA m to from where
        {
        getMaybeConvert    :: from -> m (Maybe to);
        getIs            :: Type to -> from -> m Bool;

        getIs _ from = do
            {
            (mto :: Maybe to) <- getMaybeConvert from;
            return (case mto of
                {
                Just _ -> True;
                Nothing -> False;
                });
            };
        };

    class
        (
        MonadIsA m super sub,
        MonadMaybeA m sub super
        ) =>
     MonadSubtype m super sub;    
    
    upcast:: (IsA to from) => from -> to;
    upcast = convert;

    downcast:: (MaybeA to from) => from -> Maybe to;
    downcast = maybeConvert;

    getUpcast:: (MonadIsA m to from) => from -> m to;
    getUpcast = getConvert;

    getDowncast:: (MonadMaybeA m to from) => from -> m (Maybe to);
    getDowncast = getMaybeConvert;    
    }
