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

module Data.Nothing where
    {
    import Foreign hiding (newForeignPtr);
    import Foreign.Concurrent (newForeignPtr);
    
    class HasNothing a where
        {
        nothing :: a;
        isNothing :: a -> Bool;
        };
    
    class (Monad m) => MonadHasNothing m a where
        {
        getNothing :: m a;
        getIsNothing :: a -> m Bool;
        };

{--    Can't do this unfortunately
    instance (Monad m,HasNothing a) => MonadHasNothing m a where
        {
        getNothing = return nothing;
        getIsNothing a = return (isNothing a);
        };
--}
    
    instance (Monad m) => MonadHasNothing m (Maybe a) where
        {
        getNothing = return nothing;
        getIsNothing a = return (isNothing a);
        };
    
    instance HasNothing (Maybe a) where
        {
        nothing = Nothing;
        isNothing Nothing = True;
        isNothing _ = False;
        };
    
    instance (Monad m) => MonadHasNothing m [a] where
        {
        getNothing = return nothing;
        getIsNothing a = return (isNothing a);
        };
    
    instance HasNothing [a] where
        {
        nothing = [];
        isNothing [] = True;
        isNothing _ = False;
        };
    
    instance (Monad m) => MonadHasNothing m (Ptr a) where
        {
        getNothing = return nothing;
        getIsNothing a = return (isNothing a);
        };
    
    instance HasNothing (Ptr a) where
        {
        nothing = nullPtr;
        isNothing a = a == nothing;
        };
    
    instance MonadHasNothing IO (ForeignPtr a) where
        {
        getNothing = do
            {
            n <- getNothing;
            newForeignPtr n (return ());
            };
        getIsNothing a = withForeignPtr a getIsNothing;
        };
    
    checkNothing :: (HasNothing a) => a -> Maybe a;
    checkNothing a = if (isNothing a) then Nothing else (Just a);
    
    failIfNothing :: (MonadHasNothing m a) => String -> a -> m ();
    failIfNothing failMessage a = do
        {
        noth <- getIsNothing a;
        if noth then (fail failMessage) else (return ());
        };
    }
