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

module Control.Monad.Reference where
    {
    

    -- MonadReference
    
    class (Monad m) => MonadReference m r where
        {
        get :: forall a. r a -> m a;
        set :: forall a. r a -> a -> m ();
        };

    modify :: (MonadReference m r) => r a -> (a -> m a) -> m ();
    modify ref ama = do
        {
        a <- get ref;
        a' <- ama a;
        set ref a';
        };


    -- SingleMonadReference
    
    class (MonadReference m r) => SingleMonadReference m r | r -> m;
    
    get1 :: (SingleMonadReference m r) => r a -> m a;
    get1 = get;
    
    set1 :: (SingleMonadReference m r) => r a -> a -> m ();
    set1 = set;
    

    -- Ref    
    
    data Ref m a = MkRef
        {
        getRef :: m a,
        setRef :: a -> m ()
        };

    instance (Monad m) => MonadReference m (Ref m) where
        {
        get = getRef;
        set = setRef;
        };

    instance (Monad m) => SingleMonadReference m (Ref m);

    toRef :: (MonadReference m r) => r a -> Ref m a;
    toRef r = MkRef (get r) (set r);

    refBind :: (MonadReference m r) => (m a) -> (a -> r b) -> Ref m b;
    refBind ma arb = MkRef
        (            ma >>= (\a -> get        (arb a)        ))
        (\b ->        ma >>= (\a -> set        (arb a) b    ));
    
--    ioRef r = MkRef (readIORef r) (writeIORef r);
    }
