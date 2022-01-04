-- Lewis Arnsten
-- Exercise 20.1

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- The functor instance for MaybeT.
instance (Functor m) => Functor (MaybeT m) where
    fmap f = MaybeT . (fmap (fmap f)) . runMaybeT
