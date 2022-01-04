-- Lewis Arnsten
-- Exercise 20.2

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- The functor instance for MaybeT.
instance (Functor m) => Functor (MaybeT m) where
    fmap f = MaybeT . (fmap (fmap f)) . runMaybeT

-- The monad instance for MaybeT.
instance Monad m => Applicative (MaybeT m) where
    pure a = MaybeT $ return (Just a) 
    mf <*> ma = MaybeT $ do
        mf' <- runMaybeT mf
        case mf' of
            Nothing -> return Nothing
            Just f  -> do
                ma' <- runMaybeT ma
                case ma' of
                    Nothing -> return Nothing
                    Just a  -> return (Just (f a))