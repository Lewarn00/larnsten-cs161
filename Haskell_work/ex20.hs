newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- mapMaybeT f = MaybeT . f . runMaybeT

instance (Functor m) => Functor (MaybeT m) where
    fmap f a = MaybeT $ (fmap (fmap f) (runMaybeT a)) 
    -- fmap f = mapMaybeT (fmap (fmap f))
    -- mapMaybeT f = MaybeT . f . runMaybeT
    -- fmap f = MaybeT . (fmap (fmap f)) . runMaybeT

instance Monad m => Applicative (MaybeT m) where
    pure a = MaybeT $ return (Just a) --MaybeT . return . Just 
    mf <*> mx = MaybeT $ do
        mb_f <- runMaybeT mf
        case mb_f of
            Nothing -> return Nothing
            Just f  -> do
                mb_x <- runMaybeT mx
                case mb_x of
                    Nothing -> return Nothing
                    Just x  -> return (Just (f x))