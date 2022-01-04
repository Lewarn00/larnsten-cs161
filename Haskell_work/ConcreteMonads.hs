-- Lewis Arnsten

--Exercise 14.1

newtype Writer w a = Writer { runWriter :: (a,w) } deriving (Show)

instance Functor (Writer w) where
    fmap f (Writer (a,w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
    pure a = Writer (a,mempty)
    (Writer (fa,fw)) <*> (Writer (xa,xw)) = Writer (fa xa, fw <> xw)

-- A monad instance for Writer w.
instance (Monoid w) => Monad (Writer w) where
    return a = Writer (a, mempty)
    Writer (a,w) >>= f = Writer (a', w <> w')
        where (Writer (a',w')) = (f a)
    --Writer (a,w) >>= f = Writer (f a, w `mappend` (f w))

 