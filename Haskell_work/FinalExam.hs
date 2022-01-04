import Prelude hiding(Maybe,Nothing,Just,Either,Right,Left)
import System.IO
import System.Environment
import Control.Applicative hiding (ZipList)

--record syntax 
data ABCD
    = A { foo :: String, bar :: Int }
    | B { foo :: String, baz :: () }
    | C Int
    | D

data Maybe a 
    = Just a 
    | Nothing
    deriving(Show)

--Instance declarations 
instance Functor Maybe where
    fmap f (Just a) = Just (f a)
    fmap _  _ = Nothing

instance Applicative Maybe where 
    (Just a) <*> (Just b) = Just (a b)
    _ <*> _ = Nothing
    pure = Just

instance Monad Maybe where
    (Just x) >>= y = y x
    Nothing >>= _  = Nothing

data Either a b = Left a | Right b deriving(Show)

instance Functor (Either a) where
    fmap f (Right a) = Right (f a)
    fmap _ (Left b) = Left b

instance Applicative (Either a) where
    (Right a) <*> (Right b) = Right (a b)
    (Right _) <*> (Left b)  = Left b
    (Left a)  <*> _         = Left a
    pure a = Right a

instance Monad (Either a) where
    return = Right
    Left  l >>= _ = Left l
    Right r >>= k = k r


data BinaryTree a
    = EmptyTree
    | Node a (BinaryTree a) (BinaryTree a)
    deriving (Show)

instance Functor BinaryTree where
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)
    fmap _ EmptyTree = EmptyTree

instance Applicative BinaryTree where
    (Node f fl fr) <*> (Node a left right) = (Node (f a) (fl <*> left) (fr <*> right))
    (Node _ _ _)   <*> _                   = EmptyTree
    EmptyTree      <*> _                   = EmptyTree
    pure a                                 = (Node a EmptyTree EmptyTree)

instance Monad BinaryTree where 
    return a = (Node a EmptyTree EmptyTree)
    (Node a _ _) >>= f = f a
    EmptyTree >>= _ = EmptyTree

type Pair a b = (,) b a
{-
instance Functor ((,) a) where
    fmap f (a,b) = (a,f b)
-}

liftAN :: Applicative f => ([a] -> b) -> f [a] -> f b 
liftAN = fmap

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

newtype ZipList a = ZipList { getZipList :: [a] } deriving(Show)

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (fmap f xs)

instance Applicative ZipList where
    (ZipList fs) <*> (ZipList xs) = ZipList (zipWith id fs xs)
    pure x = ZipList (repeat x)


{-All combinations of 1,2, and 3 as 3-tuples
[(,,)]<*>[1,2,3]<*>[1,2,3]<*>[1,2,3]    
[(1,1,1),(1,1,2),(1,1,3),(1,2,1),(1,2,2),(1,2,3),(1,3,1),(1,3,2),(1,3,3),
(2,1,1),(2,1,2),(2,1,3),(2,2,1),(2,2,2),(2,2,3),(2,3,1),(2,3,2),(2,3,3),
(3,1,1),(3,1,2),(3,1,3),(3,2,1),(3,2,2),(3,2,3),(3,3,1),(3,3,2),(3,3,3)]
-}
(<$)   :: Functor f => a -> f b -> f a
(<$) = fmap . const
--(<$) b a = fmap (const b) a
(*>):: Applicative f => f a -> f b -> f b
(*>) = fmap . (flip const)
(<*):: Applicative f => f a -> f b -> f a
(<*) = const
(<**>):: Applicative f => f a -> f (a -> b) -> f b
(<**>) = flip (<*>)

plusTimes :: Num a => a -> a -> a -> a
plusTimes a b = (*) ((+) a b)
plusTimes2 :: Num a => a -> a -> a -> a
plusTimes2 a = (*) . ((+) a)

newtype Identity a = Identity { runIdentity :: a }deriving(Show)

instance Functor Identity where
    fmap f m = Identity (f (runIdentity m))

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity x = Identity (f x)
instance Monad Identity where
    return  = Identity 
    m >>= k = k (runIdentity m)

join::Monad m => m(m a) -> m a
join = (>>= id)

do1 = do
    [(),()]
    x <- [1,2,3]
    pure x

do2 = do
    x <- [1,2,3]
    [(),()]
    pure x

notdo1 = [(),()] >>[1,2,3]>>=pure

notdo2 = [1,2,3] >>= \x -> [(),()]>> pure x

number :: Int -> [String] -> [String]
number _ [] = [] 
number n (i:is) = (show n ++ ". " ++ i) : (number (n+1) is)

main :: IO ()
main = do
    args <- getContents
    putStr  (unlines (number 1 (lines args)))

--putStr $ unlines $zipWith (++) (map show [1..]) (map (". "++) ["test","this","is","a","test"])
--1. test
--2. this
--3. is
--4. a
--5. test

newtype First a = First { getFirst :: Maybe a } deriving (Show)
newtype Last a  = Last  { getLast  :: Maybe a } deriving (Show)

instance Semigroup (First a) where
    First Nothing <> b = b
    a <> _ = a

instance Monoid (First a) where
    mempty = First Nothing

instance Semigroup (Last a) where 
    a <> Last Nothing = a
    _ <> b = b

instance Monoid (Last a) where
    mempty = Last Nothing

instance Alternative Maybe where
    empty         = Nothing
    Nothing <|> r = r
    l       <|> _ = l

altconcat :: Alternative f => [f a] -> f a
altconcat (a:as) = a<|>(altconcat as)
--altconcat = foldr (<|>) empty

data UnaryOp
    = Abs
    | Signum
    deriving (Show)

data BinOp
    = Add | Subtract | Multiply
    deriving (Show)

data Expr n
    = Value n
    | ApplyBinary BinOp (Expr n) (Expr n)
    | ApplyUnary UnaryOp (Expr n)
    deriving (Show)
instance Num n => Num (Expr n) where
    (+) = ApplyBinary Add
    (-) = ApplyBinary Subtract
    (*) = ApplyBinary Multiply
    abs = ApplyUnary Abs
    signum = ApplyUnary Signum
    fromInteger = Value . fromInteger

newtype Writer w a = Writer { runWriter :: (a,w) }

instance Functor (Writer w) where
    fmap f (Writer (a,w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
    pure a = Writer (a,mempty)
    (Writer (fa,fw)) <*> (Writer (xa,xw)) = Writer (fa xa, fw <> xw)

instance Monoid w => Monad (Writer w) where
    return a = Writer (a,mempty)
    (Writer (a,w)) >>= f = 
        let (a',w') = runWriter $ f a 
            in Writer (a',w `mappend` w')

newtype Reader e a = Reader {runReader :: e -> a }

instance Functor (Reader e) where
    fmap f (Reader e) = Reader (f . e)

instance Applicative (Reader e) where
    pure e = Reader (const e)
    (Reader f) <*> (Reader g) = Reader (\e -> f e (g e))

instance Monad (Reader e) where
    return e = Reader (const e)
    Reader g >>= f = Reader $ \x ->
        let Reader r = f (g x)
        in r x

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
    fmap f ma = State $ \s ->
        let (a,t) = runState ma s
        in (f a,t)

    --fmap f ma = State $ \s->(\(a,t)->(f a,t))(runState ma s)
    --fmap f ma = State $ (\(a,t) -> (f a,t)) . runState ma
{-
pure a = State $ (,) a
pure a = State $ \s->(a,s)
pure a = State $ \s-> (,) a s
pure a = State $ (,) a
-}

instance Applicative (State s) where
    pure a = State $ \s -> (a,s)
    af <*> aa = State $ \s ->
        let (f,t) = runState af s
            (a,u) = runState aa t
        in (f a, u)

instance Monad (State s) where
    ma >>= f = State $ \s ->
        let (a,t) = runState ma s
            (b,u) = runState (f a) t
        in (b,u)

