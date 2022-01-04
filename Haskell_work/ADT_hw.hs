-- Lewis Arnsten


-- | Different functions for evaluating the collatz function.
-- Exercize 3.1

module Collatz where

-- The collatz function written using guards.
collatz :: (Integral a1, Num a2) => a1 -> a2
collatz n
    | n == 1  = 1
    | n `mod` 2 == 0  = 1 + collatz( n `div` 2 )
    | otherwise       = 1 + collatz( 3 * n + 1 )

-- The collatz function written using if... then... else... statements. 
collatz' :: (Integral a1, Num a2) => a1 -> a2
collatz' n = 
    if n == 1 
    then 1
    else 
    if (n `mod` 2) == 0
    then 1 + collatz'( n `div` 2 )
    else 1 + collatz'( 3 * n + 1 )

-- Exercize 3.2

fstOf3 :: (a,b,c) -> a
fstOf3 (a, _, _) = a

sndOf3 :: (a,b,c) -> b
sndOf3 (_, b, _) = b

thirdOf3 :: (a,b,c) -> c
thirdOf3 (_, _, c) = c

curry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
curry3 f (a,b,c) = f a b c

uncurry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
uncurry3 f a b c = f (a, b , c)

-- Exercize 3.4

lookupWithError :: Eq a => a -> [(a,b)] -> Maybe b
lookupWithError _ [] = error "key not found"
lookupWithError a ((k,v):ps)        
    | a == k    = Just v
    | otherwise = lookup a ps

lookupWithDefault :: Eq a => a -> b -> [(a,b)] -> Maybe b
lookupWithDefault _ b [] = Just b
lookupWithDefault a b ((k,v):ps)        
    | a == k    = Just v
    | otherwise = lookup a ps

data Dictionary a b = Dictionary b [(a,b)]

lookupInDictionary :: Eq a => a -> Dictionary a b -> Maybe b
lookupInDictionary _ (Dictionary b []) = Just b
lookupInDictionary a (Dictionary b ((k,v):ps))
    | a == k    = Just v
    | otherwise = lookup a ps

-- Exercize 3.5
-- | A rose tree.
    
data Tree a = Node a (Forest a)
type Forest a = [Tree a]
preorder :: Tree a -> [a]
preorder (Node a []) = [a]
preorder (Node a [as]) = a : preorder as