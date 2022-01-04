-- Lewis Arnsten


-- | Functions for manipulating lists.

module ListFunction where

import Prelude hiding (length, product)

-- Exercise 2.1
-- sumf f (c:cs) = f c + sumf f cs
-- sumf (sumf square) [[1,2],[3,4]] = sumf square [1,2] + sumf (sumf square) [3,4]

-- f c = sumf square [1,2] = 1 + sumf square [2] = 1 + (4 + sum square []) = 5 + 0 = 5

-- sumf f cs = sumf (sumf square) [3,4] = sumf square [3,4] + sumf (sumf square) []
-- since sumf f [] = 0, sumf (sumf square) [] = 0
-- sumf square [3,4] = 9 + sumf square [4] = 9 + (16 + sum square []) = 25 + 0 = 25
-- f c + sumf f cs = 5 + (25 + 0) = 30 

-- Exercise 2.2

-- | Square a number.

square :: Num n => n -> n
square x = x^2

-- Given a function f, compute the product of the results of f for every element of a list.

product :: Num n => (a -> n) -> [a] -> n
product f [] = 1
product f (c:cs) = f c * product f cs
-- *ListFunction> product square [1..10]
-- The product of the squares of 1 through 10 is: 13168189440000.

-- Excersise 2.3

-- Determine if one number is divisible by another. 

divisibleBy :: Integral a => a -> a -> Bool
divisibleBy d n = n `mod` d == 0

result = sum . take 100 . filter (divisibleBy 2) . filter (divisibleBy 3) . filter (not . divisibleBy 4) . filter (not . divisibleBy 9) $ [0..]
-- *ListFunction> result
-- 90000

-- Given a list of predicates and a value, determine if that value returns true for every predicate listed.

allp :: [t -> Bool] -> t -> Bool
allp [] x = True
allp (p:ps) x  
    | p x == False     = False
    | otherwise        = allp ps x

result2 = sum . take 100 . filter (allp [divisibleBy 2, divisibleBy 3, not . divisibleBy 4, not . divisibleBy 9]) $ [0..]
-- *ListFunction> result2
-- 90000

-- Filter rewritten to include allp.
filterAll :: [a -> Bool] -> [a] -> [a]
filterAll p [] = []
filterAll p x = filter (allp p) x

result3 = sum . take 100 . filterAll [divisibleBy 2, divisibleBy 3, not . divisibleBy 4, not . divisibleBy 9] $ [0..]
-- *ListFunction> result3
-- 90000

-- Exercise 2.4 
allAtOnce :: [(a -> b)] -> a -> [b]
allAtOnce [] x = []
allAtOnce (p:ps) x = p x : allAtOnce ps x
areAll ps x = and (allAtOnce ps x)
final ps x = and (map ($ x) ps)
