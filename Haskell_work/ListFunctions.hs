module ListFunctions where

import prelude hiding (length)

-- compute length of a list
length :: [a] -> Int
length [] = 0
length (c:cs) = 1 + length cs

-- sum elements of a list
sum :: Num a => [a] -> a
sum [] = 0
sum(a:as) = a + sum as

-- sum of squares
sumOsquares :: Num c => [c] -> c
sumOsquares [] = 0
sumOsquares(a:as) = a^2 + sumOsquares as
