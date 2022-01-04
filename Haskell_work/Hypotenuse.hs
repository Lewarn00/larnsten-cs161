-- Lewis Arnsten

-- Exercise 1.3
-- (1 :: Integer) + 2 * (3 :: Double)
-- (*) :: Num a => a -> a -> a
-- (+) :: Num a => a -> a -> a
-- So in both cases the Double will return a type error
-- because (+) and (*) are expecting values a, all of type int

-- Exercise 1.4

-- | A module for working with triangles. 

module Hypotenuse where

-- | Compute the length of the hypotenuse of a triangle from the lengths
--   of its sides.

hypotenuse :: Double -> Double -> Double
hypotenuse a b = sqrt (square a + square b)

-- | Square a number.

square :: Num n => n -> n
square x = x ^ 2

-- | Compute the length of the third side of a triangle given two sides
--	 and an angle (law of cosines). 

law_of_cosines :: Double -> Double -> Double -> Double 
law_of_cosines a b gamma = sqrt (square a + square b - 2 * a * b * cos (gamma * pi / 180))