-- Lewis Arnsten


-- | Different functions for testing whether a number is even.
-- Exercize 4.1

module Even where

-- A function to determine if a number is even.
even :: Integral n => n -> Bool
even n = mod n 2 == 0

-- The even function, but written as a composition.
evenComposition :: Integer -> Bool
evenComposition = (==0) . (\x -> x `mod` 2) 

-- Exercize 4.2
{-
allp :: [(a -> Bool)] -> a -> Bool
allp ps a = and (map ($ a) ps)
allp :: a -> [(a -> Bool)] -> Bool
-}