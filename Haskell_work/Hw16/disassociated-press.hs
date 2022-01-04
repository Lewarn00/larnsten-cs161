-- Lewis Arnsten
-- Exercise 16.3

module Main where

import Control.Monad.State
import Data.Map (Map,(!),singleton,unionsWith)
import System.Random

type RandState = State StdGen
type Model = (String,Map String [Maybe String])

buildModel :: [String] -> Model
buildModel xs@(x:_) = (x,unionsWith (++) . transitions $ xs) where
    transitions (y:ys@(y':_)) = singleton y [Just y'] : transitions ys
    transitions [y] = [singleton y [Nothing]]
    transitions [] = error "Impossible error"
buildModel [] = error "Empty model"

runModel :: Model -> RandState [String]
runModel (start,wordmap) = iter start where
    iter word = (word:) <$> do
        maybeNext <- select $ wordmap ! word
        case maybeNext of
            Just nextWord -> iter nextWord
            Nothing -> pure []

roll :: Int -> RandState Int
roll n = state $ randomR (1,n)

select :: [a] -> RandState a
select as = (as !!) . (subtract 1) <$> roll (length as)

linefill :: Int -> [String] -> String
linefill _ [] = "\n"
linefill n (x:xs) = iter x xs where
    iter current (nextWord:ys)
        | length current + length nextWord + 1 > n = current ++ "\n" ++ linefill n (nextWord:ys)
        | otherwise                   = iter (current ++ " " ++ nextWord) ys
    iter current [] = current ++ "\n"

main :: IO ()
main = do
    input <- getContents
    gen <- getStdGen
    let model = buildModel (words input)
        is = replicateM 1000 (runModel model) 
        disassociatedPress = evalState is gen
        lengths = map length disassociatedPress

    putStrLn $ "Maximum:" ++ (show $ maximum lengths)
    putStrLn $ "Minimum:" ++ (show $ minimum lengths)

-- ./disassociated-press < gettysburg.txt
-- Maximum:2245
-- Minimum:7

-- ./disassociated-press < gettysburg.txt
-- Maximum:1824
-- Minimum:7