-- Lewis Arnsten, lab 4
-- Word counter.

import Data.Char 
import Data.List
import Control.Category
import qualified Data.Map as Map

-- A function to split a string into a list of words.
splitWords :: [Char] -> [String]
splitWords input = words $ map (\c -> if not (isLetter c || (c == '\'') || (c == '-')) then ' ' else c) input

-- A function to count the instances when a word occurs. 
countInstances :: (Ord k, Num a) => [k] -> Map.Map k a
countInstances (c:cs) = Map.unionsWith (+) ([(Map.fromList [(c,1)]) | c <- (c:cs)]) 

-- A function to convert all words in a list to lowercase.
lowercase :: [Char] -> [Char]
lowercase cs = [toLower c | c <- cs]

-- A function to turn tuples into strings.
toStrings :: Show a => Map.Map [Char] a -> [[Char]]
toStrings counted = map (\(a,b) -> a ++ " " ++ show b) (Map.toAscList counted) 

-- A helper function to run all required functions.
runFuncs :: [Char] -> String
runFuncs = splitWords >>> map lowercase >>> countInstances >>> toStrings >>> sort >>> unlines 

main :: IO()
main = do
    input <- getContents
    putStr $ (runFuncs input)


