-- Lewis Arnsten, lab 4
-- Permutation finder.

import Data.Char 
import Data.List
import Control.Category
import qualified Data.Map as Map

-- A function to split a string into a list of words.
splitWords :: [Char] -> [String]
splitWords input = words $ map (\c -> if not (isLetter c || (c == '\'') || (c == '-')) then ' ' else c) input

-- A function to convert all words in a list to lowercase.
lowercase :: [Char] -> [Char]
lowercase cs = [toLower c | c <- cs]

-- A function to convert a string into a list of characters.
toChars :: [Char] -> [[Char]]
toChars word = map (:[]) word 

-- A helper function to find the permutations in a list of strings.
findP :: Num a => [[Char]] -> [Char] -> a -> a
findP [] w n = n
findP (c:cs) w n
    | toChars c == toChars w = findP cs w n
    | sort (toChars c) == sort (toChars w) = (n + 1) + (findP cs w n)
    | otherwise = findP cs w n

-- A function to return all permutations.
findPermutations :: [[Char]] -> [([Char], [Char])]
findPermutations (c:cs) = Map.toAscList $ Map.unionsWith (++) ([(Map.fromList [(sort c, " " ++ c)]) | c <- (c:cs), (findP (c:cs) c 0) /= 0])

-- A function to get the actual string of permutations from the output of findPermutations.
getWords :: [(a1, a2)] -> [a2]
getWords [] = []
getWords (p:ps) = snd p : getWords ps

-- A function to sort the result of getWords.
sortPerms :: [(a, String)] -> [[String]]
sortPerms perms = map sort (map words (getWords perms))

-- A function to convert a list of lists of strings to a list of strings.
toStrings :: [[[Char]]] -> [[Char]]
toStrings perms = map (intercalate ", ") perms

-- A helper function to run all functions.
runFuncs :: [Char] -> String
runFuncs = splitWords >>> map lowercase >>> findPermutations >>> sortPerms >>> map nub >>> toStrings >>> sort >>> unlines

main :: IO()
main = do
    input <- getContents
    putStr $ (runFuncs input)

