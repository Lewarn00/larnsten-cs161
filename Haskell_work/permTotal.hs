import Data.Char 
import Data.List
import Control.Category
import qualified Data.Map as Map

splitWords :: [Char] -> [String]
splitWords input = words $ map (\c -> if not (isLetter c || (c == '\'') || (c == '-')) then ' ' else c) input

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (w:ws) 
    | elem w ws = removeDuplicates ws
    | otherwise = w : removeDuplicates ws

lowercase :: [Char] -> [Char]
lowercase cs = [toLower c | c <- cs]

toChars :: [Char] -> [[Char]]
toChars word = map (:[]) word 

findPermutations :: [[Char]] -> [Char] -> [[Char]]
findPermutations [] w = []
findPermutations (c:cs) w 
    | sort (toChars c) == sort (toChars w) = c : findPermutations cs w
    | otherwise = findPermutations cs w

--findPermutation :: [[Char]] -> [Char] -> a -> a
findPermutation :: Num p => [[Char]] -> [Char] -> p -> p
findPermutation [] w n = 0
findPermutation (c:cs) w n
    | sort (toChars c) == sort (toChars w) = n + 1 + (findPermutation cs w n)
    | otherwise = findPermutation cs w n

removeDupAndNonPerms :: Num a => [[Char]] -> Map.Map [Char] a
removeDupAndNonPerms (c:cs) = Map.unionsWith (+) ([(Map.fromList [(c,1)]) | c <- (c:cs), (findPermutation cs c 0) > 1])

convertToList perms = let (c:cs) = (Map.toAscList perms) in c : cs -- let (c:cs) = (Map.toAscList perms) in fst c : convertToList cs 

getWords [] = []
getWords (p:ps) = fst p : getWords ps

applyPermutations :: [[Char]] -> [[[Char]]]
applyPermutations input = map (findPermutations input) input 


removeNonPerm :: Foldable t => [t a] -> [t a]
removeNonPerm [] = []
removeNonPerm (p:ps) 
    | length p <= 1 = removeNonPerm ps
    | otherwise = p:removeNonPerm ps
-- = filter (== head (map sort (toChars w))) (map sort cs)
toStrings :: [[[Char]]] -> [[Char]]
toStrings perms = map (intercalate ", ") perms

runFuncs :: [Char] -> String
runFuncs = splitWords >>> map lowercase >>> removeDupAndNonPerms >>> --splitWords >>> map lowercase >>> removeDuplicates >>> applyPermutations >>> removeDuplicates 
           -- >>> removeNonPerm >>> map sort >>> toStrings >>> sort >>> unlines

main :: IO()
main = do
    input <- getContents
    putStr $ (runFuncs input)