import Data.Char 
import Data.List
import Control.Category
import qualified Data.Map as Map

splitWords :: [Char] -> [String]
splitWords input = words $ map (\c -> if not (isLetter c || (c == '\'') || (c == '-')) then ' ' else c) input

countInstances :: [String] -> Int -> String -> Int
countInstances (c:[]) n s 
    | c == s = n + 1
    | otherwise = n 
countInstances (c:cs) n s
    | s == c = (n + 1) + (countInstances cs n s)
    | otherwise = countInstances cs n s

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (w:ws) 
    | elem w ws = removeDuplicates ws
    | otherwise = w : removeDuplicates ws

lowercase :: [Char] -> [Char]
lowercase cs = [toLower c | c <- cs]

applyCount :: [String] -> [Int]
applyCount words = map (countInstances words 0) words


wordsOutput :: [String] -> [(String, Int)]
wordsOutput words = let wordsl = (map lowercase words) in removeDuplicates (zip wordsl (applyCount wordsl)) --nub = removeDuplicates

toStrings :: Show a => [([Char], a)] -> [[Char]]
toStrings counted = map (\(a,b) -> a ++ " " ++ show b) counted --maybe just use counted and call it on (wordsOutput counted) in IO

--sorting :: [String] -> [Char]
--sorting counted = intercalate ", " (sort counted) --call sort in IO
runFuncs :: [Char] -> String
runFuncs = splitWords >>> wordsOutput >>> toStrings >>> sort >>> unlines

main :: IO()
main = do
    input <- getLine
    putStr $ (runFuncs input)