-- Lewis Arnsten

--Excersize 12.1

import System.IO

-- A function to append the index and ". " to a value of a string.
enumerate :: Int -> [String] -> [String]
enumerate _ [] = []
enumerate n (i:is) = (show n ++ ". " ++ i) : (enumerate (n+1) is)

-- A function to start the index at 1.
enum :: [String] -> [String]
enum input = enumerate 1 input		

-- An IO-action to run enumerate on the contents of a file.
main :: IO()
main = do
    input <- getContents
    let output = lines input
    putStr (unlines (enum output))

--putStr $ unlines $ ZipWidth (++) (map show [1..]) (map (". "++) ["some","list"])
