-- Lewis Arnsten
--Excersize 6.1

-- A module for printing information.

module Main where
    
import System.Environment
import Data.Char

-- A function that capatitalizes the first character of a string.
cap :: [Char] -> [Char]
cap c = Data.Char.toUpper (head c) : drop 1 (take (length c) c)

-- A function that prints a sentence incorporating the users name.
main :: IO ()
main = do
    putStrLn "Hello. I am a HAL 9000 series computer."
    name <- getEnv "USER"
    putStrLn $ "Good morning, " ++ cap name ++ "."