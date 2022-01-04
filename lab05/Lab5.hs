import RandState
import System.Environment
import System.IO
import System.Random

-- Data types to represent playing cards
data CardValue
    = King
    | Queen
    | Jack
    | NumberCard Int  -- From 1 to 10
    deriving (Show, Eq)

data CardSuit
    = Hearts
    | Diamonds
    | Spades
    | Clubs
    deriving (Show, Eq)

data PlayingCard =
    PlayingCard CardValue CardSuit
    deriving (Eq)

type Deck = [PlayingCard]


instance Show PlayingCard where
    show (PlayingCard value suit) =
        valueStr value ++ suitStr suit
        where
            suitStr Hearts   = "\x1B[31m♥\x1B[0m" -- red in terminal
            suitStr Diamonds = "\x1B[31m♦\x1B[0m" -- red in terminal
            suitStr Spades   = "♠"
            suitStr Clubs    = "♣"
            -- suitStr Hearts   = "H"  -- uncomment if you don't have Unicode
            -- suitStr Diamonds = "D"
            -- suitStr Spades   = "S"
            -- suitStr Clubs    = "C"
            valueStr King           = "K"
            valueStr Queen          = "Q"
            valueStr Jack           = "J"
            valueStr (NumberCard n) = show n


-- fullCardDeck is a deck of cards, 52 in total, with a King, a Queen,
-- a Jack and NumberCards from 1 to 10 for each suit.
fullCardDeck :: Deck
fullCardDeck =
    [ PlayingCard v s | v <- allVals, s <- allSuits ]
    where
        allVals  = King : Queen : Jack : [ NumberCard i | i <- [1..10] ]
        allSuits = [Hearts, Diamonds, Spades, Clubs]

-- Basic random functions.
randR :: Random a => (a, a) -> RandState a
randR (a1, a2) = do
    gen <- get
    let (x, gen') = randomR (a1,a2) gen
    put gen'
    return x

 -- UNCOMMENT THE FOLLOWING BEFORE SUBMISSION --
 -- A function to return the sum of two random numbers on (1,6).
rollTwoDice :: RandState Int
rollTwoDice = do
    d1 <- randR (1,6)
    d2 <- randR (1,6)
    return (d1 + d2)

-- A function to pick a random number out of a list 
-- and return a tuple with the number and rest of the list.
removeCard :: Eq a => [a] -> RandState (a, [a])
removeCard cards = do
    index <- randR (0, (length cards)-1)
    let randomCard = cards !! index
    let cards' = takeWhile (/= (cards !! index)) cards ++ tail (dropWhile (/= (cards !! index)) cards)
    return (randomCard, cards')    

-- A function to build a new, shuffled deck by putting a random
-- number from the old deck in a random place in the new deck. 
shuffleDeck :: Eq a => [a] -> RandState [a]
shuffleDeck [] = return []
shuffleDeck cards = do
    (randomCard, cards') <- removeCard cards
    deck <- shuffleDeck cards'
    newIndex <- randR (0, (length cards')-1)
    let shuffled = take newIndex deck ++ [randomCard] ++ drop newIndex deck
    return shuffled

-- A function to shuffle a full deck of 52 cards.
shuffleADeck :: RandState [PlayingCard]
shuffleADeck = shuffleDeck fullCardDeck

-- A function to shuffle a full deck N times.
shuffleNTimes :: Int -> StdGen -> IO ()
shuffleNTimes nTimes gen =
    if nTimes > 0 then do
        let (x, gen') = runRandState (shuffleADeck) gen
        putStrLn $ show x
        shuffleNTimes (nTimes - 1) gen'
    else putStrLn ""

-- A function to roll two dice and sum their results N times.
rollTwoDiceNTimes :: Int -> StdGen -> IO ()
rollTwoDiceNTimes nTimes gen =
    if nTimes > 0 then do
        let (x, gen') = runRandState (rollTwoDice) gen
        putStrLn $ show x
        rollTwoDiceNTimes (nTimes - 1) gen'
    else putStrLn ""


-- BESIDES UNCOMMENTING, DO NOT MODIFY BELOW THIS LINE --
usage :: String
usage =
     "Lab 5: Randomizer\n" ++
     "\n" ++
     "$ ./Lab5 shuffle 600      # 600 times: output a full deck shuffle\n" ++
     "$ ./Lab5 rollTwoDice 800  # 800 times: output the sum of rolling two dice\n" ++
     "\n"

main :: IO ()
main = do
     gen  <- newStdGen
     args <- getArgs
     case args of
         ["shuffle",     nTimes] -> shuffleNTimes     (read nTimes) gen
         ["rollTwoDice", nTimes] -> rollTwoDiceNTimes (read nTimes) gen
         _                       -> putStrLn usage
