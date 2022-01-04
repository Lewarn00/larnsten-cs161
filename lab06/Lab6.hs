-- Lewis Arnsten
-- Functional parsing lab

module Lab6 (
  Name,
  Number,
  TopLevelExp(..),
  MathExp(..),
  parse
) where

import           Control.Applicative          hiding (many)
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Ord
import           Text.ParserCombinators.ReadP

type Name   = String  -- Variable names are strings.
type Number = Int     -- The kind of number in our language.

data TopLevelExp
    = MathTLE MathExp
    | LetTLE [Name] [MathExp] MathExp
    deriving (Eq, Show)

data MathExp
    = Number Number
    | Var    Name
    | Neg    MathExp
    | Plus   MathExp MathExp
    | Minus  MathExp MathExp
    | Mult   MathExp MathExp
    | Div    MathExp MathExp
    | Pow    MathExp MathExp
    deriving (Eq, Show)

-- A function to remove spaces and run the string function.
checkS :: String -> ReadP String
checkS str = do
    skipSpaces
    string str

-- A function to parse subtraction.
parseMinus :: ReadP (MathExp -> MathExp -> MathExp)
parseMinus = do
    checkS "-"
    return Minus

-- A function to parse adition.
parsePlus :: ReadP (MathExp -> MathExp -> MathExp)
parsePlus = do
    checkS "+"
    return Plus

-- A function to parse division.
parseDiv :: ReadP (MathExp -> MathExp -> MathExp)
parseDiv = do
    checkS "/"
    return Div

-- A function to parse multiplication.
parseMult :: ReadP (MathExp -> MathExp -> MathExp)
parseMult = do
    checkS "*"
    return Mult

-- A function to parse powers.
parsePow :: ReadP (MathExp -> MathExp -> MathExp)
parsePow = do
    checkS "^"
    return Pow

-- A function to negate an expression.
parseNeg :: ReadP MathExp -> ReadP MathExp
parseNeg exp = do
    checkS "-" 
    e <- exp
    return (Neg e)

-- A function to parse integers.
parseNum :: ReadP MathExp
parseNum = do
    skipSpaces
    x <- munch1 (isDigit)
    return $ Number (read x)

-- A function to parse variable names.
parseName :: ReadP Name
parseName = do
    skipSpaces
    l1 <- satisfy (\c -> isLower c && isAlphaNum c) 
    l2 <- munch (\c -> isLower c || isAlphaNum c)
    return (l1:l2)

-- A function to return a list of variable names.
parseName' :: ReadP [Name]
parseName' = do
    w <- parseName
    return (flip (:) [] w)

-- A function to give a variable the Var type.
parseVar :: ReadP MathExp
parseVar = do
    w <- parseName 
    return (Var w)

-- A function to parse expressions between parenthesis.
parseParen :: ReadP MathExp
parseParen = do
    exp <- between (checkS "(") (checkS ")") parseExpresion
    return (exp)

-- A function to parse expressions by parsing every operator with order of operations.
parseExpresion :: ReadP MathExp
parseExpresion = chainl1 parseHigherPrecedence1 (parseMinus +++ parsePlus)
    where parseHigherPrecedence1 = chainl1 parseHigherPrecedence2 (parseMult +++ parseDiv)
          parseHigherPrecedence2 = parseNeg (chainr1 parseHigherPrecedence3 parsePow) <++ chainr1 parseHigherPrecedence3 parsePow
          parseHigherPrecedence3 = parseNum +++ parseParen +++ parseVar

-- A function to return a list of expressions.
parseExpresion' :: ReadP [MathExp]
parseExpresion' = do
    exp <- parseExpresion
    return (flip (:) [] exp)

-- A function for evaluating inputs without a let statement.
parseMathTLE :: ReadP TopLevelExp
parseMathTLE = do
    exp <- parseExpresion
    return (MathTLE exp)

-- A function for evaluating inputs with a let statement.
parseLetTLE :: ReadP TopLevelExp
parseLetTLE = do
    checkS "let"
    names <- (between (checkS "(") (checkS ")")) (sepBy parseName (checkS ",")) <++ parseName'
    checkS "="
    value <- between (checkS "(") (checkS ")") (sepBy parseExpresion (checkS ",")) <++ parseExpresion'
    checkS "in"
    exp <- parseExpresion
    return (LetTLE names value exp)


parseTLE :: ReadP TopLevelExp
parseTLE = do
    tle <- parseLetTLE +++ parseMathTLE
    return tle

-- Run the parser on a given string.
--
-- You should not modify this function. Grading may
-- look for the specific messages below.
parse :: String -> Either String TopLevelExp
parse str =
    case (completeParses, incompleteParses) of
        ([(result, "")], _  ) -> Right result  -- Only complete result.
        ([]            , [] ) -> Left $ "No parse."
        ([]            , _:_) -> Left $ "Incomplete parse. Unparsed: " ++ show leastRemaining
        (_:_           , _  ) -> Left $ "Ambiguous parse: " ++ show completeParses
    where
        parses = readP_to_S parseTLE str
        (completeParses, incompleteParses) =
            partition (\(_, remaining) -> remaining == "") parses
        leastRemaining = minimumBy (comparing length) . map snd $ incompleteParses
