-- Lewis Arnsten

--Lab03, a simple infix calculator.

module Lab3 where

import Data.List
import Debug.Trace
import Text.Read
import Data.Char
import Data.Maybe

-- A data type to seperate a string into a list.
data Token = TokN Int | Add | Mult | Div | Par [Token] deriving(Show, Eq)
 
-- A function to remove spaces from the input string.
noSpace :: [Char] -> [Char]
noSpace [] = []
noSpace (c:rest) 
    | c == ' '  = noSpace rest
    | otherwise = c : noSpace rest

-- A function to recurse through the input string and turn it into a list of tokens.
tokenize :: [Char] -> [Token]
tokenize [] = []
tokenize s = (fst (nextToken (noSpace s))) : (tokenize (snd (nextToken (noSpace s))))
    where 
        untilCloseParen (')':s) = ([],s)
        untilCloseParen s       = let (t1, rest1) = nextToken s
                                      (t2, rest2) = untilCloseParen rest1
                                  in (t1:t2, rest2)
        nextToken (c:cs)
            | isDigit c = (TokN (read (takeWhile isDigit (c:cs)) ::Int ) , dropWhile isDigit (c:cs)) 
            | c == '-' = (TokN ( - read (takeWhile isDigit cs) ::Int) , dropWhile isDigit cs) 
            | c == '('  = (Par (fst (untilCloseParen cs)) , snd (untilCloseParen cs))
            | c == '+' = (Add, cs)
            | c == '*' = (Mult, cs)
            | c == '/' = (Div, cs)
            | otherwise = (TokN 0, cs)

-- A data type for arithmetic expressions.
data ArithExp = NumberA Int | DivA ArithExp ArithExp | MultA ArithExp ArithExp | Plus ArithExp ArithExp deriving(Show, Eq)

-- A function to turn a list of tokens into an arithmetic expression by 
-- splitting the list at the operator of least precedence and then recursing on the elements.
parse :: [Token] -> ArithExp
parse ((TokN n):[]) = NumberA n
parse ((Par tokens):[]) = parse tokens
parse tokens
    | elem Add tokens = let split1 = (splitAt (fromJust (findIndex (== Add) tokens)) tokens) in
        Plus (parse (fst split1)) (parse (drop 1 (snd split1)))
    | elem Mult tokens = let split2 = (splitAt (fromJust (findIndex (== Mult) tokens)) tokens) in
        MultA (parse (fst split2)) (parse (drop 1 (snd split2)))  
    | elem Div tokens = let split3 = (splitAt (fromJust (findIndex (== Div) tokens)) tokens) in
        DivA (parse (fst split3)) (parse (drop 1 (snd split3)))

-- A function to evaluate an arithmetic expression by applying each operator. 
eval :: ArithExp -> Int
eval (NumberA x) = x 
eval (MultA x y) = (*) (eval x) (eval y)
eval (DivA x y) = quot (eval x) (eval y)
eval (Plus x y) = (+) (eval x) (eval y)

-- A composition of eval, parse, and tokenize.
evaluate :: String -> Int
evaluate = eval . parse . tokenize 
