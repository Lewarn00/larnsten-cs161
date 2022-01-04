module Parser where

import Control.Applicative
import Control.Monad
import Data.Char 

newtype Parser a = Parser { runParser :: String -> [(a,String)] }

{- primitive parsers -}

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
    [] -> []
    a:as -> [(a,as) | f a]

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string str =
    Parser $ \s ->
        [ (t,u)
        | let (t,u) = splitAt (length str) s
        , str == t
        ]

{- instance definitions for Parser -}

instance Functor Parser where
    fmap f p = Parser $ \s ->
        [ (f a,t)
        | (a,t) <- runParser p s
        ]
    
instance Applicative Parser where
    pure a = Parser $ \s -> [(a,s)]
    af <*> aa = Parser $ \s ->
        [ (f a,u) 
        | (f,t) <- runParser af s
        , (a,u) <- runParser aa t
        ]

instance Alternative Parser where
    empty = Parser $ \s -> []
    p1 <|> p2 = Parser $ (++) <$> runParser p1 <*> runParser p2
    
instance Monad Parser where
    ma >>= f = Parser $ \s ->
        [ (b,u) 
        | (a,t) <- runParser ma s
        , (b,u) <- runParser (f a) t
        ]

data ComplexInt = ComplexInt Int Int
        deriving (Show)

digit :: Parser Char
digit = satisfy isDigit 

space :: Parser Char
space = satisfy isSpace

comma :: Parser Char
comma = satisfy (==',')

parenO :: Parser Char
parenO = satisfy (=='(')

parenC :: Parser Char
parenC = satisfy (==')')

parseInt :: Parser Int
parseInt = read <$> some digit

skipParenO :: Parser ()
skipParenO = const () <$> parenO

skipParenC :: Parser ()
skipParenC = const () <$> parenC

skipComma :: Parser ()
skipComma = const () <$> comma

skipSpaces :: Parser ()
skipSpaces = const () <$> many space

--parseCompInt = liftA3 (\_ _ i -> (ComplexInt i 0)) (string "ComplexInt") skipSpaces parseInt
parseCompInt :: Parser ComplexInt
parseCompInt = do
					skipSpaces
					i <- parseInt
					pure (ComplexInt i 0) 


parseCompPair :: Parser ComplexInt
parseCompPair = do 
					skipSpaces
					skipParenO
					i1 <- parseInt
					skipComma
					i2 <- parseInt
					skipParenC
					pure (ComplexInt i1 i2) 


instance Read ComplexInt where
    readsPrec _ s  
    	| elem '(' s  = runParser parseCompPair s 
    	| otherwise  = runParser parseCompInt s 