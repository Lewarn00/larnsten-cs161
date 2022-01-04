-- Lewis Arnsten
-- Excersize 17.1 

newtype Parser s = Parser { runParser :: String -> [(s,String)] }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
    [] -> []
    a:as -> [(a,as) | p a]

-- Start 
char1 :: Char -> Parser Char
char1 c = satisfy (c==)

-- Infix -> polish
char2 :: Char -> Parser Char
char2 c = satisfy ((==) c)

-- Create function composition
char3 :: Char -> Parser Char
char3 c = (satisfy . (==)) c

-- Eta reduce
char4 :: Char -> Parser Char
char4 = satisfy . (==)

