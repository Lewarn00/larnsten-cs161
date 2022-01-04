import Text.ParserCombinators.ReadP

greedy :: ReadP a -> ReadP [a]
greedy a = readS_to_P $ \s -> filter max (readP_to_S (many a) s)
    where max (_,s') = length (readP_to_S a s') == 0

-- import Control.Monad
-- greedy p = liftM2 (:) p (geedy p) <++ return []
--greedy1 a = many $ satisfy a

--satisfy (/= '"') <++ 
--many $ satisfy (=='a')
