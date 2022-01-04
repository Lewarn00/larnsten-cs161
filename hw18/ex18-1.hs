-- Lewis Arnsten
-- Exercise 18.1

import Text.ParserCombinators.ReadP

-- A function to parse a sequence of values, returning only maximal sequences.
greedy :: ReadP a -> ReadP [a]
greedy a = readS_to_P $ \s -> filter max (readP_to_S (many a) s)
    where max (_,s') = length (readP_to_S a s') == 0
