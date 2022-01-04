--Lewis Arnsten

--Exercise 11.2

toListA :: [Integer]
toListA = do
    [(),()]
    x <- [1,2,3]
    pure x 


toListB :: [Integer]
toListB = do
    x <- [1,2,3]
    [(),()] --might as well be Xs
    pure x --takes out x,x , inputs 
 
 x <- [1,2,3] -- takes out 1,2,3 to be applied to next function
 \x -> [(),()] >> pure x -- this indicates that each x will be put into the structure [x,x]
 -- pure creates a list [[1,1],[2,2],[3,3]] which is concatonated to [1,1,2,2,3,3] by the monad.
-- bind for lists flattens the list with concatmap

--rewritting the second
hello = concat[[(a),(a)] | a <- [1..3]]
hello2 = [1,2,3] >>= \x -> [x,x] >>= pure
hello3 = [1,2,3] >>= \x -> [(),()] >> pure x --puts each x into the structure of [(),()] and then concatonates them all

--rewritting the first
ayy1 = concat [[1,2,3] | _ <- [1,2]]
toListC :: [Integer]
toListC = [(),()] >>= \_ -> [1,2,3] >>= pure --puts [1,2,3] into each element of [(),()]
ayy3 = [(),()] >> [1,2,3] >>= pure -- take out each element, so 1 then 2 then 3, wrap with list, [1] [2] [3], concatmap with [(),()], so [[1,2,3],[]] 