-- Lewis Arnsten

liftAN :: Applicative f => ([a] -> b) -> f [a] -> f b
liftAN  = fmap

-- Exercise 10.2

instance Applicative (Either a) where
	pure a = Right a
	Right f <*> Right a = Right (f a)
	Right _ <*> Left a = Left a
	Left a <*> _ = Left a


-- Exercise 10.3

-- There is no Applicative instance for (,) a because (,) a 
-- has a variable of undetermined type and is not necesarily a functor.

-- Exercise 10.5

-- The line below applies every function in the list of functions
-- to every combination of elements in two lists. Therefore every element 
-- of the first list will be added and multiplied with every element of the second.

-- > [(+),(*)] <*> pure 2 <*> pure 3
--    [5,6]

-- The line below uses ZipList to apply each of the given operators 
-- to every element in both lists using the pattern: 
-- operator 1st element 1st element, operator 2nd element 2nd element, ...

-- > ZipList [(+),(*)] <*> pure 2 <*> pure 3
--    ZipList {getZipList = [5,6]}


-- Exercise 10.6
(<$)   :: Functor f => a -> f b -> f a
(<$) a b = fmap (const a) b

(*>)   :: Applicative f => f a -> f b -> f b
(*>) a b = b -- (\b a -> b) 

(<*)   :: Applicative f => f a -> f b -> f a
(<*) = const 

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = flip (<*>)
