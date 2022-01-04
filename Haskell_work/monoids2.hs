-- Lewis Arnsten

newtype First a = First { getFirst :: Maybe a } deriving (Show)
newtype Last a  = Last  { getLast  :: Maybe a } deriving (Show)
    
instance Semigroup (First a) where
    First Nothing <> b = b 
    a <> _ = a

instance Monoid (First a) where
	mempty = First Nothing
    
instance Semigroup (Last a) where
    a <> Last Nothing = a 
    _ <> a = a

instance Monoid (Last a) where
    mempty = Last Nothing

-- Exercise 13.3
-- Either s t is not an element of the Monoid typeclass
-- because mempty can't be defined for all types for s and t. 

-- Exercise 13.4

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

instance Alternative Maybe where
    empty         = Nothing
    Nothing <|> r = r
    l       <|> _ = l

-- A function that combines a list of alternatives.
altconcat :: Alternative f => [f a] -> f a
altconcat = foldr (<|>) empty 
--altconcat = a (<|>) (altconcat as) 
