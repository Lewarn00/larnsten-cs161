
module Intervals where

data Endpoint a
  = MIN
  | E a
  | MAX
  deriving (Eq, Ord) -- Ord lets us use min/max

data Interval a
  = Empty
  | Range (Endpoint a) (Endpoint a)
  deriving (Eq, Ord) -- Ord is for normalizeIS

instance (Show a) => Show (Interval a) where
  show Empty                     = "Empty"
  show (Range MIN MAX)           = "All"
  show (Range MIN (E end))       = "<" ++ show end
  show (Range (E start) MAX)     = ">=" ++ show start
  show (Range (E start) (E end)) = show start ++ "<=_<" ++ show end

instance (Read a) => Read (Interval a) where

  readsPrec _ "Empty"        = [(Empty, "")]
  readsPrec _ "All"          = [(Range MIN MAX, "")]
  readsPrec _ ('>':'=':next) = [(Range (E (read next)) MAX, "")]
  readsPrec _ ('<':next)     = [(Range MIN (E (read next)), "")]
  readsPrec _ str =

    case reads str of
      [] -> error "error parsing interval"
      (start, '<':'=':'_':'<':rest):_ ->
        case reads rest of
          [] -> error "error parsing interval"
          (end,_):_ -> [(Range (E start) (E end), "")]

sanitizeInterval :: Ord a => Interval a -> Interval a
sanitizeInterval int@(Range start end)
  | end <= start = Empty
  | otherwise    = int
sanitizeInterval int = int

intersectIntervals :: Ord a => Interval a -> Interval a -> Interval a
intersectIntervals Empty _ = Empty
intersectIntervals _ Empty = Empty
intersectIntervals (Range start1 end1) (Range start2 end2) = sanitizeInterval $ Range (max start1 start2) (min end1 end2)

---- Interval Sets ----

type IntervalSet a = [Interval a]

toIS :: Interval a -> IntervalSet a
toIS = (:[])

emptyIS :: IntervalSet a
emptyIS = toIS Empty

allIS :: IntervalSet a
allIS = toIS $ Range MIN MAX

intersectISI :: Ord a => IntervalSet a -> Interval a -> IntervalSet a
intersectISI [] n = []
intersectISI (c:cs) n = (intersectIntervals c n) : (intersectISI cs n)

-- The complement of an interval must return an interval set because
-- the complement of a bounded interval is the disjoint union of two
-- unbounded intervals.

complementInterval :: Ord a => Interval a -> IntervalSet a
complementInterval Empty             = [Range MIN MAX]
complementInterval (Range MIN MAX)   = [Empty]
complementInterval (Range MIN e)   = [Range e MAX]
complementInterval (Range s MAX) = [Range MIN s]
complementInterval (Range s e) = [Range MIN s, Range e MAX]

-- A helper function to create the complement of an interval
-- completely in the middle of another.
cutInterval :: Ord a => Interval a -> Endpoint a -> Endpoint a -> [Interval a]
cutInterval (Range s e) s1 e1 = [Range s1 s, Range e e1]

-- An interval minus an interval must return an interval set because
-- the second could cut a hold in the middle of the first.

differenceIntervals :: Ord a => Interval a -> Interval a -> IntervalSet a
differenceIntervals Empty _     = [Empty]
differenceIntervals itvl Empty  = toIS itvl
differenceIntervals (Range start1 end1) (Range start2 end2)
  | start1 >= start2  && end1 <= end2  = [Empty]
  | end2 <= start1 = [(Range start1 end1)] 
  | start2 >= end1 = [(Range start1 end1)]
  | start1 <= start2  && end1 <= end2  = [(Range start1 start2)]
  | start1 >= start2  && end1 >= end2  = [(Range end2 end1)]
  | start1 <= start2  && end1 >= end2  = cutInterval (intersectIntervals (Range start1 end1) (Range start2 end2)) start1 end1 

-- interval set minus an interval
differenceISI :: Ord a => IntervalSet a -> Interval a -> IntervalSet a
differenceISI [] n = emptyIS
differenceISI (x:xs) n = (differenceIntervals x n) ++ (differenceISI xs n)

---- Helpers for interval sets ----

intersection :: Ord a => IntervalSet a -> IntervalSet a -> IntervalSet a
intersection = concatMap . intersectISI

union :: Ord a => IntervalSet a -> IntervalSet a -> IntervalSet a
union = (++)

difference :: Ord a => IntervalSet a -> IntervalSet a -> IntervalSet a
difference = foldr (flip differenceISI) 


---- Queries on interval sets ----

intersectAll :: Ord a => [IntervalSet a] -> IntervalSet a
intersectAll = foldr intersection allIS 

unionAll :: Ord a => [IntervalSet a] -> IntervalSet a
unionAll = concat

-- Subtract from the first interval set all the remaining interval
-- sets.
differenceAll :: Ord a => [IntervalSet a] -> IntervalSet a
differenceAll []           = emptyIS
differenceAll (first:rest) = foldr (flip difference) first rest 


---- Boolean Helpers ----

isEmpty :: Eq a => IntervalSet a -> Bool
isEmpty = null . filter (/= Empty)

-- two interval sets are disjoint if they do not overlap
areDisjoint :: Ord a => IntervalSet a -> IntervalSet a -> Bool
areDisjoint is1 is2 = isEmpty (intersection is1 is2) 

isSubset :: Ord a => IntervalSet a -> IntervalSet a -> Bool
isSubset is1 is2 = isEmpty (difference is1 is2)


areEqual :: Ord a => IntervalSet a -> IntervalSet a -> Bool
areEqual is1 is2 = (isSubset is1 is2) && (isSubset is2 is1)

---- Boolean Queries ----
areAllDisjoint :: Ord a => [IntervalSet a] -> Bool
areAllDisjoint [] = True
areAllDisjoint (first:rest) = (and $ map (areDisjoint first) rest) && (areAllDisjoint rest) 

areAllEqual :: Ord a => [IntervalSet a] -> Bool
areAllEqual [] = True
areAllEqual (first:rest) =  and $ map (areEqual first) rest
