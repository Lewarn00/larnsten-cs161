import Data.Traversable

addLabels :: (Traversable t, Num a) => t b -> t (a, b)
addLabels t = snd $ mapAccumL (\a b -> (a + 1, (a,b))) 1 t
