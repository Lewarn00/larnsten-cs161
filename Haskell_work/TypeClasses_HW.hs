-- Lewis Arnsten


-- | Different functions for evaluating the collatz function.

module TypeClasses where
import Prelude hiding(sum,product,foldr)
import Data.Foldable hiding(foldr)
import Data.List hiding(foldr)

-- Exercize 5.1

-- Defining data type Pair.
data Pair a b = Pair a b

-- Defining an instance of Eq for Pair
instance (Eq a, Eq b) => Eq (Pair a b) where
    Pair a1 b1 == Pair a2 b2 = a1 == a2 && b1 == b2

 -- Defining an instance of Ord for Pair
instance (Ord a, Ord b) => Ord (Pair a b) where
    Pair a1 b1 <= Pair a2 b2 = a1 <= a2 || b1 <= b2


-- Exercize 5.2
instance (Show a, Show b) => Show (Pair a b) where
    show (Pair a b) = ("(" ++ show a ++ "," ++ show b ++ ")")


-- Exercize 5.3

-- Defining a data type for a binary tree
data BinaryTree a
    = EmptyTree
    | Node a (BinaryTree a) (BinaryTree a) -- value leftChild rightChild
    deriving (Show)

-- Produces a list based on a binary tree.
--visit :: BinaryTree a -> [a]
--visit EmptyTree = []
--visit (Node a left right) = visit left ++ [a] ++ visit right

-- Defining the node setup for the binary tree.
tree :: BinaryTree Integer
tree = Node 3
        (Node 1
            (Node 0 EmptyTree EmptyTree)
            (Node 2 EmptyTree EmptyTree))
        (Node 5
            (Node 4 EmptyTree EmptyTree)
            (Node 6 EmptyTree EmptyTree))

foldr :: (a -> b -> b) -> b -> BinaryTree a -> b
foldr combiner base EmptyTree = base
foldr combiner base (Node a left right) = foldr combiner (combiner a (foldr combiner base right)) left

visit :: BinaryTree a -> [a]
visit = foldr (:) []
-- A function to create a mirror-image of the binary tree
reverseTree :: BinaryTree Integer -> BinaryTree Integer
reverseTree EmptyTree = EmptyTree
reverseTree (Node a (l) (r)) = Node a (reverseTree r) (reverseTree l)

reverseTree' :: BinaryTree Integer -> BinaryTree Integer
reverseTree' EmptyTree = EmptyTree
reverseTree' (Node a (l) (r)) = foldr (\l r -> (Node a (reverseTree' r) (reverseTree' l))) EmptyTree (Node a (l) (r))
