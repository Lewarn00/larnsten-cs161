
-- Lewis Arnsten

-- Exercize 9.1

data BinaryTree a
        = EmptyTree
        | Node a (BinaryTree a) (BinaryTree a)
        deriving (Show)

tree :: BinaryTree Integer
tree = Node 3
        (Node 1
            (Node 0 EmptyTree EmptyTree)
            (Node 2 EmptyTree EmptyTree))
        (Node 5
            (Node 4 EmptyTree EmptyTree)
            (Node 6 EmptyTree EmptyTree))

-- An instance of functor to apply a function to every node of a BinaryTree.
instance Functor BinaryTree where
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)
    fmap f EmptyTree = EmptyTree

-- Exercize 9.3

--instance Functor ((->) a) where
--    fmap = (.)

--fmap :: (b -> c) -> (a -> b) -> (a -> c)