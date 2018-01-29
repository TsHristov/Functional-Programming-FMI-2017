-- Task 2:

-- a\:
data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Show)

makeLeaf :: a -> BinaryTree a
makeLeaf x = (Node x EmptyTree EmptyTree)

growTree :: BinaryTree a -> a -> BinaryTree a
growTree EmptyTree _                           = EmptyTree
growTree (Node root EmptyTree EmptyTree) value = (Node root (makeLeaf value) (makeLeaf value)) 
growTree (Node root left right) value          = (Node root (growTree left value) (growTree right value)) 
