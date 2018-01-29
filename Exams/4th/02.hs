-- Task 2:

-- a\:

data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Show)

makeLeaf :: a -> BinaryTree a
makeLeaf x = (Node x EmptyTree EmptyTree)

growTree :: BinaryTree a -> a -> BinaryTree a
growTree EmptyTree _                           = EmptyTree
growTree (Node root EmptyTree EmptyTree) value = (Node root (makeLeaf value) (makeLeaf value)) 
growTree (Node root left right) value          = (Node root (growTree left value) (growTree right value))

-- b\: Generate endless stream of complete binary trees with height [1,2,3..]

growingTrees = map (\height -> growTree 0 (2^height)) [0..]
  where growTree n height = if 2^n > height
                            then EmptyTree
                            else (Node (2^n) (growTree (n + 1) height) (growTree (n + 1) height))

