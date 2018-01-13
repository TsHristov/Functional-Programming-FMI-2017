data BinaryTree a = EmptyTree | Node { root :: a,
                                       left :: (BinaryTree a),
                                       right :: (BinaryTree a) } deriving (Eq, Show)

-- sumLeaves:
-- Example: sumLeaves (Node 1 (Node 2 EmptyTree EmptyTree) (Node 3 EmptyTree EmptyTree)) -> 5
sumLeaves :: (Num a) => BinaryTree a -> a
sumLeaves EmptyTree = 0
sumLeaves (Node root EmptyTree EmptyTree) = root
sumLeaves (Node root left right) = sumLeaves left + sumLeaves right
