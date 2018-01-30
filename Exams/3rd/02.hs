-- | Task 2:
-- | Transform a Binary Tree into new Binary Tree where the value of each root
-- | is tuple of the smallest interval contatining all values in the tree:

data BinaryTree a = EmptyTree | Node { root  :: a,
                                       left  :: (BinaryTree a),
                                       right :: (BinaryTree a)
                                     } deriving (Eq, Show)

getValues :: BinaryTree a -> [a]
getValues EmptyTree                    = []
getValues (Node x EmptyTree EmptyTree) = [x]
getValues (Node x l r)                 = getValues l ++ [x] ++ getValues r

intervalTree :: BinaryTree Int -> BinaryTree [Int]
intervalTree EmptyTree                       = EmptyTree
intervalTree (Node root EmptyTree EmptyTree) = (Node [root, root] EmptyTree EmptyTree)
intervalTree (Node x l r)                    = (Node (intervals) (intervalTree l) (intervalTree r))
  where findMin   = minimum $ [x] ++ getValues l
        findMax   = maximum $ [x] ++ getValues r
        intervals = [findMin, findMax]
