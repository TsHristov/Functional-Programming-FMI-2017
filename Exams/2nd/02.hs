data BinaryTree a = EmptyTree | Node { root :: a,
                                       left :: (BinaryTree a),
                                       right :: (BinaryTree a)
                                     } deriving (Show)

isLeaf :: BinaryTree a -> Bool
isLeaf (Node a EmptyTree EmptyTree) = True
isLeaf                           _  = False

makeLeaf :: a -> BinaryTree a
makeLeaf value = Node value EmptyTree EmptyTree

testTree = Node 1 (Node 2 EmptyTree EmptyTree)
           (Node 3 (Node 4 EmptyTree EmptyTree) (Node 5 EmptyTree EmptyTree))

grow :: BinaryTree a -> a -> BinaryTree a
grow EmptyTree _ = EmptyTree
grow tree@(Node root left right) value
  | isLeaf tree = Node root (makeLeaf value) (makeLeaf value)
  | otherwise   = Node root (grow left value) (grow right value)

