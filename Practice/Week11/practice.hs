data BinaryTree a =
  EmptyTree | Node { root :: a, left :: (BinaryTree a), right :: (BinaryTree a) } deriving (Show, Read, Eq)

testTree = Node 1 (Node 2 (Node 4 EmptyTree EmptyTree) (Node 5 EmptyTree EmptyTree)) (Node 3 EmptyTree EmptyTree)

inorder :: BinaryTree a -> [a]
inorder EmptyTree = []
inorder (Node root EmptyTree EmptyTree) = [root]
inorder binaryTree    = traverseLeft ++ rootValue ++ traverseRight
  where traverseLeft  = inorder (left $ binaryTree)
        rootValue     = [root $ binaryTree]
        traverseRight = inorder (right $ binaryTree)

preorder :: BinaryTree a -> [a]
preorder EmptyTree = []
preorder (Node root EmptyTree EmptyTree) = [root]
preorder binaryTree   =  rootValue ++ traverseLeft ++ traverseRight
  where traverseLeft  = preorder (left $ binaryTree)
        rootValue     = [root $ binaryTree]
        traverseRight = preorder (right $ binaryTree)

postorder :: BinaryTree a -> [a]
postorder EmptyTree = []
postorder (Node root EmptyTree EmptyTree) = [root]
postorder binaryTree  = traverseLeft ++ traverseRight ++ rootValue
  where traverseLeft  = postorder (left $ binaryTree)
        rootValue     = [root $ binaryTree]
        traverseRight = postorder (right $ binaryTree)
        
