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
        
--  All nodes at a given level:
allNodesAtLevel :: BinaryTree a -> Int -> [a]
allNodesAtLevel EmptyTree _ = []
allNodesAtLevel binaryTree 0 = [root $ binaryTree]
allNodesAtLevel binaryTree level = allNodesInLeft ++ allNodesInRight
  where allNodesInLeft  =  allNodesAtLevel (left $ binaryTree) (level-1)
        allNodesInRight =  allNodesAtLevel (right $ binaryTree) (level-1)

-- Maps a function over the binary tree:
mapTree :: (a -> a) -> BinaryTree a -> BinaryTree a
mapTree _ EmptyTree = EmptyTree
mapTree function binaryTree = Node mapRoot mapLeft mapRight
  where mapRoot  = function $ root $ binaryTree
        mapLeft  = mapTree function (left $ binaryTree)
        mapRight = mapTree function (right $ binaryTree)

-- Check if a path in the tree exists:
pathExists :: (Eq a) => BinaryTree a -> [a] -> Bool
pathExists   EmptyTree [] = True
pathExists           _ [] = False
pathExists EmptyTree path = False
pathExists (Node root left right) (x:xs)
  | x == root = pathExists left xs || pathExists right xs
  | x /= root = False


