{-# LANGUAGE NoMonomorphismRestriction #-}
module Tree where

data  BST a = Tree (BST a) a (BST a)
    | Empty deriving (Eq)

treeInsert :: Ord a => a -> BST a -> BST a
treeInsert x Empty = Tree Empty x Empty
treeInsert x (Tree left a right)
  | x < a     = Tree (treeInsert x left) a right
  | otherwise = Tree left a (treeInsert x right)

fromList :: Ord a => [a] -> BST a
fromList xs = foldr treeInsert Empty xs

tree1 :: BST Int
tree1 = fromList [1, 5, 2, 3, 6, 7]

tree2 :: BST Int
tree2 = fromList [6, 23, 54, 31, 5, 89, 24]

mapTree :: (a -> b) -> BST a -> BST b
mapTree f Empty = Empty
mapTree f (Tree left a right) = Tree (mapTree f left) (f a) (mapTree f right)

foldTree :: (a -> b -> b) -> b -> BST a -> b
foldTree f b Empty = b
foldTree f b (Tree left a right) = foldTree f rightFolded left
  where rightFolded = f a (foldTree f b right)

toList :: Ord a => BST a -> [a]
toList t = foldTree (:) [] t

treeSort :: Ord a => [a] -> [a]
treeSort = toList . fromList

joinTrees :: Ord a => BST a -> BST a -> BST a
joinTrees = foldTree treeInsert

-- EXTRAS

instance Show a => Show (BST a) where
  show = showTree

showTree :: Show a => BST a -> String
showTree = unlines . treeIndent

treeIndent :: Show a => BST a -> [String]
treeIndent Empty = ["-- (nil)"]
treeIndent (Tree r a l) = ["--" ++ show a]
  ++ map ("  |" ++) ls ++ ("  `" ++ r') : map ("   " ++ ) rs
  where
  (r':rs) = treeIndent r
  ls      = treeIndent l