-- Chapter 8

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a 
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node t left right) 
	| x == t = Node x left right
	| x > t  = Node t left (treeInsert x right)
	| x < t  = Node t (treeInsert x left) right


treeElem :: (Ord a) => a -> Tree a -> Bool 
treeElem x EmptyTree = False
treeElem x (Node a left right) 
	| x == a = True
	| x  < a = treeElem x left
	| x  > a = treeElem x right
