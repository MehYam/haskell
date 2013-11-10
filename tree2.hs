data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

-- pretend we don't know about Functors yet
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node left right) = Node (treeMap f left) (treeMap f right)

-- now implement the Functor for Tree, simply using the treeMap we've already implemented
instance Functor Tree where
	fmap = treeMap

sampleTree = Node (Node (Leaf "foo") (Leaf "bar")) (Node (Leaf "blah") (Leaf "blar"))

-- now you can do:
--treeMap length sampleTree
--fmap length sampleTree