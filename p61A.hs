data Tree a = Empty | Node a (Tree a) (Tree a)
              deriving (Show, Eq)
tree1 = Node 1 (Node 2 Empty Empty)
               (Node 3 (Node 4 Empty Empty) Empty)
tree2 = Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))
               (Node 5 Empty (Node 6 Empty Empty))

collectLeaves :: Tree a -> [a]
collectLeaves Empty = []
collectLeaves (Node x Empty Empty) = [x]
collectLeaves (Node x l r) = collectLeaves l ++ collectLeaves r

ex62t1, ex62t2 :: Bool
ex62t1 = collectLeaves tree1 == [2, 4]
ex62t2 = collectLeaves tree2 == [3, 4, 6]