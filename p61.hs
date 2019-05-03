data Tree a = Empty | Node a (Tree a) (Tree a)
              deriving (Show, Eq)
tree1 = Node 1 (Node 2 Empty Empty)
               (Node 3 (Node 4 Empty Empty) Empty)
tree2 = Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))
               (Node 5 Empty (Node 6 Empty Empty))
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves(Node a Empty Empty) = 1
countLeaves(Node a l r) = countLeaves l + countLeaves r

ex61t1, ex61t2 :: Bool
ex61t1 = countLeaves tree1 == 2
ex61t2 = countLeaves tree2 == 3