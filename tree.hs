data Tree a = Empty | Node a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf a = Node a Empty Empty

tree1 = Node 1 (Node 2 Empty Empty)
               (Node 3 (Node 4 Empty Empty) Empty)



mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror Empty _ = False
mirror _ Empty = False
mirror (Node x xl xr) (Node y yl yr) = mirror xl yr && mirror xr yl

ex56t1, ex56t2 :: Bool
ex56t1 = mirror (Node 'x' Empty Empty) Empty == False
ex56t2 = mirror (Node 'x' Empty Empty) (Node 'x' Empty Empty) == True