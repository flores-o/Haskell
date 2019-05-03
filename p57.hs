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

add :: Ord a => a -> Tree a -> Tree a
add x Empty      = Node x Empty Empty
add x t@(Node y l r)  = case compare x y of 
                            LT -> Node y (add x l) r    
                            GT -> Node y l (add x r)
                            EQ -> t

construct xs = foldl (flip add) Empty xs

ex57t1, ex57t2, ex57t3:: Bool
ex57t1 = mirror l r == True 
            where (Node x l r) = construct $ [5, 3, 18, 1, 4, 12, 21] 
ex57t2 = mirror l r == True
            where (Node x l r) = construct $ [3, 2, 5, 7, 1]
ex57t3 = mirror l r == False
            where (Node x l r) = construct $ [1, 2, 3, 4, 5, 6]

