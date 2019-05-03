data Tree a = Empty | Node a (Tree a) (Tree a)
              deriving (Show, Eq)
tree1 = Node 1 (Node 2 Empty Empty)
               (Node 3 (Node 4 Empty Empty) Empty)
tree2 = Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))
               (Node 5 Empty (Node 6 Empty Empty))

collectInternal :: Tree a -> [a] 
collectInternal Empty = []
collectInternal (Node x Empty Empty) = []
collectInternal (Node x l r) = [x] ++ collectInternal l ++ collectInternal r


ex62t1, ex62t2, ex62t3 :: Bool
ex62t1 = collectInternal tree1 == [1, 3]
ex62t2 = collectInternal tree2 == [1, 2, 5]
ex62t3 = collectInternal tree1 /= [4]