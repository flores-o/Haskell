data Tree a = Empty | Node a (Tree a) (Tree a)
              deriving (Show, Eq)
tree1 = Node 1 (Node 2 Empty Empty)
               (Node 3 (Node 4 Empty Empty) Empty)
tree2 = Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))
               (Node 5 Empty (Node 6 Empty Empty))

atLevel :: Tree a -> Int -> [a]
atLevel (a) n = helper (a) n 
    where helper Empty _ = []
          helper (Node x l r) 1 = [x]
          helper (Node x l r) n = helper l (n-1) ++ helper r (n-1)

p62Bt1, p62Bt2, p62Bt3:: Bool
p62Bt1 = atLevel tree1 2 == [2, 3]
p62Bt2 = atLevel tree2 3 == [3, 4, 6]
p62Bt3 = atLevel tree1 4 == []
