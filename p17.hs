split' :: [a] -> Int -> [[a]]
split' x n = [take n x, drop n x]

--split'' :: [a] -> Int -> ([a], [a])
split'' x n = helper x n []
    where helper xs 0 acc = (reverse acc, xs) 
          helper [] _ acc = (reverse acc, [])
          helper (x:xs) n acc = helper xs (n-1) (x:acc) 