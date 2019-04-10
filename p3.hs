elementAt :: [a] -> Int -> a 
elementAt list i = list !! (i-1)

{-- this function doesn't work
elementAt' :: [a] -> Int -> a
elementAt' [] k = error "not enough input elements"
elementAt' (x : xs) k 
    | k = 1     = x
    | otherwise =  elementAt' xs (k-1)
--}
elementAt' :: [a] -> Int -> a
elementAt' [] _     = error "Index out of bounds"
elementAt' (x:_) 1  = x
elementAt' (_:xs) k
  | k < 1           = error "Index out of bounds"
  | otherwise       = elementAt' xs (k - 1)