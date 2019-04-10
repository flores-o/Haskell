myButLast :: [a] -> a
myButLast x = reverse x !! 1

myButLast' :: [a] -> a
myButLast' [] = error "empty list"
myButLast' [x] = error "too few elements"
myButLast' [x, _] = x
myButLast' (x : xs) = myButLast' xs

myButLast'' :: [a] -> a
myButLast'' [] = error "empty list"
myButLast'' [x] = error "too few elements"
myButLast'' (x : xs) = 
  if length xs == 1 then x
  else myButLast'' xs