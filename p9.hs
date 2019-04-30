-- I need a foldr to traverse the list, the acumulator will be a list of lists
-- for every elem, compare it 

packDuplicates x = foldr func [] x
    where func el [] = [[el]]
          func el (y:xs) = if el == (head y) then (el:y):xs else [el]:y:xs