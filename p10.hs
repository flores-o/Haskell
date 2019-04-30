packDuplicates :: Eq a => [a] -> [[a]]
packDuplicates x = foldr func [] x
    where func el [] = [[el]]
          func el (y:xs) = if el == (head y) then (el:y):xs else [el]:y:xs

lengthEncoding :: [[a]] -> [(Int, a)]
lengthEncoding x = foldr (\el acc -> (length el, head el) : acc) [] x

encoding2 x = foldr func [] x
    where func el acc = (length el, head el) : acc