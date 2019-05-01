-- drop every n th elementh


drop' x n = foldr (\(el, idx) acc -> if idx `mod` n == 0 then acc else el : acc) [] (zip x [1, 2..])
    
