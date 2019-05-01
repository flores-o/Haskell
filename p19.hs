-- rotate n steps to the left
rotate :: [a] -> Int -> [a]
rotate x steps 
    |steps > n = rotate x (steps `mod` n)
    |steps < 0 = rotate x (n + steps)             -- n - (abs steps)
    |otherwise = (drop steps x) ++ (take steps x)
    where n = length x