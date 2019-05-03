insertAt :: [a] -> Int -> a -> [a]
insertAt x pos el = (take (pos-1) x) ++ [el] ++ (drop (pos-1) x)