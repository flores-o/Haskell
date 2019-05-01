repli :: [a] -> Int -> [a]
repli x count = concatMap (replicate count) x

