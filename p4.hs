myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength(xs)

myLength' :: [a] -> Int
myLength' x = case x of [] -> 0
                        (_: xs) -> 1 + myLength' xs