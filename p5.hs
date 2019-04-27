myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' a = helper a []
    where helper [] reversed = reversed
          helper (x:xs) reversed = helper xs (x : reversed)