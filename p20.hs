-- remove the kth element from a list
removeAt :: Int -> [a] -> [a]
removeAt n x = [el | (el, i) <- zip x [1, 2..], i /= n]