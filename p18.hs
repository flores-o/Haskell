slice' :: [a] -> Int -> Int -> [a]
slice' x l r = take (r-l+1) (drop (l-1) x)

slice'' x l r = [ el | (el, i) <- zip x [1, 2..], l <= i, r >= i]