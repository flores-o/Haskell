{-|
λ> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
|-}

data EncodedElem z = Multiple Int z | Single z
    deriving Show

encodeDirect ::(Eq a) => [a] -> [EncodedElem a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect' 1 x xs

encodeDirect' n x [] = [encodeElem n x]
encodeDirect' n x y | x == head y = encodeDirect' (n+1) x (tail y)
                    | otherwise   = encodeElem n x : encodeDirect y 

encodeElem 1 x = Single x
encodeElem n x = Multiple n x
