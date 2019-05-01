duplicate :: [a] -> [a]
duplicate x = foldr(\el acc -> el : (el : acc)) [] x
    
-- weird that it works like this, check precedence of :
duplicate' :: [a] -> [a]
duplicate' x = foldr(\el acc -> el : el : acc) [] x