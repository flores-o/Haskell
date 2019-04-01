myLast :: [a] -> a
myLast [] = error "No last elem for empty lists"
myLast [x] = x
myLast (_:xs) = myLast xs