--given the output of p11
{-|
λ> decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
|-}

data EncodedElem z = Multiple Int z | Single z
    deriving Show

decode x = concatMap helper x
    where helper (Single z) = [z]
          helper (Multiple n z) = replicate n z