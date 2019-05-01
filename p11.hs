--import Encode 
data EncodedElem z = Multiple Int z | Single z
    deriving Show

encode :: [(Int, a)] -> [EncodedElem a]
encode x = foldr func [] x
    where func (fst, snd) acc = if (fst == 1) then (Single snd) : acc else (Multiple fst snd) : acc

-- How to make p12 with foldr ?
-- p12 uncompressed version from the output of p11

{-|
λ> decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
|-}
--decode :: [EncodedElem z] -> [z] 
{-|
decode x = foldr func [] x
    where func (Single z) acc = z : acc
          func (Multiple count z) acc = func (Multiple count-1 z) z:acc
-|}