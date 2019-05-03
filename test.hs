import Data.List 
import Data.Char

type Linie = Int
type Coloana = Char
type Pozitie = (Coloana, Linie)

type DeltaLinie = Int 
type DeltaColoana = Int 
type Mutare = (DeltaColoana, DeltaLinie)

charToInt :: Char -> Int
charToInt c = i 
    where [i] = [idx | (el, idx) <- zip ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] [1, 2..], el == c]


intToChar :: Int -> Char
intToChar i = c 
    where [c] = [el | (el, idx) <- zip ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] [1, 2..], idx == i]


mutaDacaValid :: Pozitie -> Mutare -> Pozitie
mutaDacaValid (c, l) (dc, dl)
    | (1 <= linie && linie <= 8 &&
       1 <= colIdx && colIdx <= 8) 
    = ((intToChar colIdx), linie)
    | otherwise 
    =  (c, l)
    where linie = l + dl
          colIdx = charToInt c + dc

ex1t1, ex1t2, ex1t3 :: Bool
ex1t1 = mutaDacaValid ('e', 5) (1, -2) == ('f', 3)
ex1t2 = mutaDacaValid ('b', 5) (-2, 1) == ('b', 5)
ex1t3 = mutaDacaValid ('e', 2) (1, -2) == ('e', 2)

--

mutariPosibile :: [Mutare]
mutariPosibile = [(-2, -1), (-2, 1), (2, -1), (2, 1), (-1, -2), (1, -2), (-1, 2), (1, 2)]

type IndexMutare = Int
type Joc = [IndexMutare]

exJoc :: Joc
exJoc = [0, 3, 2, 7]

type DesfasurareJoc = [Pozitie]


-- get Mutare acording to index, if index out of range return 0, 0 
getMutare :: Int -> Mutare
getMutare n 
    | 0 <= n && n < length mutariPosibile
    = mutariPosibile !! n
    | otherwise 
    = (0, 0)


joaca :: Pozitie -> Joc -> DesfasurareJoc
joaca pos joc = foldl func [pos] joc
    where func acc el 
            | (mutare == (0, 0)) = acc
            | (mutaDacaValid (last acc) mutare == last acc) = acc
            | otherwise = acc ++ [mutaDacaValid (last acc) mutare]
            where mutare = getMutare el

ex2t1, ex2t2, ex2t3 :: Bool
ex2t1 = joaca ('e', 5) [0, 3, 2, 7] == [('e', 5), ('c', 4), ('e', 5), ('g', 4), ('h', 6)]
ex2t2 = joaca ('e', 5) [0, 3, 9, 2, 7] == [('e', 5), ('c', 4), ('e', 5), ('g', 4), ('h', 6)]
ex2t3 = joaca ('a', 8) [0, 3, 2, 7] == [('a', 8), ('c', 7)]
