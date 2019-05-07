import Data.List
import Data.Maybe

import qualified Data.Set as Set

type Imagine = Char
type Coloana = Int
type Linie = Int 

type Elem = (Imagine, Linie, Coloana)
linie (_, x, _) = x
coloana (_, _, x) = x
imagine (x, _, _) = x

type Tabla = [Elem]

tabla :: Tabla
tabla = [('A', 1, 1), ('C', 1, 2), ('B', 2, 1), ('C', 2, 2), ('B', 2, 3), ('A', 3, 1)]

--ex1

getChars :: Tabla -> [Imagine]
getChars t = concatMap (\(x, _, _) -> [x]) t

test1 :: Bool
test1 = getChars tabla == ['A', 'C', 'B', 'C', 'B', 'A']

--a = Set.fromList [2, 2, 1, 1]
--b = Set.toList a


imaginiTabla :: Tabla ->[Imagine]
imaginiTabla t = Set.toList (Set.fromList (getChars t))

test2 = imaginiTabla tabla == ['A', 'B', 'C']
--test3 = imaginiTabla tabla == ['A', 'C', 'B']
--Set.fromList gets them sorted

--ex2
par :: Tabla -> Bool
{-|
q = getChars tabla
w = sort q
e = groupBy(\x y -> x == y) w
r = fmap length e
u = fmap (`mod` 2) r
i = sum u
|-}

-- I don't know how to make this work


--r = fmap length e
par x = (f . e . d . c . b . a) x == 0
        where
            a = getChars
            b = sort
            c = groupBy(\x y -> x == y)
            d = fmap length
            e = fmap (`mod` 2)
            f = sum



test4 :: Bool
test4 = par tabla == True

--ex3

type Pozitie = (Linie, Coloana)
type Mutare = (Pozitie, Pozitie)
 

a1 = Set.fromList (fmap (\(_, x, y) ->(x, y)) tabla)
  
verificaPozitii :: Mutare -> Bool
verificaPozitii (p1, p2) = Set.member p1 a1 && Set.member p2 a1 
--test5 = verificaPozitii ((1, 1), (1, 2)) == True

gasesteElement :: Pozitie -> Tabla -> Elem
gasesteElement (p1, p2) t = foldl(\acc (x, y, z) -> if (y == p1 && z == p2) then (x, y, z) else acc) ('a', 0, 0) t

--(pos1, pos2) = mutare
--elem1 = gasesteElement pos1 tabla
--elem2 = gasesteElement pos2 tabla

joacaRunda :: Tabla -> Mutare -> Tabla
joacaRunda t mutare = delete elem1 (delete elem2 t) 
                        where (pos1, pos2) = mutare
                              elem1 = gasesteElement pos1 t
                              elem2 = gasesteElement pos2 t

{-|
joacaRunda' :: Tabla -> Mutare -> Tabla
joacaRunda' t mutare = if (verificaPozitii mutare t == True) then
                           (delete elem1 (delete elem2 t)
                                where (pos1, pos2) = mutare
                                      elem1 = gasesteElement pos1 t
                                      elem2 = gasesteElement pos2 t
                            )
                        else
                            t
|-}