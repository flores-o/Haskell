import Data.List 

type Linie = Integer
type Coloana = Integer

-- piese unite <=> l1 = l2 || c2 = c1 + 1
type Partida = [(Linie, Coloana)]

exemplu1 :: Partida
exemplu1 = [ (2, 2), (1, 3), (2, 3), (2, 4), (3, 5), (0, 2), (2, 1), (1, 4), (2, 0), (1, 2), (3, 1), (1, 0)]


--ex1
test11 :: Bool
test11 = separaX0 exemplu1
            == ( [(2,2),(2,3),(3,5),(2,1),(2,0),(3,1)]
            , [(1,3),(2,4),(0,2),(1,4),(1,2),(1,0)])

separaX0 :: [a] -> ([a], [a])

separaX0 partida = (jucator1, jucator2)
        where jucator1 = [el | (el, idx) <- zip partida [0, 1..], idx `mod` 2 == 0]
              jucator2 = [el | (el, idx) <- zip partida [0, 1..], idx `mod` 2 == 1]

--ex2
--test12 :: Bool
--test12 = maxLista [[1,2,3], [4,5], [6], [7, 8, 9, 10], [11, 12, 13]] == [7, 8, 9, 10]

--maxLista :: [[a]] -> [a]
--maxLista = undefined

{-|
lista = [[1,2,3], [4,5], [6], [7, 8, 9, 10], [11, 12, 13]]

--lungimi x = fmap length x

lungimi = fmap length lista

maxm = maximum lungimi 

listaAugmentata  = zip lista lungimi 

elemMax  = ([el | (el, lung) <- listaAugmentata, lung == maxm]) !! 0

--test12 :: Bool
--test12 = elemMax == [7, 8, 9, 10]
|-}

lungimi x = fmap length x

maxm x = maximum (lungimi x)

listaAugmentata x = zip x (lungimi x)

elemMax x = ([el | (el, lung) <- (listaAugmentata x), lung == maxm x]) !! 0

test12 :: Bool
test12 = elemMax [[1,2,3], [4,5], [6], [7, 8, 9, 10], [11, 12, 13]] == [7, 8, 9, 10]

--gasesteIndexHelper target x = foldl func (-1, 0) x
--    where func (fst, snd) el = if (el == target && fst == -1) then (snd, snd) else (fst, snd + 1)

--gasesteIndex target x = index
--  where (index, _) = gasesteIndexHelper target x

--gasesteIdxMax x = gasesteIndex (maximum (lungimi x)) x

--elemMax x = x !! (gasesteIdxMax x)
--lung = lungimi lista
--listaAugmented = zip lista lung

-- ex 3

test130, test13X :: Bool
test130 = grupeazaUnite (sort [(1,3),(2,4),(0,2),(1,4),(1,2),(1,0)]) == [[(0,2)],[(1,0)],[(1,2),(1,3),(1,4)],[(2,4)]]
test13X = grupeazaUnite (sort [(2,2),(2,3),(3,5),(2,1),(2,0),(3,1)]) == [[(2,0),(2,1),(2,2),(2,3)],[(3,1)],[(3,5)]]

grupeazaUnite :: Partida -> [Partida]
grupeazaUnite = foldr func [] 
                    where func el [] = [[el]]
                          func el acc@((h:t):rest) = if (check el h == True) then (el:h:t):rest
                                                     else [el] : (h:t):rest --acc


--check :: Partida -> Partida -> Bool
-- put the abs here 
check (t0, t1) (t2, t3) = (t0 == t2 && (t1 + 1) == t3)

--appendLast :: a -> [a] -> [a]
--appendLast x [] = [x]
--appendLast x (h:t) = h : (appendLast x t)



lista = sort [(1,3),(2,4),(0,2),(1,4),(1,2),(1,0)]

a = foldr func [] lista
    where func el [] = [[el]]
          func el acc@((h:t):rest) = if (check el h == True) then (el:h:t):rest
                                     else [el] : (h:t):rest --acc

--ex 4
test14 :: Bool
test14 = maxInLinie exemplu1 == ([(2,0),(2,1),(2,2),(2,3)], [(1,2),(1,3),(1,4)])

--toList' :: (a, a) -> [a, a]

toList (x, y) = [x, y]

toTuple [x, y] = (x, y)

maxInLinie :: Partida -> (Partida, Partida)
maxInLinie partida = (maxSecventa fst, maxSecventa snd) 
    where (fst, snd) = separaX0 partida
          maxSecventa = elemMax . grupeazaUnite . sort
