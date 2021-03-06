{-
 - Vaja 1: Uvod v Haskell
 -}

-- Funkcija predzadnjiElement vrne predzadnji element seznama l.
predzadnjiElement :: [a] -> a
predzadnjiElement [] = undefined -- baza
predzadnjiElement [_] = undefined -- baza
predzadnjiElement [x,_] = x -- baza
predzadnjiElement (x:xs) = predzadnjiElement xs --rekurzija

-- Funkcija poisci naj poišče k-ti element v seznamu l.
-- Zgled:
-- ghci> poisci 2 [0,0,1,0,0,0]
-- 1
poisci :: Int -> [a]-> a
poisci _ [] = undefined
poisci k (x:xs)
	|k < 0 = undefined
	|k == 0 = x
	|otherwise = poisci (k-1) xs


-- Funkcija podvoji naj 'podvoji' seznam l.
-- Zgled:
-- ghci> podvoji [1,2,3,3]
-- [1,1,2,2,3,3,3,3]
-- Namig: Funkcija concat iz seznama seznamov naredil seznam z elementi podseznamov.
podvoji :: [a] -> [a]
podvoji [] = [] --baza
podvoji (x:xs) = [x,x] ++ podvoji xs --rekurzija

-- Funkcija razdeli k l naj seznam l razdeli na dva seznama. V prvem naj bo prvi k elementov
-- seznama l, v drugem pa vsi ostali. Funkcija naj vrne par teh dveh seznamov.
-- Zgled:
-- ghci> razdeli 2 [1,1,1,2,2,2]
-- ([1,1],[1,2,2,2])
razdeli :: Int -> [a] -> ([a],[a])
razdeli k l
  |k == 0 = ([], l)
  |null l = ([],[])
razdeli k (x:xs) = (x:l1,l2)
  where
    (l1,l2) = razdeli (k-1) xs

-- Funkcija zbrisi naj iz seznama l pobriše k-ti element.
-- Zgled:
-- ghci> zbrisi 3 [0,0,0,1,0,0,0]
-- [0,0,0,0,0,0]
zbrisi :: Int -> [a] -> [a]
zbrisi _ [] = []
zbrisi k (x:xs)
  |k == 0 = xs
  |otherwise = x : (zbrisi (k-1) xs)


-- Funkcija rezina i k l naj sestavi novi seznam, ki naj vsebuje elemente seznama l od vključno
-- i-tega do -- k-tega (brez k-tega).
-- Zgled:
-- ghci> rezina 3 6 [0,0,0,1,2,3,0,0,0]
-- [1,2,3]
rezina :: Int -> Int -> [a] -> [a]
rezina i k [] = []
rezina i k (x:xs)
  | i >= k = []
  | i < 0 = rezina 0 k (x:xs)
  | i == 0 = x:r
  |otherwise = r
  where
    r = rezina (i-1) (k-1) xs

-- Napišite funkcijo "vstavi x k l", ki na k-to mesto seznama l vrine x.
-- Na primer:
-- ghci> vstavi 2 5 [0,0,0,0,0,0]
-- [0,0,0,0,0,2,0]
vstavi :: a -> Int -> [a] -> [a]
vstavi x 0 l = x:l
vstavi x k [] = undefined -- k ni nič
vstavi x k (y:ys) = y : (vstavi x (k-1) ys)


-- Napišite funkcijo "zavrti n l", ki seznam l zavrti za n mest v levo.
-- Na primer:
-- ghci> zavrti 2 [1,2,3,4,5]
-- [3,4,5,1,2]
zavrti :: Int -> [a] -> [a]
zavrti _ [] = []
zavrti 0 l = l
zavrti n (x:xs) = zavrti (n-1) (xs ++ [x])

-- Napišite funkcijo "pobrisi x l", ki vrne seznam, ki je kot l,
-- le da smo iz njega pobrisali vse pojavitve elementa x
-- Na primer:
-- ghci> pobrisi 'a' "abrakadabra"
-- "brkdbr"
pobrisi :: Eq a => a -> [a] -> [a]
pobrisi _ [] = []
pobrisi x (y:ys)
  |x == y = pobrisi x ys
  |otherwise = y : (pobrisi x ys)


-- Funkcija jePalindrom naj ugotovi, če je seznam l palindrom.
-- Zgled:
-- ghci> jePalindrom [1,2,3,2,1]
-- True
-- ghci> jePalindrom [1,2,2,1]
-- True
-- ghci> jePalindrom [1,2,3]
-- False
jePalindrom l = undefined

-- Napišite funkcijo "maxPoKomponentah l1 l2", ki vrne seznam, ki ima za
-- elemente večjega od elementov na ustreznih mestih v seznamih l1 in l2.
-- Če je eden od seznamov krajši, naj bo krajši tudi vrnjeni seznam.
-- Na primer:
-- ghci> maxPoKomponentah [1,10,5,6] [2,3,7,4,8]
-- [2,10,7,6]
maxPoKomponentah :: Ord a => [a] -> [a] -> [a]
maxPoKomponentah [] l2 = []
maxPoKomponentah l1 [] = []
maxPoKomponentah (x:xs) (y:ys)
  |x >= y = x : maxPoKomponentah xs ys
  |otherwise = y : maxPoKomponentah xs ys

-- Napišite funkcijo "drugiNajvecji l", ki vrne drugi največji element
-- seznama l. Predpostavite lahko, da sta v l vsaj dva različna elementa.
-- Na primer:
-- ghci> drugiNajvecji [1,10,5,6]
-- 6
drugiNajvecji l = undefined
