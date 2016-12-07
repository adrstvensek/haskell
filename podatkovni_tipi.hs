{-
 - Vaja 3: Podatkovni tipi
 -}


-- Naravna števila
-- ===============

data Naravno = Nic | Nasl Naravno deriving (Show)

-- Sestavite funkcijo sestej, ki vrne vsoto naravnih števil.

sestej :: Naravno -> Naravno -> Naravno
sestej Nic y = y
sestej (Nasl x) y = Nasl (sestej x y) 

-- Sestavite funkcijo zmnoži, ki vrne zmnožek naravnih števil.

zmnozi :: Naravno -> Naravno -> Naravno
zmnozi Nic y = Nic
zmnozi (Nasl x) y = sestej y (zmnozi x y)

-- Sestavite funkcijo vNaravno, ki Integer pretvori v naravno število
--
-- Zgled:
-- ghci> vNaravno 0
-- Nic
-- ghci> vNaravno 2
-- Nasl (Nasl Nic)

vNaravno :: Integer -> Naravno
vNaravno 0 = Nic
vNaravno x = Nasl (vNaravno (x - 1))

-- Sestavite funkcijo izNaravnega, ki naravno število pretvori v Integer
-- 
-- Zgled:
-- ghci> izNaravnega Nic
-- 0
-- ghci> izNaravnega (Nasl (Nasl Nic))
-- 2

izNaravnega :: Naravno -> Integer
izNaravnega Nic = 0
izNaravnega(Nasl x) = 1 + izNaravnega x



-- Drevesa
-- =======

-- Spodaj je definiran rekurzivni podatkovni tip Drevo. Dodali bomo še nekaj
-- funkcij za delo s tipom Drevo. Kot zgled je že definirana funkcija vsota, ki
-- izračuna vsoto vseh elementov v drevesu.

data Drevo a = Prazno | Sestavljeno a (Drevo a) (Drevo a) deriving (Show)

vsota :: Num a => Drevo a -> a
vsota Prazno = 0
vsota (Sestavljeno x levo desno) = x + vsota levo + vsota desno

-- Sestavite funkcijo globina, ki vrne globino drevesa. Prazno drevo ima globino 0.
-- 
-- Zgled:
-- ghci> let d = Sestavljeno 3 (Sestavljeno 7 Prazno (Sestavljeno 2 Prazno Prazno)) (Sestavljeno 8 Prazno Prazno)
-- ghci> globina d
-- 3

globina :: Num a => Drevo a -> Int
globina Prazno = 0
globina (Sestavljeno x levo desno) = 1 + max (globina levo) (globina desno)

-- Sestavite funkcijo steviloElementov, ki vrne število elementov v drevesu.
-- 
-- Zgled:
-- ghci> let d = Sestavljeno 3 (Sestavljeno 7 Prazno (Sestavljeno 2 Prazno Prazno)) (Sestavljeno 8 Prazno Prazno)
-- ghci> steviloElementov d
-- 4

steviloElementov :: Drevo a -> Int
steviloElementov Prazno = 0
steviloElementov (Sestavljeno x levo desno) = 1 + steviloElementov levo + steviloElementov desno

-- Sestavite funkcijo prezrcali, ki drevo prezrcali, tako da pri vsakem vozlišču zamenja levo in desno poddrevo.
-- 
-- Zgled:
-- ghci> let d = Sestavljeno 3 (Sestavljeno 7 Prazno (Sestavljeno 2 Prazno Prazno)) (Sestavljeno 8 Prazno Prazno)
-- ghci> prezrcali d
-- Sestavljeno 3 (Sestavljeno 8 Prazno Prazno) (Sestavljeno 7 (Sestavljeno 2 Prazno Prazno) Prazno)

prezrcali :: Drevo a -> Drevo a
prezrcali Prazno = Prazno
prezrcali (Sestavljeno x levo desno) = Sestavljeno x (prezrcali desno) (prezrcali levo)

-- Sestavite funkcijo, ki vrne najblj levi element v drevesu.
-- Zgled:
-- ghci> let d = Sestavljeno 3 (Sestavljeno 7 Prazno (Sestavljeno 2 Prazno Prazno)) (Sestavljeno 8 Prazno Prazno)
-- ghci> najboljLevi d
-- 7

najboljLevi :: Drevo a -> a
najboljLevi Prazno = undefined
najboljLevi (Sestavljeno x Prazno desno) = x
najboljLevi (Sestavljeno x levo desno) = najboljLevi levo
									



-- Kompleksna števila
-- ==================

-- Definiran je podatkovni tip Kompleksno, ki predstavlja kompleksno število.
-- Dodali bomo še nekaj funkcij za delo s kompleksnimi števili.

data Kompleksno = Kompleksno Double Double deriving (Show)

-- Sestavite funkcijo, ki vrne realni del kompleksnega števila.

re :: Kompleksno -> Double
re (Kompleksno x y) = x

-- Sestavite funkcijo, ki vrne imaginarni del kompleksnega števila.

im :: Kompleksno -> Double
im (Kompleksno x y) = y

-- Sestavite funkcijo, ki izračuna konjugirano kompleksno število.
konjugiraj :: Kompleksno -> Kompleksno
konjugiraj (Kompleksno x y) = Kompleksno x (-y)


-- Polinomi
-- ========

-- Definiran je podatkovni tip Polinom, ki predstavlja polinom nad kolobarjem
-- celih števil. Dodali bomo še nekaj funkcij za delo s polinomi.

data Polinom = Polinom [Rational] deriving (Show)

x :: Polinom
x = Polinom [0, 1]

-- Sestavite funkcijo, ki izračuna polinom v dani točki. 
--
-- Zgled:
-- ghci> let p = Polinom [2,0,-1]
-- ghci> eval p 2
-- -2

eval :: Polinom -> Rational -> Rational
eval (Polinom []) _ = undefined
eval (Polinom y) t = sum $ [k * t^n | (k,n) <- zip y [0,1..]]

-- Sestavite funkcijo, ki izračuna odvod polinoma (v točki x).

odvod :: Polinom -> Polinom
odvod (Polinom [])= undefined
odvod (Polinom y) = Polinom (drop 1 [x * k| (x,k) <- zip [0,1..] y])

-- Sestavite funkcijo, ki izračuna nedoločeni integral polinoma.

integral :: Polinom -> Polinom
integral (Polinom []) = undefined
integral (Polinom y) = Polinom (0 : [k / x | (k,x) <- zip y [1,2..]])
