{-# LANGUAGE MultiParamTypeClasses #-}


-- Kompleksna števila
-- ==================
-- 
-- Pokažite, da tipi Naravno, Kompleksno in Polinom s prejšnjih vaj pripadajo
-- razredu Num:

data Naravno = Nic | Nasl Naravno deriving (Show)

data Kompleksno = Kompleksno Double Double

data Polinom = Polinom [Rational] deriving (Show)

instance Num Naravno where
  Nic + n = n
  (Nasl m) + n = Nasl (m + n)
  Nic * n = Nic
  (Nasl m) * n = n + m * n
  abs n = n
  signum n = 1
  fromInteger 0 = Nic
  fromInteger m = Nasl (fromInteger(m - 1))
  negate n = undefined

instance Num Kompleksno where
    (Kompleksno x1 y1) * (Kompleksno x2 y2) = Kompleksno (x1 * x2 - y1 * y2) (x1 * y2 + y2 * x1)
    negate (Kompleksno x1 y1) = Kompleksno (- x1) (- y1)
    (Kompleksno x1 y1) + (Kompleksno x2 y2) = Kompleksno (x1 + x2) (y1 + y2)
    abs (Kompleksno x1 y1) = (Kompleksno ((x1^2 + y1^2)**(0.5)) 0)
    signum (Kompleksno x1 y1) = undefined
    fromInteger n = Kompleksno (fromIntegral n) 0
    
    

instance Num Polinom where
    Polinom [] + Polinom xs = Polinom xs
    Polinom [x] + Polinom (y:ys) = Polinom ((x + y):ys)
    Polinom (x:xs) + Polinom (y:ys) = Polinom [x + y] + (Polinom xs + Polinom ys)
    Polinom [] * Polinom xs = Polinom []
    Polinom [x] * Polinom ys = Polinom [x * y | y <- ys]
    Polinom [0, 1] * Polinom [] = Polinom []
    Polinom [0, 1] * Polinom xs = Polinom (0:xs)
    Polinom (x:xs) * Polinom (y:ys) = Polinom [x] * Polinom (y:ys) + (Polinom [0, 1])*(Polinom xs * Polinom ys)
    abs (Polinom xs) = undefined
    signum = error "Polinom: operacija signum nima smisla"
    fromInteger n = Polinom [fromIntegral n]
    negate (Polinom xs) = Polinom [- x | x <- xs]
--Polinom [x] * Polinom ys = Polinom [x * y | y <- ys]
--Polinom (x:xs) * Polinom ys = Polinom [x] * Polinom ys + Polinom (0:zs) 
--    where
--        Polinom zs = Polinom xs * Polinom ys

-- Algebraične stukture
-- ====================

-- Razred Polgrupa lahko definiramo na sledeč način:

class  Polgrupa a  where
    (***) :: a -> a -> a

-- Definirajte še naslednje razrede:
--
-- PolgrupaZEnoto (s posebno vrednostjo "enota")
class (Polgrupa a) => PolgrupaZEnoto a where
    enota :: a
--
-- Grupa (s posebno vrednostjo "inv")
class (PolgrupaZEnoto a) => Grupa a where
    inv :: a -> a
-- Kolobar
class (Grupa a) => Kolobar a where
    (###) :: a -> a -> a --množenje


-- Pokažite, da cela števila pripadajo razredu Kolobar. 
instance Polgrupa Integer where
    n *** m = n + m
instance PolgrupaZEnoto Integer where
    enota = 0
instance Grupa Integer where
    inv n = - n
instance Kolobar Integer where
    n ### m = n * m

-- Pokažite, da tip Bool pripada razredu Grupa.
instance Polgrupa Bool where
    True *** True = True
    False *** False = True
    _ *** _ = False
instance PolgrupaZEnoto Bool where
    enota = True
instance Grupa Bool where
    inv False = False
    inv True = True

-- Pokažite, da podatkovni tip Z_2 (definiran spodaj) pripada razredu Grupa.

data Z_2 =  Nicla | Ena deriving (Show)

instance Polgrupa Z_2 where
    Nicla *** Nicla = Nicla
    Ena *** Ena = Nicla
    _ *** _ = Ena
instance PolgrupaZEnoto Z_2 where
    enota = Nicla
instance Grupa Z_2 where
    inv Nicla = Nicla
    inv Ena = Ena

-- Pokažite, da kartezični produkt tipov v razredu Grupa pripada razredu Grupa.
instance (Polgrupa a, Polgrupa a) => Polgrupa (a, b) where
    (x1, y1) *** (x2, y2) = (x1 *** x2, y1 *** y2)
--instance (Grupa a, Grupa b) => Grupa (a, b) where
    --inv = (e, enota)

-- Naj tipa a in b pripadata razredu Grupa. Če želimo povedati, da sta grupi a
-- in b izomorfni, lahko uporabimo razred Izomorfno:

class  Izomorfno a b  where
    naprej :: a -> b
    nazaj :: b -> a

-- Pokažite, da je grupa Bool izomorfna grupi Z_2.


-- Porazdelitve
-- ============

-- Podatkovni tip Porazdelitev a je definiran takole:

data Porazdelitev a = Porazdelitev [(a, Rational)] deriving Show

-- Dva enostavna primera:

kovanec = Porazdelitev [("cifra", 1/2), ("grb", 1/2)]
kocka = Porazdelitev [(1, 1/6), (2, 1/6), (3, 1/6), (4, 1/6), (5, 1/6), (6, 1/6)]

-- Sestavite funkcijo jePorazdelitev, ki preveri, če je porazdelitev zares
-- porazdelitev, tj. če se verjetnosti seštejejo v 1.

jePorazdelitev = undefined

-- Funkcija urediPorazdelitev naj "uredi" porazdelitev,
-- tj. dogodke naj uredi in združi skupaj enake dogodke.

urediPorazdelitev = undefined

-- Funkcija najverjetnejsi naj vzame porazdelitev in vrne
-- najverjetnejši izid (če jih je več, tistega, ki je najbolj desno).

najverjetnejsi = undefined

-- Funkcija enakomerna naj vzame seznam in vrne enakomerno porazdelitev, kjer so
-- dogodki elementi tega seznama.

enakomerna = undefined

-- Funkcija upanje naj dobi slučajno spremenljivko, kot funkcijo iz a v Rational
-- in naj vrne matematično upanje (=pričakovana vrednost).

upanje = undefined    

-- Funkcija utezenaVsota na vzame neko decimalno število p med 0 in 1 in vrne
-- uteženo vsoto dveh porazdelitev, kjer je prva utežena s p, druga pa z
-- (1 - p). Predpostavimo, da sta obe porazdelitvi urejeni in ju lahko
-- združimo z zlivanjem.

utezenaVsota = undefined 

-- Pokažite, da konstruktor tipov Porazdelitev pripada razredu Functor.

instance  Functor Porazdelitev  where
    fmap = undefined


-- Premikanje v prostoru
-- =====================

-- Definirajte podatkovni tip Tocka, ki naj vsebuje parametre {ime, koordinataX,
-- koordinataY, koordinataZ}. Implementirajte funkcijo show za tip Tocka. 


-- Točka se bo premikala v prostoru. Da se premikanje lahko začne, moramo točko
-- najprej poiskati. V ta namen definirajte razred tipov Pozicioniran (z
-- vrednostjo pridobiLokacijo) in pokažite, da Tocka pripada tipu Pozicioniran.


-- Definirajte razred tipov Premakljiv (z vrednostjo nastaviNovoLokacijo) in
-- pokažite, da Tocka pripada tipu Premakljiv.


-- Za spremenljivke, katerih tip pripada razredu Premakljiv, definirajte
-- funkcijo premakniZa, ki spremenljivko premakne za določen vektor. Ta funkcija
-- bo tako delovala tudi za poljubno točko!


-- Znano je, da samice mnogih vrst pajkov po parjenju pojejo samca. Samice
-- pajkov, ki pojedo samca, bodo izlegle več jajčec, nov zarodek pa bo močnejši
-- in večji. Ena izmed teorij, zakaj je temu tako, se veže na to, da so takšne
-- samice bolj agresivne in so zato tudi boljše pri lovu. Druga teorija pa
-- ugotavlja, da so samci odličen vir pomembnih hranljivih snovi.
--
-- Predstavljaj si samico, locirano v koordinati (0,0,0). Samica želi pojesti
-- samca, ki je trenutno lociran v koordinati (3,3,3). Predpostavimo, da sta
-- pajka omejena na gibanje v kocki velikosti 10x10x10 (torej je vsaka
-- koordinata vedno med 0 in 9).
--
-- Samica in samec se premikata po potezah.  Najprej se premakne samica, ki se
-- želi čim bolj približati samcu.  Potem se premakne samec, ki se želi čim bolj
-- oddaljiti od samice.
--
-- Sestavite funkcijo poteza, ki sprejme podatke o samici in samcu ter vrne
-- vsakega od obeh pajkov ustrezno premakne.

poteza = undefined

-- Nazadnje sestavite funkcijo simuliraj, ki simulira obnašanje samice in samca.

simuliraj = undefined