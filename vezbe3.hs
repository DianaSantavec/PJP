-- 1
data Element a = Empty
    | Cvor a (Element a) deriving Show

-- 2
kreirajMojuListu :: [Int] -> Element Int
kreirajMojuListu [] = Empty
kreirajMojuListu (x:xs) = Cvor x (kreirajMojuListu xs)

-- 3
duzinaListe :: Element a -> Int
duzinaListe Empty = 0
duzinaListe (Cvor _ sledeci) = 1 + duzinaListe sledeci

-- 4
uListi :: Eq a => a -> Element a -> Bool
uListi _ Empty = False
uListi x (Cvor y sledeci) = x == y || uListi x sledeci

-- 5
data Planeta = Nista | Planeta
    {
        ime :: String,
        precnik :: Double,
        gasovita :: Bool
    } deriving Show

-- 6
type Planete = [Planeta]

-- 7
nadjiPoImenu :: [Char] -> Planete -> Planeta
nadjiPoImenu _ [] = Nista
nadjiPoImenu imePlanete (x:xs)
    | imePlanete == ime x    = x
    | otherwise              = nadjiPoImenu imePlanete xs

-- 8
vratiGaovite :: Planete -> Planete
vratiGaovite [] = []
vratiGaovite (x:xs)
    | gasovita x    =x:vratiGaovite xs
    | otherwise     =vratiGaovite xs