-- 1
data Stablo = Nista
    | Cvor Int Stablo Stablo
    deriving Show

-- 2
sadrzi :: Int -> Stablo -> Bool
sadrzi _ Nista = False
sadrzi n (Cvor br levo desno)
    | n == br   =True
    |otherwise  =(sadrzi n levo) (sadrzi n desno)

-- 3
uListu :: Stablo -> [Int]
uListu Nista = []
uListu (Cvor br levo desno) = (uListu levo) ++ [br] ++ (uListu desno)

-- 4
deljivi :: Stablo -> [Int]
deljivi Nista = []
deljivi (Cvor br levo desno)
    | deljivoSa5Ili3     = (deljivi levo) ++ [br] ++ (deljivi desno)
    | otherwise             = (deljivi levo) ++ (deljivi desno)
    where
        deljivoSa5Ili3 = mod br 3 == 0 || mod br 5 == 0

-- 5
preslikaj :: Stablo -> Stablo
preslikaj Nista = Nista
preslikaj (Cvor br levo desno) = Cvor br (preslikaj desno) (preslikaj levo)

-- 6
filterStablo :: (Int -> Bool) -> Stablo -> [Int]
filterStablo _ Nista = []
filterStablo f (Cvor br levo desno)
    | f br      = (filterStablo levo) ++ [br] ++ (filterStablo desno)
    | otherwise = (filterStablo levo) ++ (filterStablo desno)