-- 1
rastaviString :: Char -> String -> [String]
rastaviString delimiter string = rastaviPom string delimiter []
    where
        rastaviPom :: String -> Char -> String ->[String]
        rastaviPom [] _ acc = [acc]
        rastaviPom (x:xs) delimiter acc
            | x == delimiter    =[acc] ++ rastaviPom xs delimiter []
            |otherwise          =rastaviPom xs delimiter (acc ++ [x])

-- 2
-- spojiStringove :: [String] -> String
-- spojiStringove [] = ""
-- spojiStringove (x:xs) = x ++ "." ++ spojiStringove xs

spojiStringove :: [String] -> String
spojiStringove l = foldr spoji "" l
    where
        spoji [] b = b
        spoji a [] = a
        spoji a b = a ++ "," ++ b

-- 3
radvojiStringove :: [String] -> String
radvojiStringove [] = ""
radvojiStringove s = spojiStringove rastavljeni
    where
        rastavljeni = foldl (++) [] (map (rastaviString ' ') s)

-- 4
svastaSaListom :: Num a => [[a]] -> a
svastaSaListom l = foldl (*) 1 sume
    where
        kvadrirani = map (\x -> map (\y -> y^2) x) l
        sume = map sum kvadrirani

-- 5
data Naselje =
    Selo {
        povrsina :: Double,
        brojStanovnika :: Int,
        tip :: String
    }
    | Varosica{
        povrsina :: Double,
        brojStanovnika :: Int
    }
    | Grad {
        povrsina :: Double,
        brojStanovnika :: Int,
        imaBazen :: Bool
    }deriving Show

-- 6
type Naselja = [Naselje]
izdvojNaselja :: Naselja -> Naselja
izdvojNaselja [] = []
izdvojNaselja (x:xs)
    | (jeSelo x) && (tip x) == "razbijeno"                          = x: izdvojNaselja xs
    | (jeGrad x) && (imaBazen x) && (brojStanovnika x) >= 150000    = x :izdvojNaselja xs
    | otherwise                                                     = izdvojNaselja xs
    where
        jeSelo (Selo {}) = True
        jeSelo _ = False
        jeGrad (Grad {}) = True
        jeGrad _ = False