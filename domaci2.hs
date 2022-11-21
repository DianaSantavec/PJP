import Data.Char

-- 1
transformList :: [Int] -> [Int]
transformList [] = []
transformList l 
    | mod (length l) 2 == 0     =map (\x -> x * x) l
    | otherwise                 =map (\x -> x * 10) l

-- 2
transformString :: [Char] -> [Char]
transformString l = map toUpper samoMala
    where
            samoMala = filter (\x -> x >= 'a' && x <= 'z' ) l

-- 3
primeni :: ([[Int]] -> [[Int]]) -> ([[Int]]->[Int]) -> [[Int]] -> [Int]
primeni _ _ [] = []
primeni f1 f2 l = f2 $ f1 l

-- 4
-- Zadatak 2 sa vezbi
sumeListi :: [[Int]] -> [Int]
sumeListi [] = []
sumeListi (x:xs) = suma : sumeListi xs
    where
        suma = foldl (+) 0 x

-- Zadatak 3 sa vezbi
izbaciParne :: [[Int]] -> [[Int]]
izbaciParne [] = []
izbaciParne (x:xs)  
	| bezParnih == [] = izbaciParne xs
	| otherwise       = bezParnih : izbaciParne xs
	where 
		bezParnih = filter (\x -> mod x 2 /= 0) x

izbaciParnePaSumiraj = primeni izbaciParne sumeListi

-- 5
prosecnaDuzina :: [[Char]] -> Int
prosecnaDuzina l = div sumaDuzina (length l)
    where
        duzine = map length l
        sumaDuzina = foldl (+) 0 duzine