-- 1
ispeglaj :: [[Int]] -> [Int]
ispeglaj [] = []
ispeglaj (x:xs) = x ++ ispeglaj xs

-- 2
sumLists :: [[Int]] -> [Int]
sumLists [] = []
sumLists (x:xs) = suma : sumLists xs
    where
        suma = foldl (+) 0 x

-- 3
removeEven :: [[Int]] -> [[Int]]
removeEven [] = []
removeEven (x:xs)
    | bezParnih == []   =removeEven xs
    | otherwise         =bezParnih : removeEven xs
    where
        bezParnih = filter(\x -> mod x 2 /= 0) x

-- 4
reverseListOfStrings :: [[Char]] -> [[Char]]
reverseListOfStrings l = map reverse l

-- 5
removeDevisible :: [[Int]] -> [[Int]]
removeDevisible [] = []
removeDevisible (x:xs) = (filter nijeDeljivSa3 x) : removeDevisible xs
    where
        nijeDeljivSa3 t = mod t 3 /= 0
    
-- 6
manjeOd5El :: [[Int]] -> [[Int]]
manjeOd5El [] = []
manjeOd5El l = filter duziOd4 (removeDevisible l)
    where
        duziOd4 l = length l >=5