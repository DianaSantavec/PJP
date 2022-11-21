-- 1.1
ukloniPoslednji [] = []
ukloniPoslednji [x] = []
ukloniPoslednji (x:xs) = x : ukloniPoslednji xs

-- 1.3.b
fakt' :: Int -> Int
fakt' x =  factPomocni x 1

factPomocni 0 acc = acc
factPomocni x acc = factPomocni (x-1) (acc * x)

-- 1.7
kvadriraj :: [Int] -> [Int]
kvadriraj l = [x*x | x <- l]

kvadriraj' :: [Int] -> [Int]
kvadriraj' l = map (\x -> x*x) l

-- 1.10
filter' :: (Int->Bool) -> [Int] -> [Int]
filter' f [] = []
filter' f (x:xs)
    | f x       = x : filter' f xs -- [x] ++
    | otherwise = filter' f xs

-- 2.2
sumElOfLists :: [[Int]] -> [Int]
sumElOfLists [] = []
sumElOfLists (x:xs) = suma : sumElOfLists xs
    where
        suma = foldl (+) 0 x

-- 2.5
ukloniDeljiveSa3 :: [[Int]] -> [[Int]]
ukloniDeljiveSa3 [] = []
ukloniDeljiveSa3 (x:xs) = (filter deljivSa3 x) : ukloniDeljiveSa3 xs
    where
        deljivSa3 x = mod x 3 /= 0

-- 3.1
data Element a = Empty
    | Cvor a (Element a) deriving Show

-- 4.5
data Naselje = 
    Selo {
        povrsina :: Double,
        brojStanovnika :: Int,
        tip :: String
    }
    | Varosica {
        povrsina :: Double,
        brojStanovnika :: Int
    }
    | Grad {
        povrsina :: Double,
        brojStanovnika :: Int,
        imaBazen :: Bool
    } deriving Show