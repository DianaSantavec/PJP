removeLast [] = []
removeLast [x] = []
removeLast (x:xs) = x : removeLast xs

removeSecondToLast [] = []
removeSecondToLast [x] = []
removeSecondToLast (x:y:[]) = [y]
removeSecondToLast (x:xs) = x : removeSecondToLast xs

factRec :: Int -> Int
factRec 0 = 1
factRec 1 = 1
factRec x = x * factRec (x-1)

factTail n = factAcc n 1
factAcc n acc
    | n == 0    =acc
    | otherwise = factAcc (n-1) (acc*n)

imaVelikaSlova :: [Char] -> Bool
imaVelikaSlova [] = False
imaVelikaSlova (x:xs)
    | x >= 'A' && x <= 'Z'  =True
    | otherwise             =imaVelikaSlova(xs)

spljosti [] = []
spljosti [x] = [x]
spljosti (x:y:xs)
    | x == y    =spljosti(x:xs)
    | otherwise = x : spljosti(y:xs)

svaMala :: [[Char]] -> [[Char]]
svaMala [] = []
svaMala (x:xs)
    | imaVelikaSlova x      =[x] ++ svaMala xs
    | otherwise             = svaMala xs

kvadrirajListu :: [Int] -> [Int]
kvadrirajListu l = [x*x | x <- l]

jeDeljiv :: Int -> Int -> Bool
-- test
-- jeDeljiv x y
--     | mod x y == 0  =True
--     | otherwise     =False

jeDeljiv _ 0 = False
jeDeljiv x y = mod y x == 0

jeDeljivSa3 x = jeDeljiv 3 x

filter' :: (Int->Bool) -> [Int] -> [Int]
filter' f [] = []
filter' f (x:xs)
    | f x       =[x] ++ filter' f xs
    | otherwise =filter' f xs

deljiviSa3 :: [Int] -> [Int]
deljiviSa3 l = filter' jeDeljivSa3 l