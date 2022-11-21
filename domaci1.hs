removeNth n l = removeNthHelp n l 0

removeNthHelp _ [] _ = []
removeNthHelp n (x:xs) count
    | n == count + 1    =removeNthHelp n xs 0
    | otherwise         =[x] ++ removeNthHelp n xs (count + 1)

delioci :: Int -> [Int]
delioci 0 = []
delioci n = [x | x <- [1..n], mod n x == 0 ]

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = (quicksort (manji xs)) ++ [x] ++ (quicksort (veci xs))
    where
        veci xs = filter (> x) xs
        manji xs = filter (<= x) xs
    
filter'' :: (Int -> Bool) -> [Int] -> [Int]
filter'' f l = [x | x <- l, f x]

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

zip' :: [Int] -> [Int] -> [Int]
zip' [] [] = []
zip' l1 [] = l1
zip' [] l2 = l2
zip' (x:xs) (y:ys) = [x + y] ++ zip' xs ys

sumDigit :: Int -> Int
sumDigit x
    | x < 10    =x
    | otherwise =mod x 10 + sumDigit (div x 10)

sumDigit' :: Int -> Int
sumDigit' x = sumDigitAcc x 0
sumDigitAcc 0 acc = acc
sumDigitAcc x acc = sumDigitAcc(div x 10)(acc+mod x 10)

sumEven :: Int -> Int
sumEven 0 = 0
sumEven n
    | mod digit 2 == 0      = digit + sumEven next
    | otherwise             = sumEven next
    where
            digit = mod n 10
            next = div n 10