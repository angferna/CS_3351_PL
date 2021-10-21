module DailySix where

    --shorterThan
    --  consumes a number and a list of words.
    --  produce a list of the words whose length is shorter than or equal to the given number.
    shorterThan :: Int -> [String] -> [String]
    shorterThan n = filter(\x -> n >= length x)

    --removeMultiples 
    --  consumes a number and a list of numbers. 
    --  produce a list where the multiples of the given number have been removed. 
    --  E.g.: removeMultiples 5 [3,5,10,9, 15] should produce [3,9]
    removeMultiples :: Int -> [Int] -> [Int]
    removeMultiples n = filter (\x -> x `mod` n /= 0)

    --onlyJust
    --  consumes a list of Maybe a
    --  produces a list where all values of Nothing have been eliminated
    --  E.g.: onlyJust [Nothing, Just 5, Nothing, Just 10] should produce [Just 5, Just 10].
    onlyJust :: Eq a =>[Maybe a] -> [Maybe a]
    onlyJust xs= filter(\x -> x /= Nothing ) xs