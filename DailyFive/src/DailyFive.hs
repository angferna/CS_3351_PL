module DailyFive where
    import Data.Char(isLower)

    -- multPairs
    --  consumes a list of pairs of integers
    --  produces a list of the products of each pair.
    -- Eg: multPairs [(2,4),(1,2)] gives [8,2]
    multPairs :: [(Int,Int)] -> [Int]
    multPairs = map (\(a,b) -> a * b)

    --squareList
    --  consumes a list of Integers as input
    --  produces a new list of pairs of Integers
    --  The resulting pairs should be the original Integer and the square of that integer
    --  Eg: squareList [1,3,2] should produce [(1,1), (3, 9), (2, 4)].
    squareList :: [Int] -> [(Int,Int)]
    squareList = map (\a -> (a, (a^2)))

    --findLowercase
    --  consumes a list of String
    --  produces a list of Bool
    --  An entry in the produced list should be True if the corresponding String starts with a lower case character and False otherwise
    --  You may assume that each String has at least one character
    --  You may use the isLower function (in the Data.Char module)
    -- Eg: findLowercase [['Hello']] must produce [False]
    findLowercase :: [String] -> [Bool]
    findLowercase [] = []
    findLowercase xs= map (\(x:xs) -> isLower x) xs
