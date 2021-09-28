module DailyTwo where

-- everyThird formula 
--    case 1 : empty list returns empty list
--    case 2 : list with one one value returns same value
--    case 3 : list with just 2 values returns empty list
--    case 4 : list with just 3 values returns 3rd value
--    case 4 : consumes a list of Integers making sure there is 3 integers and returns 3rd value recursively
everyThird :: [Integer] -> [Integer]
everyThird [] = [ ]
everyThird [x] = [x]
everyThird [x1, x2] = [ ]
everyThird [x1, x2, x3] = [x3]
everyThird (x1:x2:x3:xs) = x3 : everyThird xs

-- tupleDotProduct
--    case 1 : 2 empty lists return 0
--    case 2 and 3 : 1 empty lists and 1 list retun 0
--    case 4 : consumes 2 list of Integers and returns the tuple dot product of those two lists
tupleDotProduct :: [Integer] -> [Integer] -> Integer
tupleDotProduct [][] = 0
tupleDotProduct (x:xs) [] = 0
tupleDotProduct [] (x:xs) = 0
tupleDotProduct (q:qs)(p:ps) = (q*p) + (tupleDotProduct qs ps)

-- appendToEach
--   case 1 : empty string and empty list of strings return empty list
--   case 2 : single string and empty list return list containg only one value i.e single string
--   case 3 : emptry string and list of stringd return list of strings itself
--   case 4 : consumes 1 string and a list of strings and returns appended version of single string with every valur in list of string
appendToEach :: String -> [String] -> [String]
appendToEach " " [ ] = [ ]
appendToEach n [ ] = [n]
appendToEach " " (x:xs) = (x:xs)
appendToEach n (x:xs) = (n ++ x) : (appendToEach n xs)
-- :  :: elem -> [elem] -> [elem]
-- ++ :: [a] -> [a] -> [a]
-- 1:[2,3] = [1]++[2,3]