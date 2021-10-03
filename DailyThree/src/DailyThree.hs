module DailyThree where

-- removeAllExcept
--    Case 1 : consumes 1 argument of any type and an empty list and returns an empty list
--    Case 2 : consumes 1 argument of a type and a list of the same type and returns only that value as many times as it appears
-- Eg: removeAllExcept 'a' ['b', 'a', 'c', 'a'] will return the list of Char "aa"
-- Eg: removeAllExcept 1 [2, 3, 4, 1] should return [1]
removeAllExcept :: Eq a => a -> [a] -> [a]
removeAllExcept x [ ] = [ ]
removeAllExcept n (x:xs) = if n==x
                                then x:(removeAllExcept n xs)
                            else removeAllExcept n xs

-- countOccurrences
--    Case 1 : consumes 1 argument of any type and an empty list and returns an empty list
--    Case 2 : consumes 1 argument of a type and a list of the same type and returns number of times that valur has occured in list
-- Eg : countOccurrences 'a' ['a', 'b', 'a', 'c'] will produce 2
-- Eg :  countOccurrences 1 [2, 4, 5, 2] would produce 0
countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x [ ] = 0
countOccurrences n (x: xs) = if n==x
                                then 1+ (countOccurrences n xs)
                            else countOccurrences n xs

-- substitute
--    Case 1 : consumes 2 arguments of any single type and an empty list and returns an empty list
--    Case 2 : consumes 2 arguments of a type and a list of the same type and returns a list where all occurrences of the first argument are replaced with the second argument in the list
-- Eg : substitute 3 4 [1, 2, 3, 4] will give you [1, 2, 4, 4]
substitute :: Eq a => a -> a -> [a] -> [a]
substitute a b [] = []
substitute a b (x:xs) = if a==x 
                            then b : (substitute a b xs)
                        else x :(substitute a b xs)