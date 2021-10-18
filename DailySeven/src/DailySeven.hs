module DailySeven where

    --createOneList 
    --  consumes a list of lists of some type
    --  produce a single list which contains all the elements contained in the lists.
    --  E.g. createOneList [ [1,2], [3], [ ], [4, 5] ] should produce [1,2,3,4,5].
    createOneList :: [[a]] -> [a]
    createOneList xs = foldr (++) [] xs
    
    --findLargest
    --  consumes a list of positive integer numbers
    --  produce the largest number contained in the list.
    --  E.g. findLargest [3, 5, 10, 9, 15] should produce 15, findLargest [] should return 0.
    findLargest :: [Int] -> Int
    findLargest = foldl max 0
    
    --allTrue 
    --  consumes a list of boolean values 
    --  produces True if all the elements of the list are True and False otherwise.
    allTrue :: [Bool] -> Bool
    allTrue [] = False
    allTrue (x:xs) = foldr (&&) True xs