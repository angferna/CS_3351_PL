module DailyNine where

    -- minAndMax
    --  finds the minimum and maximum of a list of numbers in 3⌊𝑛/2⌋ comparisons. 
    --  you must process the list of numbers in pairs first with each other and then with the minimum and maximum for a total of 3 comparisons for every two numbers. 
    --  Hint: Assume the list has an even number of elements first, then handle an odd number of elements. 
    --  produce a tuple containing the minimum and maximum numbers. 
    --  Note: First finding the max and then finding the min, using min and max will result in 2n comparisons, not the lower bounds stated above.
    minAndMax :: [Int] -> (Int,Int)
    minAndMax [] = (0,0)
    minAndMax [x] = (x,x)
    minAndMax (x:xs) = (min x mins, max x maxs)
        where (mins, maxs) = minAndMax xs

    -- everyK
    --  consumes a number and a list
    --  produce every kth element of the list
    --  k is a positive number
    everyK:: Int -> [Int] -> [Int]
    everyK _ [] = [0]
    everyK k (x:xs) = x : (everyK k . drop (k-1)) xs

    --shuffle
    --  consumes two lists of the same type elements
    --  produces one list which contains alternating elements from the given lists. 
    --  E.g. shuffle [1,3,5] [2,4] should produce [1,2,3,4,5]. 
    --  The given lists can be any length.
    shuffle:: [a] -> [a] -> [a]
    shuffle [] [] = []
    shuffle [] (x:xs) = (x:xs)
    shuffle (x:xs) [] = (x:xs)
    shuffle (x:xs) (y:ys) = x: y : shuffle xs ys