module DailyFour where

    --zip3Lists
    --  takes three lists as arguments and creates a list of tuples. 
    --  Each tuple consists of the elements from each list consecutively. 
    --  You can assume the lists are the same size.
    --  Eg: zip3Lists [1, 2, 3] ['a', 'b', 'c'][4, 5, 6] would produce [(1, 'a', 4), (2, 'b', 5), (3, 'c', 6)]
    zip3Lists :: [x] -> [y] -> [z] -> [(x, y, z)]
    zip3Lists [][][]=[]
    --could not write individual cases as they produced errors
    zip3Lists (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3Lists xs ys zs

    --unzipTriples 
    --  takes a list of triples and produces a tuple of three lists. 
    --  Each of the resulting lists consists of the first elements of the triples, the second elements of the triples, and the third elements of the triples. 
    --  Eg: unzipTriples [(1,2,3), (4, 5, 6), (7, 8, 9)] should produce ( [1,4,7], [2, 5, 8], [3, 6, 9] )
    unzipTriples :: [(x,y,z)] -> ([x],[y],[z])
    unzipTriples [] = ([],[],[])
    unzipTriples ((x,y,z): xyzs) = (x:xs, y:ys, z:zs) where (xs,ys,zs) = unzipTriples xyzs
    
    --mergeSorted3
    --  takes 3 lists which are in sorted order and merges them so that the final list is sorted in increasing order
    --  Eg: mergeSorted3 [2, 3, 5] [1, 8] [-1, 0, 4, 10] is [-1, 0, 1, 2, 3, 4, 5, 8, 10]
    mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
    mergeSorted3 [][][]=[]
    mergeSorted3 [x][][]=[x]
    mergeSorted3 [][y][]=[y]
    mergeSorted3 [][][z]=[z]
    mergeSorted3 [x][y][] = if x<y
                            then [x,y]
                            else [y,x]
    mergeSorted3 [x][][z] = if x<z
                            then [x,z]
                            else [z,x]
    
    mergeSorted3 [][y][z] = if y<z
                            then [y,z]
                            else [z,y]

    mergeSorted3 (x:xs)[][] = (x:xs)
    mergeSorted3 [](y:ys)[] = (y:ys)
    mergeSorted3 [][](z:zs) = (z:zs)

    mergeSorted3 (x:xs)(y:ys)[] = if(x<y)
                                    then (x:(mergeSorted3 xs (y:ys) []))
                                    else (y:(mergeSorted3 (x:xs) ys []))

    mergeSorted3 (x:xs)[](z:zs) = if(x<z)
                                    then (x:(mergeSorted3 xs [] (z:zs)))
                                    else (z:(mergeSorted3 (x:xs) [] zs))

    mergeSorted3 [](y:ys)(z:zs) = if(y<z)
                                    then (y:(mergeSorted3 [] ys (z:zs)))
                                    else (z:(mergeSorted3 [] (y:ys) zs))

    mergeSorted3 (x:xs) (y:ys) (z:zs) = if(x < y && x < z)
                                        then (x : mergeSorted3 xs (y:ys) (z:zs))
                                        else if (y < x && y < z)
                                            then (y:(mergeSorted3 (x:xs) ys (z:zs)))
                                            else (z:(mergeSorted3 (x:xs) (y:ys) zs))