module DailyFourSpec where

    import Test.Hspec
    import DailyFour

    main :: IO ()
    main = hspec $ do

    describe "zip3Lists" $ do
        it "produce zip3Lists of [1, 2, 3] ['a', 'b', 'c'] [4, 5, 6]" $
         (zip3Lists [1, 2, 3] ['a', 'b', 'c'] [4, 5, 6]) `shouldBe` [(1, 'a', 4), (2, 'b', 5), (3, 'c', 6)]

         it "produce zip3Lists of [][][]" $
         (zip3Lists [][][]) `shouldBe` []

         it "produce zip3Lists of [1, 'a', 3] ['a', 2, 'c'] [4, 'c', 6]" $
         (zip3Lists [1, 'a', 3] ['a', 2, 'c'] [4, 'c', 6]) `shouldBe` [(1, 'a', 4), ('a', 2, 'c'), (3, 'c', 6)]
    
    describe "unzipTriples" $ do
        it "produce unzipTriples of [(1,2,3), (4, 5, 6), (7, 8, 9)]" $
         (unzipTriples [(1,2,3), (4, 5, 6), (7, 8, 9)]) `shouldBe` ( [1,4,7], [2, 5, 8], [3, 6, 9] )

        it "produce unzipTriples of []" $
         (unzipTriples 1 []) `shouldBe` ([],[],[])

    describe "mergeSorted3" $ do
        it "produce mergeSorted3 of [2, 3, 5] [1, 8] [-1, 0, 4, 10]" $
         (mergeSorted3 [2, 3, 5] [1, 8] [-1, 0, 4, 10]) `shouldBe` [-1, 0, 1, 2, 3, 4, 5, 8, 10]

        it "produce mergeSorted3 of [2, 3, 5] [1, 8] []" $
         (mergeSorted3 [2, 3, 5] [1, 8] []) `shouldBe` [2,3,5,1,8]

        it "produce mergeSorted3 of [] [1, 8] []" $
         (mergeSorted3 [] [1, 8] []) `shouldBe` [1, 8]