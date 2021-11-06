module DailyNineSpec where

    import Test.Hspec
    import DailyNine

    main :: IO ()
    main = hspec $ do

    describe "minAndMax" $ do
    it "produce minAndMax of [1,2,3,4,5]" $
         (minAndMax [1,2,3,4,5]) `shouldBe` (1,5)

        it "produce minAndMax of [3,7,5,8,5,3,7,5,8,5]" $
         (minAndMax [3,7,5,8,5,3,7,5,8,5]) `shouldBe` (3,8)

         it "produce minAndMax of [5]" $
         (minAndMax [5]) `shouldBe` (5,5)

         it "produce minAndMax of [] " $
         (minAndMax []) `shouldBe` (0,0)
    
    describe "everyK" $ do
        it "produce everyK of 2[3,7,5,8,5,3,7,5,8,5]" $
         (everyK 2[3,7,5,8,5,3,7,5,8,5]) `shouldBe` [3,5,5,7,8,0]
        
        it "produce everyK of 4 [3,7,5,8,5]" $
         (everyK 4 [3,7,5,8,5]) `shouldBe` [3,5,0]

        it "produce everyK of 5 [ ]" $
         (everyK 5 []) `shouldBe` [0]

    describe "shuffle" $ do
        it "produce shuffle of [1,3,5] [2,4]" $
         (shuffle [1,3,5] [2,4]) `shouldBe` [1,2,3,4,5]

        it "produce shuffle of [1,3,5] []" $
         (shuffle [1,3,5] []) `shouldBe` [1,3,5]

        it "produce shuffle of [] [2,4]" $
         (shuffle [] [2,4]) `shouldBe` [2, 4]

        it "produce shuffle of [][]" $
         (shuffle [][]) `shouldBe` []
