module DailySevenSpec where

    import Test.Hspec
    import DailySeven

main :: IO ()
main = hspec $ do

    describe "createOneList" $ do
        it "produce createOneList of [(2,4),(1,2)]" $
         (createOneList [ [1,2], [3], [ ], [4, 5] ]) `shouldBe` [1,2,3,4,5]

         it "produce createOneList of []" $
         (createOneList []) `shouldBe` []

         it "produce createOneList of [[8]]" $
         (createOneList [[8]]) `shouldBe` [8]
        
    describe "findLargest" $ do
        it "produce findLargest of [3, 5, 10, 9, 15]" $
         (findLargest [3, 5, 10, 9, 15]) `shouldBe` 15

        it "produce findLargest of []" $
         (findLargest []) `shouldBe` 0

        it "produce findLargest of [8]" $
         (findLargest [8]) `shouldBe` 8

    describe "allTrue" $ do
        it "produce allTrue of [True, True]" $
         (allTrue [True, True]) `shouldBe` True

        it "produce allTrue of []" $
         (allTrue []) `shouldBe` False

        it "produce allTrue of [True,True, False]" $
         (allTrue [True,True, False]) `shouldBe` False
