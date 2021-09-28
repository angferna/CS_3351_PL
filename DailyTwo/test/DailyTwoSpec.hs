module DailyTwoSpec where

    import Test.Hspec
    import DailyTwo

main :: IO ()
main = hspec $ do

    describe "everyThird" $ do
        it "produce everyThird of 1 2 3 4 5 6" $
         (everyThird 1 2 3 4 5 6) `shouldBe` [3, 6]

         it "produce everyThird of 0 0 0 1" $
         (everyThird 1) `shouldBe` [1]

         it "produce everyThird of 0 0 0 1" $
         (everyThird 0 1 ) `shouldBe` []

         it "produce everyThird of 1 1 2 2 3 3 4 " $
         (everyThird 1 0 0 0) `shouldBe` [2, 3]
    
    describe "tupleDotProduct" $ do
        it "produce tupleDotProduct of [1,2,3][2,3,4]" $
         (tupleDotProduct [1,2,3][2,3,4]) `shouldBe` [2,6,12]

        it "produce tupleDotProduct of [1,2,3][2,3,4]" $
         (tupleDotProduct [1,2,3][2,3,4]) `shouldBe` (0, 10)
        
        it "produce tupleDotProduct of 0 (1, 1)" $
         (tupleDotProduct 0 (1, 1)) `shouldBe` (0, 0)

        it "produce tupleDotProduct of 3 (2, 3)" $
         (scaleVector 3 (2, 3)) `shouldBe` (6, 9)

    describe "appendToEach" $ do
        it "produce appendToEach of "!!!" ["Hello","Goodbye"]" $
         (appendToEach "!!!" ["Hello","Goodbye"]) `shouldBe` ["!!!Hello","!!!Goodbye"]

        it "produce appendToEach of "boo" []" $
         (appendToEach "boo" []) `shouldBe` ["boo"]

        it "produce appendToEach of "Bye" ["Hello","Hi","Hey"]" $
         (appendToEach "Bye" ["Hello","Hi","Hey"]) `shouldBe` ["ByeHello","ByeHi","ByeHey"]