module DailySixSpec where

    import Test.Hspec
    import DailySix

    main :: IO ()
    main = hspec $ do

        describe "shorterThan" $ do
            it "produce shorterThan of 3 ["hello"]" $
                (shorterThan 3 ["hello"]) `shouldBe` []

            it "produce shorterThan of 3 ["hello","hey","hi"]" $
                (shorterThan 3 ["hello","hey","hi"]) `shouldBe` ["hey","hi"]

            it "produce shorterThan of 3 []" $
                (shorterThan 3 []) `shouldBe` []
        
        describe "removeMultiples" $ do
            it "produce removeMultiples of  5 [3,5,10,9, 15]" $
                (removeMultiples 5 [3,5,10,9, 15]) `shouldBe` [3,9]

            it "produce removeMultiples of []" $
                (removeMultiples 5 []) `shouldBe` []

            it "produce removeMultiples of 4 [2,4,6,8]" $
                (removeMultiples 4 [2,4,6,8]) `shouldBe` [2,6]

        describe "onlyJust" $ do
            it "produce onlyJust of [Nothing, Just 5, Nothing, Just 10]" $
                (onlyJust [Nothing, Just 5, Nothing, Just 10]) `shouldBe` [Just 5,Just 10]

            it "produce onlyJust of [Nothing, Nothing]" $
                (onlyJust [Nothing, Nothing]) `shouldBe` []

            it "produce onlyJust of []" $
                (onlyJust []) `shouldBe` []