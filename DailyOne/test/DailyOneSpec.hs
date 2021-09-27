module DailyOneSpec where

    import Test.Hspec
    import DailyOne

main :: IO ()
main = hspec $ do

    describe "quadratic" $ do
        it "produce quadratic of 0 0 0 1" $
         (quadratic 0 0 0 1) `shouldBe` 0

         it "produce quadratic of 0 0 0 1" $
         (quadratic 0 0 1 0) `shouldBe` 0

         it "produce quadratic of 0 0 0 1" $
         (quadratic 0 1 0 0) `shouldBe` 0

         it "produce quadratic of 0 0 0 1" $
         (quadratic 1 0 0 0) `shouldBe` 1
    
    describe "scaleVector" $ do
        it "produce scaleVector of 5 (1, 0)" $
         (scaleVector 5 (1, 0)) `shouldBe` (5, 0)

        it "produce scaleVector of 10 (0, 1)" $
         (scaleVector 10 (0, 1)) `shouldBe` (0, 10)
        
        it "produce scaleVector of 0 (1, 1)" $
         (scaleVector 0 (1, 1)) `shouldBe` (0, 0)

        it "produce scaleVector of 3 (2, 3)" $
         (scaleVector 3 (2, 3)) `shouldBe` (6, 9)

    describe "tripleDistance" $ do
        it "produce tripleDistance of (0,0,1)(0,0,0)" $
         (scaleVector (0,0,1)(0,0,0)) `shouldBe` 1.0

        it "produce tripleDistance of (0,0,1)(0,0,-1)" $
         (scaleVector (0,0,1)(0,0,-1)) `shouldBe` 2.0

        it "produce tripleDistance of (0,0,1)(0,0,0)" $
         (scaleVector (0,0,1)(0,1,0)) `shouldBe` (sqrt((0-0)^2)+(0-1)^2+(1-0)^2))