module DailyThreeSpec where

    import Test.Hspec
    import DailyThree

main :: IO ()
main = hspec $ do

    describe "removeAllExcept" $ do
        it "produce removeAllExcept of 'a' ['b', 'a', 'c', 'a']" $
         (removeAllExcept 'a' ['b', 'a', 'c', 'a']) `shouldBe` "aa"

         it "produce removeAllExcept of 1 [2, 3, 4, 1]" $
         (removeAllExcept 1 [2, 3, 4, 1]) `shouldBe` [1]

         it "produce removeAllExcept of 'a' ['b', 'a', 'c', 'f']" $
         (removeAllExcept 'a' ['b', 'a', 'c', 'f'] ) `shouldBe` "a"

         it "produce removeAllExcept of 1 [1, 3, 4, 1] " $
         (removeAllExcept 1 [1, 3, 4, 1]) `shouldBe` [1, 1]
    
    describe "countOccurrences" $ do
        it "produce countOccurrences of 'a' ['a', 'b', 'a', 'c']" $
         (countOccurrences 'a' ['a', 'b', 'a', 'c']) `shouldBe` 2

        it "produce countOccurrences of 1 [2, 4, 5, 2]" $
         (countOccurrences 1 [2, 4, 5, 2]) `shouldBe` 0
        
        it "produce countOccurrences of 5 [ ]" $
         (countOccurrences 5 [ ]) `shouldBe` 0

    describe "substitute" $ do
        it "produce substitute of 3 4 [1, 2, 3, 4]" $
         (substitute 3 4 [1, 2, 3, 4]) `shouldBe` [1, 2, 4, 4]

        it "produce substitute of 3 4 []" $
         (substitute 3 4 []) `shouldBe` [ ]

        it "produce substitute of 5 0 [1, 2, 3, 4]" $
         (substitute 5 0 [1, 2, 3, 4]) `shouldBe` [1, 2, 3, 4]
