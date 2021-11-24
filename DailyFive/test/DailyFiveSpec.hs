module DailyFiveSpec where

    import Test.Hspec
    import DailyFive

main :: IO ()
main = hspec $ do

    describe "multPairs" $ do
        it "produce multPairs of [(2,4),(1,2)]" $
         (multPairs [(2,4),(1,2)]) `shouldBe` [8,2]

         it "produce multPairs of []" $
         (multPairs []) `shouldBe` []

         it "produce multPairs of [(2,4),(1,2),(50,2)]" $
         (multPairs [(2,4),(1,2),(50,2)]) `shouldBe` [8,2,100]
        
    describe "squareList" $ do
        it "produce squareList of [1,3,2]" $
         (squareList [1,3,2]) `shouldBe` [(1,1), (3, 9), (2, 4)]

        it "produce squareList of []" $
         (squareList []) `shouldBe` []

        it "produce squareList of [8]" $
         (squareList [8]) `shouldBe` [(8,64)]

    describe "findLowercase" $ do
        it "produce findLowercase of ["hello"]" $
         (findLowercase ["hello"]) `shouldBe` [True]

        it "produce findLowercase of ["Hello"]" $
         (findLowercase ["Hello"]) `shouldBe` [False]

        it "produce findLowercase of ["Hey","hi","Hello","hola"]" $
         (findLowercase ["Hey","hi","Hello","hola"]) `shouldBe` [False,True,False,True]

        it "produce findLowercase of []" $
         (findLowercase []) `shouldBe` []
