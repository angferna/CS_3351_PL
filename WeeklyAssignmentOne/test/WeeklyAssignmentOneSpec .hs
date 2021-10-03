module WeeklyAssignmentOneSpec where

    import Test.Hspec
    import WeeklyAssignmentOne

main :: IO ()
main = hspec $ do

    describe "removeChar" $ do
        it "produce removeChar of 'a' "angel"" $
         (removeChar 'a' "angel") `shouldBe` "ngel"

         it "produce removeChar of 'a' """ $
         (removeChar 'a' "") `shouldBe` ""

         it "produce removeChar of 'a' "anaconda"" $
         (removeChar 'a' "anaconda") `shouldBe` "ncond"
    
    describe "removeWhitespace" $ do
        it "produce removeWhitespace of "hello "" $
         (removeWhitespace "hello ") `shouldBe` "hello"

        it "produce removeWhitespace of "hello  hello"" $
         (removeWhitespace "hello    hello") `shouldBe` "hellohello"
        
        it "produce removeWhitespace of "hello hello    hello"" $
         (removeWhitespace "hello hello    hello") `shouldBe` "hellohellohello"

    describe "removePunctuation" $ do
        it "produce removePunctuation of "Hello, Goodbye"" $
         (removePunctuation "Hello, Goodbye") `shouldBe` "Hello Goodbye"

        it "produce removePunctuation of "Hello, Goodbye"" $
         (removePunctuation "Hello,Goodbye.Hello" []) `shouldBe` ["HelloGoodbyeHello"]

        it "produce appendToEach of "Hello, Goodbye. Hello() Goodbye{} Hello[] "" $
         (appendToEach "Bye" "Hello, Goodbye. Hello() Goodbye{} Hello[] ") `shouldBe` "Hello Goodbye Hello Goodbye Hello "
    
    describe "charsToAscii" $ do
        it "produce charsToAscii of " "" $
         (charsToAscii " ") `shouldBe` []

         it "produce charsToAscii of "hello"" $
         (charsToAscii "hello") `shouldBe` [104,101,108,108,111]

         it "produce charsToAscii of "hello goodbye"" $
         (charsToAscii "hello goodbye") `shouldBe` [104,101,108,108,111,32,103,111,111,100,98,121,101]
    
    describe "asciiToChars" $ do
        it "produce asciiToChars of [104,101,108,108,111]" $
         (asciiToChars [104,101,108,108,111]) `shouldBe` "hello"

        it "produce asciiToChars of []" $
         (asciiToChars []) `shouldBe` []
        
        it "produce asciiToChars of [98,111,111]" $
         (asciiToChars [98,111,111]) `shouldBe` "boo"

    describe "shiftInts" $ do
        it "produce shiftInts of 1 [2, 4, 6]" $
         (shiftInts 1 [2, 4, 6]) `shouldBe` [3, 5, 7]

        it "produce shiftInts of 8 [2, 4, 6]" $
         (shiftInts 8 [2, 4, 6]) `shouldBe` [10,12,14]

        it "produce shiftInts of 0 [2, 4, 6]" $
         (shiftInts 0 [2, 4, 6]) `shouldBe` [2,4,6]
    
    describe "shiftMessage" $ do
        it "produce shiftMessage of 1 "hello"" $
         (shiftMessage 1 "hello") `shouldBe` "ifmmp"

        it "produce shiftMessage of 0 "hello"" $
         (shiftMessage 0 "hello") `shouldBe` "hello"
        
        it "produce shiftMessage of 1 """ $
         (shiftMessage 1 "") `shouldBe` ""