module DailyEIghtSpec where
    import Test.Hspec
    import DailyEight

    main :: IO ()
    main = hspec $ do

    describe "inYear" $ do
    it "produce inYear of 2001 l" $
         (inYear 2001 l) `shouldBe` [Event {name = "Angel's Birthday", day =2, month = "Nov", year = 2001, xlocation = 100.0, ylocation = 100.0}, Event { name = "Analise's Birthday", day =10, month = "Dec", year = 2001, xlocation = 150.0, ylocation = 150.0}]

        it "produce inYear of 1960 l" $
         (inYear 1960 l) `shouldBe` [Event {name = "Clara's Birthday", day =1, month = "Jul", year = 1960, xlocation = 500.0, ylocation = 800.0}]

        it "produce inYear of 1900 l" $
         (inYear 1900 l) `shouldBe` []
    
    describe "inDayRange" $ do
        it "produce inDayRange of 5 20 l" $
         (inDayRange 5 20 l) `shouldBe` ["Analise's Birthday"]
        
        it "produce inDayRange of 1 5 l" $
         (inDayRange 1 5 l) `shouldBe` ["Angel's Birthday", "Clara's Birthday"]

        it "produce inDayRange of 20 30 l" $
         (inDayRange 20 30 l) `shouldBe` []

    describe "inArea" $ do
        it "produce inArea of "Angel's Birthday" 50 200 50 200" $
         (inArea "Angel's Birthday" 50 200 50 200) `shouldBe` [Event {name = "Angel's Birthday", day =2, month = "Nov", year = 2001, xlocation = 100.0, ylocation = 100.0}]

        it "produce inArea of "Analise's Birthday" 50 100 50 100" $
         (inArea "Analise's Birthday" 50 100 50 100) `shouldBe` []

        it "produce inArea of "Clara's Birthday" 100 1000 100 1000" $
         (inArea "Clara's Birthday" 100 1000 100 1000) `shouldBe` [Event {name = "Clara's Birthday", day =1, month = "Jul", year = 1960, xlocation = 500.0, ylocation = 800.0}]

        it "produce inArea of "Clara's Birthday" 0 10 10 20" $
         (inArea "Clara's Birthday" 0 10 10 20) `shouldBe` []

