module WeeklyAssignmentTwoSpec where
    
    import Test.Hspec
    import WeeklyAssignmentTwo

main::IO()
main = hspec $ do

    describe "search" $ do
        it "produce search of search 5 Empty" $
         (search 5 Empty) `shouldBe` False

         it "produce search of search 5 (NodeOne 5 Empty Empty Empty)" $
         (search 5 (NodeOne 5 Empty Empty Empty)) `shouldBe` True

         it "produce search of search 5 NodeTwo 4 7 Empty (NodeOne 5 Empty Empty Empty) Empty" $
         (search 5 (NodeTwo 4 7 Empty (NodeOne 5 Empty Empty Empty) Empty)) `shouldBe` True 

    describe "insert" $ do
        it "produce insert of insert 5 Empty" $
         (insert 5 Empty) `shouldBe` NodeOne 5 Empty Empty Empty

        it "produce insert of insert 10 (NodeOne 5 Empty Empty Empty)" $
         (insert 10 (NodeOne 5 Empty Empty Empty)) `shouldBe` NodeTwo 5 10 Empty Empty Empty

        it "produce insert of insert 5 (NodeTwo 4 7 Empty (NodeOne 5 Empty Empty Empty) Empty)" $
         (insert 5 (NodeTwo 4 7 Empty (NodeOne 5 Empty Empty Empty) Empty)) `shouldBe` NodeTwo 4 7 Empty (NodeTwo 5 5 Empty Empty Empty) Empty 

    describe "identical" $ do
        it "produce identical of identical Empty Empty" $
         (identical Empty Empty) `shouldBe` True

        it "produce identical of identical (NodeOne 5 Empty Empty Empty) (NodeTwo 4 7 Empty Empty Empty))" $
         (identical (NodeOne 5 Empty Empty Empty) (NodeTwo 4 7 Empty Empty Empty)) `shouldBe` False

        it "produce identical of identical (NodeOne 5 Empty Empty Empty) (NodeOne 5 Empty Empty Empty)" $
         (identical (NodeOne 5 Empty Empty Empty) (NodeOne 5 Empty Empty Empty)) `shouldBe` True

    describe "treeMap" $ do
        it "produce treeMap of treeMap (\\x->x+5) Empty" $
         (treeMap (\x->x+5) Empty) `shouldBe` Empty

        it "produce treeMap of treeMap (\\x->x+5) (NodeOne 5 Empty Empty Empty))" $
         (treeMap (\x->x+5) (NodeOne 5 Empty Empty Empty)) `shouldBe` NodeOne 10 Empty Empty Empty

        it "produce treeMap of treeMap (\\x->x+5) (NodeTwo 4 7 Empty (NodeOne 5 Empty Empty Empty) Empty)" $
         (treeMap (\x->x+5) (NodeTwo 4 7 Empty (NodeOne 5 Empty Empty Empty) Empty)) `shouldBe` NodeTwo 9 12 Empty (NodeOne 10 Empty Empty Empty) Empty
    
    describe "treeFoldPreOrder" $ do
        it "produce treeFoldPreOrder of treeFoldPreOrder (+) 2 Empty" $
         (treeFoldPreOrder (+) 2 Empty) `shouldBe` 2

        it "produce treeFoldPreOrder of treeFoldPreOrder (/) 2 (NodeOne 5 Empty Empty Empty))" $
         (treeFoldPreOrder (/) 2 (NodeOne 5 Empty Empty Empty)) `shouldBe` 0.4

        it "produce treeFoldPreOrder of treeFoldPreOrder (*) 2 (NodeTwo 4 7 Empty (NodeOne 5 Empty Empty Empty) Empty)" $
         (treeFoldPreOrder (*) 2 (NodeTwo 4 7 Empty (NodeOne 5 Empty Empty Empty) Empty)) `shouldBe` 40

    describe "treeFoldInOrder" $ do
        it "produce treeFoldInOrder of treeFoldInOrder (+) 2 Empty" $
         (treeFoldInOrder (+) 2 Empty) `shouldBe` 2

        it "produce treeFoldInOrder of treeFoldInOrder (/) 2 (NodeOne 5 Empty Empty Empty))" $
         (treeFoldInOrder (/) 2 (NodeOne 5 Empty Empty Empty)) `shouldBe` 0.4

        it "produce treeFoldInOrder of treeFoldInOrder (*) 2 (NodeTwo 4 7 Empty (NodeOne 5 Empty Empty Empty) Empty)" $
         (treeFoldInOrder (*) 2 (NodeTwo 4 7 Empty (NodeOne 5 Empty Empty Empty) Empty)) `shouldBe` 40

    describe "treeFoldPostOrder" $ do
        it "produce treeFoldPostOrder of treeFoldPostOrder (+) 2 Empty" $
         (treeFoldPostOrder (+) 2 Empty) `shouldBe` 2

        it "produce treeFoldPreOrder of treeFoldPostOrder (/) 2 (NodeOne 5 Empty Empty Empty))" $
         (treeFoldPostOrder (/) 2 (NodeOne 5 Empty Empty Empty)) `shouldBe` 0.4

        it "produce treeFoldPostOrder of treeFoldPostOrder (*) 2 (NodeTwo 4 7 Empty (NodeOne 5 Empty Empty Empty) Empty)" $
         (treeFoldPostOrder (*) 2 (NodeTwo 4 7 Empty (NodeOne 5 Empty Empty Empty) Empty)) `shouldBe` 40
