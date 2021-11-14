module WeeklyAssignmentTwo where

      data TriTree a =  Empty |
                        NodeOne a (TriTree a) (TriTree a) (TriTree a) | --left, right, never use
                        NodeTwo a a (TriTree a) (TriTree a) (TriTree a)
                        deriving (Show, Eq)

      --search 
      --  consumes a value and a TriTree. 
      --  The function should produce True if the value is stored in the TriTree and False otherwise. 
      --  The type of value should belong to the Ord typeclass.
      search :: Ord a => a -> TriTree a -> Bool
      search val Empty = False
      search val (NodeOne a left middle right) =
            if a == val
                  then True
                  --(search val middle)
            else if a <= val
                  then (search val left)
            else if a >= val
                  then (search val right)
            else False
      search val (NodeTwo val1 val2 left middle right) = 
            if val == val1 || val ==val2
                  then True
            else if val<=val1
                  then (search val left)
            else if val>=val2
                  then (search val right)
            else if val >= val1 && val <=val2
                  then (search val middle)
            else False
       
            
      --insert
      --  consumes a value and a TriTree
      --  produce a new TriTree which contains the given value
      --  value should be inserted into the TriTree in the correct location (based on the ordering of the type of the value)
      --  The TriTree does not have to be balanced after insertion however.
      insert :: (Ord a) => a -> TriTree a -> TriTree a
      insert val Empty = NodeOne val Empty Empty Empty
      insert val (NodeOne v left middle right) = 
            if val < v
                  then (NodeTwo val v Empty Empty Empty)
                  else  (NodeTwo v val Empty Empty Empty)
            {-else if val == v
                  then (NodeOne v left (insert val middle) right)
            else if val > v
                  then (NodeOne v left middle (insert val right))
            else 
                  insert val middle-}
      insert val (NodeTwo val1 val2 left middle right) = 
            if val < val1
                  then (NodeTwo val1 val2 (insert val left) middle right)
            else if val > val2
                  then (NodeTwo val1 val2 left middle (insert val right))
            else 
                   (NodeTwo val1 val2 left (insert val middle) right)

      --insertList 
      --  consumes a list of values and a TriTree
      --  produce a new TriTree which contains all of the values from the given list.
      insertList :: (Ord a) => [a] -> TriTree a -> TriTree a
      --insertList [] hello = hello
      insertList [] Empty = Empty
      insertList (x:xs) Empty = insertList xs (insert x (NodeOne x Empty Empty Empty))
      --insertList (x:xs) hello = insertList xs (insert x hello)
      --insertList (x:xs) Empty = Empty
      insertList [] (NodeOne v left middle right) = (NodeOne v left middle right)
      insertList [] (NodeTwo val1 val2 left middle right)  =(NodeTwo val1 val2 left middle right)
      insertList (x:xs) (NodeOne v left middle right) = insertList xs (insert x (NodeOne v left middle right))
      insertList (x:xs) (NodeTwo val1 val2 left middle right) = insertList xs (insert x (NodeTwo val1 val2 left middle right))

      --identical 
      --  consumes two TriTrees
      --  produce True if the two TriTrees are exactly identical and False otherwise.
      identical :: Ord a => TriTree a -> TriTree a -> Bool 
      identical Empty Empty = True
      identical (NodeOne a la ma ra) Empty = False
      identical (NodeTwo a1 a2 la ma ra) Empty = False
      identical Empty (NodeOne a la ma ra) = False
      identical Empty (NodeTwo a1 a2 la ma ra) = False
      
      identical (NodeOne a la ma ra) (NodeOne b lb mb rb) = 
            if (a == b) then
                  if (la == lb) && (ma == mb) && (ra == rb)
                        then True
                  else False 
            else False
      identical (NodeTwo a1 a2 la ma ra) (NodeTwo b1 b2 lb mb rb) = 
            if (a1==b1) then
                  if (a2==b2) && (la==lb) && (ma==mb) && (ra==rb)
                        then True
                  else False 
            else False 
      identical (NodeOne a la ma ra) (NodeTwo b1 b2 lb mb rb) = False

      --treeMap 
      --  consumes a function, f :: a -> b, and a TriTree. 
      --  A new tree should be produced which results from apply f to each value of stored in the TriTree.
      treeMap :: Ord a => (a -> b) -> TriTree a -> TriTree b
      treeMap f Empty = Empty
      treeMap f (NodeOne a left middle right) = NodeOne (f a) (treeMap f left) (treeMap f middle) (treeMap f right)
      treeMap f (NodeTwo a b left middle right) = NodeTwo (f a) (f b) (treeMap f left) (treeMap f middle) (treeMap f right)

      --treeFoldPreOrder 
      --  consume a function, f :: a -> a -> a, an initial value, and a TriTree.
      --  produce the result of using f to combine values in the TriTree. 
      --  The left-most value in the node should be processed first, then the right-most value in the node, and finally the left, middle, and right subtrees should be processed.
      treeFoldPreOrder :: Ord a => (a -> a -> a) -> a -> TriTree a -> a
      treeFoldPreOrder f val Empty = val
      treeFoldPreOrder f val (NodeOne a left middle right) = treeFoldPreOrder f(treeFoldPreOrder f(treeFoldPreOrder f (f val a) left) middle) right
      treeFoldPreOrder f val (NodeTwo a b left middle right) = treeFoldPreOrder f(treeFoldPreOrder f(treeFoldPreOrder f (f val a) left) middle) right
     
      --treeFoldInOrder 
      --  consume a function, f :: a -> a -> a, an initial value, and a TriTree. 
      --  produce the result of using f to combine values in the TriTree.             
      --  The values in the left subtree should be processed first, then the left-most value, followed by the middle subtree, and then the right-most value, and then the right subtree.
      treeFoldInOrder :: Ord a => (a -> a -> a) -> a -> TriTree a -> a
      treeFoldInOrder f val Empty = val
      treeFoldInOrder f val (NodeOne a left middle right) = treeFoldInOrder f(treeFoldInOrder f(treeFoldInOrder f(treeFoldInOrder f(treeFoldInOrder f (f val a) left) left) middle) right) right
      treeFoldInOrder f val (NodeTwo a b left middle right) = treeFoldInOrder f(treeFoldInOrder f(treeFoldInOrder f(treeFoldInOrder f(treeFoldInOrder f  (f val a) left) left) middle) right) right

      --treeFoldPostOrder
      --  consume a function, f :: a -> a -> a, an initial value, and a TriTree. 
      --  produces the result of using f to combine values in the TriTree. 
      --  The values in the subtrees should be processed first, followed by the left and right values stored in the node.
      treeFoldPostOrder :: Ord a => (a -> a -> a) -> a -> TriTree a -> a
      treeFoldPostOrder f val Empty = val
      treeFoldPostOrder f val (NodeOne a left middle right) = treeFoldInOrder f(treeFoldInOrder f(treeFoldInOrder f (f val a) left) right) middle
      treeFoldPostOrder f val (NodeTwo a b left middle right) = treeFoldInOrder f(treeFoldInOrder f(treeFoldInOrder f (f val a) left) right) middle
