module WeeklyAssignmentOne where

-- removeChar 
--     consumes a single character and a string
--     Produces a new string with all instances of the character removed
removeChar :: Char -> String -> String
removeChar n "" = ""
removeChar n (x:xs) = if n==x
                        then removeChar n xs
                    else x:removeChar n xs

-- removeWhitespace 
--     consumes a string. 
--     Produces a new string with all spaces, tabs, new line characters, and carriage returns removed
--     composes instances of removeChar
removeWhitespace :: String -> String
removeWhitespace = removeChar ' '.removeChar '\n'.removeChar '\t'.removeChar '\CR'

-- removePunctuation 
--     consumes a string. 
--     Produces a new string with all commas, periods, parentheses, square brackets, and curly brackets removed
--     composes instances of removeChar
removePunctuation :: String -> String
removePunctuation = removeChar ','.removeChar '.'.removeChar '('.removeChar ')'. removeChar '['.removeChar ']'.removeChar '{'.removeChar '}'

-- charsToAscii 
--     consumes a string 
--     Produces a new list containing the ASCII values of the characters in the given string. 
--     uses fromEnum function
charsToAscii :: String -> [Int]
charsToAscii "" = []
charsToAscii (x:xs) = fromEnum x: charsToAscii xs

-- asciiToChars 
--     consumes a list of integers (which are assumed to be valid ASCII values). 
--     Produces a new list of characters created from the ASCII values. 
--     uses toEnum function
asciiToChars :: [Int] ->[Char]
asciiToChars [] = []
asciiToChars (x:xs) = toEnum x : asciiToChars xs

-- shiftInts 
--     consumes an integer (the shift value) and a list of integers (which are assumed to be valid ASCII values). 
--     Produces a new list of integers where each value in the given list has been increased by the shift value (modulo 128 which is the maximum ASCII value). 
--     Eg : shiftInts 1 [2, 4, 6] should produce [3, 5, 7]
shiftInts :: Integer -> [Integer] -> [Integer]
shiftInts n [ ] = [ ]
shiftInts 0 (x:xs) = x:xs
shiftInts n (x:xs) = if x + n > 128
                    then mod (x+n) 128 : shiftInts n xs
                    else x+n: shiftInts n xs

-- shiftMessage 
--     consumes an integer (the shift value) and a string (the message). 
--     Produces a new string which is the encrypted message where each character has been shifted by the shift value in the ASCII encoding. 
--     Note that this can also be used to decrypt a message using a negative integer as the shift value
shiftMessage :: Int -> String -> String
shiftMessage 0 (x:xs) = x:xs
shiftMessage n [] =[]
shiftMessage n (x:xs) = if (fromEnum x + n) > 128
                        then toEnum(mod(fromEnum x+ n) 128) : shiftMessage n xs
                        else toEnum (fromEnum x+ n) : shiftMessage n xs

--Angel :)