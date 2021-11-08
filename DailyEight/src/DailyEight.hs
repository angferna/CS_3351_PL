module DailyEight where

        data Event = Event { name :: String,
                                day ::Int,
                                month ::String,
                                year ::Int,
                                xlocation::Float,
                                ylocation::Float} 
                                deriving (Show)
                                
{-}
        data Name = Name String deriving (Show)
        data Day = Day Int deriving (Show)
        data Month = Month String deriving (Show)
        data Year = Year Int deriving (Show)
        data Xlocation = Xlocation Float deriving (Show)
        data Ylocation = Ylocation Float deriving (Show)
        data Event = Event Name Day Month Year Xlocation Ylocation 
                deriving (Show)
       -}
        --inYear
        --  conumes number called, year, and a list of event structures 
        --  produces a new list
        --  Each element in the new list should be an event structure which occurred during year.
       
        --inYear :: Event -> [Event] -> [Event]
        --inYear getYear(Event a  d  m  y  x  yl) [] = []
        --inYear (Event a  d  m  y  x  yl) ys = filter(\y-> getYear y==x) ys
        --nYear Event{year = y} [] = []
        --inYear Event{year = y} (x:xs)= filter (\ y -> getYear y == x)
        
        inYear :: Int -> [Event] -> [Event]
        inYear y [] = []
        --inYear (Year y) xs = filter(\x-> getYear x == y) xs
        inYear y xs = filter(\x-> getYear x == y) xs

        getYear:: Event -> Int
        --getYear (Event (Name a)(Day d)(Month m)(Year y)(Xlocation x)(Ylocation yl))= y
        getYear (Event a d m y x yl)= y

        getDay:: Event -> Int
        --getDay (Event (Name a)(Day d)(Month m)(Year y)(Xlocation x)(Ylocation yl))= d
        getDay (Event a d m y x yl)= d

        getName:: Event -> String
        --getName (Event (Name a)(Day d)(Month m)(Year y)(Xlocation x)(Ylocation yl))= a
        getName (Event a d m y x yl)= a

        getXlocation:: Event -> Float
        --getXlocation (Event (Name a)(Day d)(Month m)(Year y)(Xlocation x)(Ylocation yl))= x
        getXlocation (Event a d m y x yl)= x

        getYlocation:: Event -> Float
        --getYlocation (Event (Name a)(Day d)(Month m)(Year y)(Xlocation x)(Ylocation yl))= yl
        getYlocation (Event a d m y x yl)= yl

        --inDayRange
        --  consumes two days (a start day and an end day) and a list of event structures
        --  produces a new list.
        --  Each element in the new list should be the name of all events which occurred between the two days (including start day and end day) but in any month or year.
        inDayRange :: Int -> Int -> [Event] -> [Event]
        --inDayRange (Day x)(Day y) [] = []
        inDayRange  x  y [] = []
        --inDayRange (Day x)(Day y) zs = filter (\a -> getDay a >=x && getDay a<=y) zs
        inDayRange x y zs = filter (\a -> getDay a >=x && getDay a<=y) zs
                
        --inArea
        --  consumes a name, a lower x location, an upper x location, a lower y location, an upper y location and a list of event structures
        --  produces a new list
        --  Each element in the new list should be the event structures which match the name and occurred in the spatial region specified.
        inArea :: String -> Float -> Float -> Float -> Float -> [Event] -> [Event]
        --inArea (Name a) _ _ _ _ []= []
        inArea a _ _ _ _ []= []
        --inArea (Name a) xl xh yl yh zs = filter(\z -> getName z==a && getXlocation z>= xl && getXlocation z<= xh && getYlocation z>= yl && getYlocation z<= yh) zs
        inArea a xl xh yl yh zs = filter(\z ->  getName z == a && getXlocation z>= xl && getXlocation z<= xh && getYlocation z>= yl && getYlocation z<= yh) zs
