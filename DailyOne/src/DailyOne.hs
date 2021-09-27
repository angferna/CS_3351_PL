module DailyOne where

-- quadratic formula 
--    consumes a,b,c,x  are 4 integers in a tuple
quadratic :: (Integer, Integer, Integer, Integer) -> Integer
quadratic (a,b,c,x) = a+(b*x)+(c*(x^2))

-- scaleVector
--    consumes a,b,c that are 3 integers
--    (b,c) is a 2-tuple integer
--    must result in (a*b, a*c)
scaleVector :: Integer -> (Integer,Integer) -> (Integer,Integer)
scaleVector a (x,y) = (a*x,a*y)

-- tripleDistance
--   2 3-tuples 
--   consumes 6 integers that are 2 sets of 3-dimensional coordinates
tripleDistance :: (Double,Double,Double)-> (Double,Double,Double) -> Double
tripleDistance (x1,y1,z1) (x2,y2,z2) = sqrt (((x2-x1)^2)+((y2-y1)^2)+((z2-z1)^2))