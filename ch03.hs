----
--   Name:               ch03.hs
--
--   Started:            Fri Sep 13 00:19:55 2024
--   Modifications:
--
--   Purpose:
--
--
--
--   Calling Sequence:
--
--
--   Inputs:
--
--   Outputs:
--
--   Example:
--
--   Notes:
--
----

-- module ch03

-- average :: Num -> Num -> Num
-- average :: Double -> Double -> Double
average :: Fractional a => a -> a -> a
average x y = (x + y) / 2

-- 3.5
cube :: Num a => a -> a
cube x = x^3

-- 3.6
-- pythag :: Num a => a -> a -> Double
pythag :: Double -> Double -> Double
pythag x y = sqrt (x^2 + y^2)

-- 3.7
milesPerGallon :: Double -> Double -> Double -> Double
milesPerGallon initial final consumed = (final - initial) / consumed

-- 3.11
isLonger :: [a] -> [a] -> Bool
isLonger [] _ = False
isLonger _ [] = True
isLonger (_:xs) (_:ys) = isLonger xs ys

-- 3.12
-- Ugh...
addLength :: [Int] -> [Int]
addLength xs = (length xs):xs
