----
--   Name:               ch04.hs
--
--   Started:            Mon Nov 11 22:57:34 2024
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

-- module ch04

-- 4.1
makeEven :: Int -> Int
makeEven n
    | even n = n
    | otherwise = n + 1

-- 4.2
-- further :: Double -> Double
further :: (Num a, Ord a) => a -> a
further x
    | x > 0 = x + 1
    | x < 0 = x - 1
    | otherwise = x

-- 4.4
-- ordered :: Num a => a -> a -> [a]
ordered :: Ord a => a -> a -> [a]
-- ordered :: Double -> Double -> [Double]
ordered a b
    | b < a = [b, a]
    | otherwise = [a, b]
-- ordered a b = if b < a then [b, a] else [a, b]

-- 4.9
makeOdd :: Int -> Int
makeOdd n = n + (mod (n + 1) 2)

-- 4.10
constrain :: Int -> Int -> Int -> Int
constrain x mn mx = max (min x mx) mn

-- 4.11
firstZero :: [Int] -> String
firstZero ns = checkIt ns ["first", "second", "third"]

checkIt :: [Int] -> [String] -> String
checkIt [] [] = "none"
checkIt (0:_) (l:_) = l
checkIt (_:ns) (_:ls) = checkIt ns ls

-- 4.12
cycle' :: Int -> Int -> Int
-- cycle' n n = 1
cycle' n l = (mod n l) + 1

howAlike :: Int -> Int -> String
howAlike a b
    | a == b = "the same"
    | even a && even b = "both even"
    | odd a && odd b = "both odd"
    | a < 0 && b < 0 = "both negative"
    | a > 0 && b > 0 = "both positive"
    | otherwise = "not alike"

sameSign :: Int -> Int -> Bool
sameSign x y = (x == 0 && y == 0) ||
               (x < 0 && y < 0) ||
               (x > 0 && y > 0)

sameSign1 :: Int -> Int -> Bool
sameSign1 x y
    | x == 0 = y == 0
    | x < 0 = y < 0
    | x > 0 = y > 0
    | otherwise = False

sameSign2 :: Int -> Int -> Bool
sameSign2 0 0 = True
sameSign2 x y = (x * y) > 0

sameSign3 :: Int -> Int -> Bool
sameSign3 x y = signum x == signum y

-- 4.16
fancy :: Int -> Int
fancy x
    | odd x && x > 0 = x * x
    | odd x && x < 0 = 2 * x
    | otherwise = div x 2
    -- | otherwise = x / 2

-- 4.17
categorize :: String -> String -> Bool
categorize "boy" "child" = True
categorize "girl" "child" = True
categorize "man" "adult" = True
categorize "woman" "adult" = True
categorize _ _ = False

-- 4.18
beats :: String -> String -> Bool
beats "rock" "scissors" = True
beats "scissors" "paper" = True
beats "paper" "rock" = True
beats  _ _ = False

play :: String -> String -> String
play a b
    | a == b = "tie"
    | beats a b = "first wins"
    | beats b a = "second wins"

-- 4.20
comp :: Ord a => a -> a -> String
comp a b
    | a == b = "numbers are the same"
    | a < b = "first is smaller"
    | otherwise = "first is bigger"
    