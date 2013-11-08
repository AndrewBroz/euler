----------------------------------
-- Project Euler problems 41-50 --
----------------------------------

import Data.List
import Data.Char (ord)

----------------------------------
-- problem 41:

isPrime :: Integral a => a -> Bool
isPrime x = not . any divisible . takeWhile small $ 2 : [3,5..] where
     divisible y = mod x y == 0
     small y     = y * y <= x

listToNumber :: Integral a => [a] -> a
listToNumber = foldl' (\a b -> 10*a+b) 0

candidates :: Integral a => [a]
candidates = map listToNumber . reverse . sort $ permutations [1..7]

solution41 = head $ filter isPrime candidates

----------------------------------
-- problem 42:

triNum :: Int -> Int
triNum n = n * (n + 1) `div` 2

isTriNum :: Int -> Bool
isTriNum n = n == triNum x
    where x = floor . sqrt . fromIntegral $ 2 * n

wordToValue :: String -> Int
wordToValue s = sum $ map f s
    where f c = ord c - ord 'A' + 1 

toWordList :: String -> [String]
toWordList contents = read $ "[" ++ contents ++ "]"

getResult42 :: FilePath -> IO Int
getResult42 fileName = do
    contents <- readFile fileName
    return . length . filter (isTriNum . wordToValue) $ toWordList contents

solution42 = getResult42 "42.txt"
-- 162 (0.03 secs, 55949704 bytes)

----------------------------------
-- problem 43:


----------------------------------
-- problem 44:


----------------------------------
-- problem 45:


----------------------------------
-- problem 46:


----------------------------------
-- problem 47:


----------------------------------
-- problem 48: Find the last ten digits of the series,
-- 1^1 + 2^2 + 3^3 + ... + 1000^1000.


solution48 = sum [ x^x | x <- [1..1000] ] :: Integer


----------------------------------
-- problem 49:


----------------------------------
-- problem 50:


----------------------------------
