----------------------------------
-- Project Euler problems 21-30 --
----------------------------------

import Control.Monad
import System.IO
import Data.List
import Data.Char (ord)

----------------------------------
-- problem 21: Evaluate the sum of all the amicable numbers under 10000.

solution21 = sum $ filter amicable [1..9999]
-- 31626

amicable :: Int -> Bool
amicable n = n /= n' && n == n''
    where n' = sum $ properDivisors n
          n'' = sum $ properDivisors n'

properDivisors :: Int -> [Int]
properDivisors 0 = []
properDivisors n = init $ divisors n

divisors :: Int -> [Int]
divisors n = xs ++ reverse (map (\x -> div n x) xs')
    where xs = smallDivisors n
          -- if n is a perfect square then remove duplicate root:
          xs' = if (last xs)^2 == n then init xs else xs

smallDivisors :: Int -> [Int]
smallDivisors n = [ x | x <- [1..floor . sqrt $ fromIntegral n], rem n x == 0 ]


----------------------------------
-- problem 22:

solution22 = getResult22 "names.txt"
-- 871198282

getResult22 :: FilePath -> IO Int
getResult22 fileName = do
    contents <- readFile fileName
    return . sum . toValues $ toNameList contents

toValues :: [String] -> [Int]
toValues names = zipWith toValue [1..] names

toValue :: Int -> String -> Int
toValue idx name = idx * nameToValue name

nameToValue :: String -> Int
nameToValue = sum . map (\c -> ord c - ord 'A' + 1)

toNameList :: String -> [String]
toNameList contents = sort . read $ "[" ++ contents ++ "]"

----------------------------------
-- problem 23: Find the sum of all the positive integers which
-- cannot be written as the sum of two abundant numbers.

abundantNums = [ n | n <- [1..], n < sum (properDivisors n) ] :: [Int]




----------------------------------
-- problem 24:


----------------------------------
-- problem 25:


----------------------------------
-- problem 26:


----------------------------------
-- problem 27:


----------------------------------
-- problem 28:


----------------------------------
-- problem 29:


----------------------------------
-- problem 30:


----------------------------------
