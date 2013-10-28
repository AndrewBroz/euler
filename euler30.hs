----------------------------------
-- Project Euler problems 21-30 --
----------------------------------

import Control.Monad
import System.IO
import Data.List
import Data.List.Split (splitOn)
import Data.Char (ord)

----------------------------------
-- problem 21: Evaluate the sum of all the amicable numbers under 10000.


solution21 = sum $ filter amicable [1..9999]


amicable :: Int -> Bool
amicable n   = n /= n' && n == (dSum n')
    where n' = dSum n

dSum :: Int -> Int
dSum n = sum $ properDivs n

properDivs :: Int -> [Int]
properDivs n = [ x | x <- [1..div n 2], n `rem` x == 0]


----------------------------------
-- problem 22:

solution22 = getResult22 "names.txt"

getResult22 :: FilePath -> IO Int
getResult22 fileName = do
    contents <- readFile fileName
    return . sum . toValues $ toNameList contents

toValues :: [String] -> [Int]
toValues = zipWith tupleToValue [1..]

tupleToValue :: (Int, String) -> Int
tupleToValue (idx, name) = idx * nameToValue name

nameToValue :: String -> Int
nameToValue = sum . map (\c -> ord c - ord 'A' + 1)

toNameList :: String -> [String]
toNameList contents = sort . read $ "[" ++ contents ++ "]"

----------------------------------
-- problem 23:


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
