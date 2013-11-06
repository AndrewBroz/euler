----------------------------------
-- Project Euler problems 21-30 --
----------------------------------

import Control.Monad
import System.IO
import Data.List
import Data.IntSet (toList, fromList)
import Data.Char (ord, chr, digitToInt)
import qualified Data.Set as S

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
    where xs = divsToSqrt n
          -- if n is a perfect square then remove duplicate root:
          xs' = if (last xs)^2 == n then init xs else xs

divsToSqrt :: Int -> [Int]
divsToSqrt n = [ x | x <- [1..floor . sqrt $ fromIntegral n], rem n x == 0 ]


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

upperLimit23 = 20161 :: Int -- all numbers larger can be written 
                     -- as the sum of two abundant numbers

abundantNums :: [Int]
abundantNums = [ n | n <- [1..], n < sum (properDivisors n) ]

-- nub removes duplicates from a list
-- HACK: using (toList . fromList) from Data.IntSet faster
abundantSums :: [Int]
abundantSums = toList .
               fromList $
               sort [ a + b |
                    a <- takeWhile (<= upperLimit23) abundantNums,
                    b <- takeWhile (<= upperLimit23 - a) abundantNums,
                    a <= b ]

nonabundantSums :: [Int]
nonabundantSums = [1..upperLimit23] \\ abundantSums

solution23 = sum nonabundantSums
-- a different approach, but not significantly faster
solution23' = (sum [1..upperLimit23]) - (sum abundantSums)
--4179871


----------------------------------
-- problem 24:

solution24 = (sort $ permutations "0123456789") !! 999999
-- "2783915460" (8.12 secs, 3152813656 bytes)


{- The original way I did this problem:

-- think of representation by a factorial base, so 2*9! + 6*8! + ...
-- so, divide by each succesive factorial and store the result in a list,
-- which will represent these values

fac :: Int -> Int
fac n = product [1..n]

idxList :: Int -> [Int] -> Int -> [Int]
idxList _ ls 0 = reverse (0 : ls)
idxList idx ls maxN = idxList (idx `rem` (fac maxN))
                                ((idx `div` (fac maxN)) : ls)
                                (maxN - 1)

findPerm :: [Int] -> [Int] -> [Int] -> Int
findPerm [] _ acc = sum $ zipWith (*) (map (\n->10^n) [0..]) acc
findPerm ls idxs acc = findPerm (ls \\ [val]) (tail idxs) (val : acc)
    where val = (ls !! head idxs)

solution24' = findPerm [0..9] idxs [] 
    where idxs = idxList 999999 [] 9 :: [Int]
-- 2783915460 (0.01 secs, 3646096 bytes) -}


-- refactored code:
fac n = product [1..n]

findPerm [] _ = []
findPerm xs idx = x : findPerm (delete x xs) (idx `rem` b)
    where b = fac $ length xs - 1
          x = xs !! (idx `div` b)

-- 2783915460 (0.01 secs, 3639368 bytes)
solution24'    = sum $ zipWith (*) pwrs perm
    where pwrs = iterate (*10) 1 -- map (10^) [0..]
          perm = reverse $ findPerm [0..9] 999999


----------------------------------
-- problem 25: Which is the first fib with more than 1000 digits?

-- here's the easy way to do this in Haskell:
solution25' = length $ takeWhile (< 10^999) fibs
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Binet's formula with a double for rt5 fails for even relatively small n 
-- (notice this version uses fact that |psi|^n / rt5 < 1/2 for n >= 0)
fib' :: Integer -> Integer
fib' n = round $ phi^n / rt5
    where phi = (1 + rt5) / 2
          rt5 = sqrt 5 :: Double

-- we need a more sophisticated closed form that doesn't rely on doubles:
-- for that, we need to pull out algebra. Rather than using doubles, we 
-- can work in the field Z[phi], that is, numbers of the form a + b*phi,
-- a and b integers. We create a new data type Zf:
data Zf = Zf !Integer !Integer
    deriving (Eq, Show)

-- and give method instances of key functions
instance Num Zf where
    fromInteger n = Zf n 0
    negate (Zf a b) = Zf (-a) (-b)
    (Zf a b) + (Zf c d) = Zf (a + c) (b + d)
    -- observe that phi^2 = 1 + phi, so:
    (Zf a b) * (Zf c d) = Zf (a * c + b * d) (a * d + b * c + b * d)
    abs zf = zf -- filler
    signum _ = 1 -- filler

-- which leads us to our fib function. We use Binet's formula again, but
-- our choice of Z[phi] leads to an elegant expression:
fib :: Integer -> Integer
fib n = let Zf _ x = phi^n in x
    where phi = Zf 0 1

solution25 = head $ filter (\x -> fib x > 10^999) [1..]
-- 4782

----------------------------------
-- problem 26:

-- uses primes defined below
solution26 = head $ filter (\p -> (mod (10^(div (p-1) 2) - 1) p) /= 0) ls
    where ls = reverse $ takeWhile (<1000) primes
-- 983 (0.06 secs, 7303528 bytes)

----------------------------------
-- problem 27:

-- a nice corecursive definition of primes
primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
    where factor n (p:ps) 
            | p * p > n      = [n]
            | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
            | otherwise      = factor n ps

isPrime :: Integer -> Bool
isPrime n = n > 1 && 1 == (length $ primeFactors n)

candidateList :: [(Integer, Integer)]
candidateList =  [ (a, b) |
                 b <- takeWhile (< 1000) primes,
                 a <- [(-b), ((-b) + 2) .. 999],
                 isPrime $ 1 + a + b
                 ]

seqLen :: (Integer, Integer) -> Int 
seqLen (a,b) = length $ takeWhile (\n -> isPrime $ n^2 + a * n + b) [0..]

findLg :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
findLg (a,b) (c,d) | seqLen (a,b) >= seqLen (c,d) = (a,b)
                   | otherwise                    = (c,d)

solution27 = let (a,b) = foldl1' findLg candidateList in (a * b, (a, b))
-- (-59231,(-61,971)) (29.47 secs, 11341876088 bytes)

-- observation: (b^2 + a*b + b) is never prime, so once b is less than the
-- longest sequence generated we have the answer. This can be optimized to
-- use this fact.
-- TODO Refactor and optimize code

----------------------------------
-- problem 28:

-- sum $ scanl (+) 1 [ 2 * floor(1 + 0.25 * fromIntegral n)| n <- [0..1999]]
-- 1 + 4 * sum [n * (4 * n + 1) + 1 | n <- [1..500]]
-- (4*x^3 + 3*x^2 + 8*x - 9) `div` 6

solution28 = sum $ diagonals 1001
-- 669171001 (0.01 secs, 5255856 bytes)

diagonals :: Int -> [Int]
diagonals x = scanl (+) 1 incList where
    incList = take (2 * x - 2) $ concatMap (replicate 4) [2,4..]

----------------------------------
-- problem 29: How many distinct terms are in the sequence generated by a^b
-- for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates   = helper S.empty where
    helper _ []    = []
    helper a (b:c) | S.member b a = helper a c
                   | otherwise      = b : helper (S.insert b a) c

solution29 =  length $ removeDuplicates [a^b | a <- [2..100], b <- [2..100]]
-- 9183 (0.07 secs, 34562376 bytes)

-- this can also be done without the removeDuplicates function by using the
-- Data.Map module fromList and size functions, or simply using the
-- built-in 'nub' function instead of removeDuplicates.

----------------------------------
-- problem 30: Find the sum of all the numbers that can be written
-- as the sum of fifth powers of their digits.

-- note that a clear upper bound on such numbers is 6*9^5 == 354294
-- we can move it down further incrementally: 3^5+5*9^5 == 295488
-- and 2^5+5*9^5 == 295277. Furthermore, 199999 exceeds itself, so
-- 289999 -> 268996, 199998 exceeds itself, 279999 -> 253035
-- [this line of thought bounds based on maximizing and minimizing
-- strings], 199997 exceeds itself, 269999 -> 244004, 199996 exceeds
-- itself, and [230999,231999..239999] all are deficient, thus 229999
-- is a bound

bound30 = 229999 :: Int -- could be improved a little, maybe, but eh

fifthPowerSum :: Int -> Int
fifthPowerSum 0 = 0
fifthPowerSum n = (n `rem` 10)^5 + fifthPowerSum (n `div` 10)

solution30 = sum $ filter (\n -> n == fifthPowerSum n) [2..bound30]
-- 443839i (2.19 secs, 1582552168 bytes)

-- the simple solution is already okay, but an interesting
-- observation: numbers with the same digits have the same sums, so it is
-- sufficent to test unordered collections of six digits. If the digits of
-- the fifth power sum of the collection are the members of the collection
-- then the sum is a solution.

sixDigitCollections :: [[Int]]
sixDigitCollections = tail [ [a,b,c,d,e,f] |
                           a <- [1..9], b <- [0..a],
                           c <- [0..b], d <- [0..c],
                           e <- [0..d], f <- [0..2] ]

fifthPowerSum' :: [Int] -> Int
fifthPowerSum' = sum . map (^5)

-- very fast solution: 443839 (0.07 secs, 57925400 bytes)
solution30' = sum $ map f sixDigitCollections
    where l = length . show . fifthPowerSum'
          addZeros xs = xs ++ take (6 - l xs) (repeat 0)
          toColl = addZeros . reverse . sort .
                   map digitToInt . show . fifthPowerSum'
          f xs | xs == toColl xs = fifthPowerSum' xs
               | otherwise = 0


----------------------------------
