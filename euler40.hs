----------------------------------
-- Project Euler problems 31-40 --
----------------------------------

import Data.List
import Control.Monad

----------------------------------
-- problem 31:

coins = [1, 2, 5, 10, 20, 50, 100, 200] :: [Int]


----------------------------------
-- problem 32:

{- First working attempt:
-- helper function takes string to int
rInt :: String -> Int
rInt = read

-- multiplicand/multiplier/product
mmp :: String -> [(Int, Int, Int)]
mmp ls = [ (rInt md1, rInt mp1, rInt p)
         , (rInt md2, rInt mp2, rInt p) ]
    where (mm, p) = splitAt 5 ls
          (md1, mp1) = splitAt 2 mm -- case 1
          (md2, mp2) = splitAt 1 mm -- case 2

mmps :: [(Int, Int, Int)]
mmps = concat . map mmp $ permutations "123456789"

isPan :: (Int, Int, Int) -> Bool
isPan (md, mp, p) = md * mp == p

panProds :: [Int]
panProds = nub $ map (\(a,b,c) -> c) panIds
    where panIds = filter isPan mmps

solution32 = sum panProds
-- 45228 (11.77 secs, 17427823088 bytes) 
-}
-- The above approach takes about 12 seconds and is pretty brutish.
-- Improving the code shouldn't be too hard. Here's another approach that
-- also doesn't do many clever things, but gets a result twenty-five times
-- faster.

isPandigital :: Int -> Bool
isPandigital n = loop [] n
    where loop ls 0 = ls == (delete 0 $ nub ls) -- delete 0 (want 1..9)
          loop ls n = loop (rem n 10 : ls) (quot n 10)
          
case1 :: [(Int, Int, Int)]
case1 = [ (a, b, c) |
        a <- [1..9], b <- [1234..9876], -- case1: 1 and 4 digits
        let c = a * b, c < 9877,        -- upper bound on product
        isPandigital $ a + 10 * b + 100000 * c ]

case2 :: [(Int, Int, Int)]
case2 = [ (a, b, c) |
        a <- [12..98], b <- [123..987],    -- case2: 2 and 3 digits
        let c = a * b, c > 1233, c < 9877, -- upper/lower bounds
        isPandigital $ a + 100 * b + 100000 * c ]

pandigitalTriples = case1 ++ case2

solution32 = sum . nub $ map (\(a,b,c) -> c) pandigitalTriples
-- 45228 (0.43 secs, 202786912 bytes)

-- But can we do better than this? We know that we only need to find
-- possible 1,4 and 2,3 multiplicand/multiplier pairs for 9_C_5 digits,
-- with two possible cases (one digit and four or two and three digits),
-- then test if the result contains the remaining four digits. 

-- The following is adapted from Jedai's solution. It's actually slower
-- (why?), but it realizes the above thoughts about solving the problem:
 
-- returns tuples of all combinations of n elements from xs and the rest
combinations :: (Integral a, Eq b) => a -> [b] -> [([b], [b])] 
combinations 0 xs = [ ([], xs) ]
combinations n xs = [ (y : ys, rest) |
                    y <- xs,
                    (ys, rest) <- combinations (n-1) (delete y xs) ]
 
listToNumber :: Integral a => [a] -> a
listToNumber = foldl' (\a b -> 10*a+b) 0
 
swap (a,b) = (b,a) -- used in the explode function
 
-- takes a number and returns a reversed list of its digits
explode :: Integral a => a -> [a]
explode = unfoldr (\a -> if a == 0 then Nothing
                         else Just . swap $ quotRem a 10)
 
pandigitalProducts :: Integral a => [a]
pandigitalProducts =
    nub $ do (beg,end) <- combinations 5 [1..9]
             n <- [1,2]
             let (a,b) = splitAt n beg
                 prod  = listToNumber a * listToNumber b
             guard $ sort (explode prod) == end
             return prod

solution32' = sum pandigitalProducts
-- 45228 (0.53 secs, 215520976 bytes)

-- TODO Is it possible to make a faster version?

----------------------------------
-- problem 33:


----------------------------------
-- problem 34:


----------------------------------
-- problem 35: How many circular primes are there below one million?

-- note there are thirteen such primes below 100:
-- 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97

primes :: [Int]
primes = 2 : filter isPrime [3,5..]

primeFactors :: Int-> [Int]
primeFactors n = factor n primes
    where factor n (p:ps) 
            | p * p > n      = [n]
            | n `rem` p == 0 = p : factor (n `div` p) (p:ps)
            | otherwise      = factor n ps

--TODO consider optimizations
isPrime :: Int -> Bool
isPrime n = n > 1 && 1 == (length $ primeFactors n)

rotations :: [a] -> [[a]]
rotations ls = init (zipWith (++) (tails ls) (inits ls))

intRotations :: Int -> [Int]
intRotations = map read . rotations . show

solution35 = length . filter circular $ takeWhile (<10^6) primes
    where circular = all isPrime . intRotations

----------------------------------
-- problem 36:


----------------------------------
-- problem 37:


----------------------------------
-- problem 38:


----------------------------------
-- problem 39:


----------------------------------
-- problem 40:


----------------------------------
