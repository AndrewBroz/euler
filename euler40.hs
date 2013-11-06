----------------------------------
-- Project Euler problems 31-40 --
----------------------------------

import Data.List

----------------------------------
-- problem 31:


----------------------------------
-- problem 32:


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
