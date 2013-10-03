---------------------------------
-- Project Euler problems 1-10 --
---------------------------------

import Data.List (union)
import Data.Char (digitToInt)
import Control.Monad

---------------------------------
-- problem 1: sum multiples of 3 or 5 less than n, n = 1000


-- first solution, adequate for n = 1000, slow for large n
solution1 :: Int
solution1 = sum3or5 1000

sum3or5 :: Int -> Int
sum3or5 n     = sum $ filter f (take n [0..])
    where f x = x `mod` 3 == 0 || x `mod` 5 == 0


-- optimized algorithm, appropriate for large n
solution1' :: Integer
solution1' = sum3or5' 1000

sumDivBy :: Integer -> Integer -> Integer
sumDivBy n m = n * (p * (p + 1)) `div` 2
    where p   = (m - 1) `div` n

sum3or5' :: Integer -> Integer
sum3or5' m = sumDivBy 3 m + sumDivBy 5 m - sumDivBy 15 m


-- a very short solution using union from Data.List
solution1'' = sum (union [3,6..999] [5,10..999])


{- outline of a more general solution
sumDivByOr :: [Integer] -> Integer -> Integer
sumDivByOr ns m    = sumAll - sumMults
    where sumAll   = sum $ map (`sumDivBy'` m) ns
          sumMults = sum $ map (`sumDivBy'` m) mults
          mults    = ???

sum3or5'' :: Int -> Int
sum3or5'' = sumDivByOr [3,5]
-}


---------------------------------
-- problem 2: sum of even valued Fibonacci numbers less than 4 million

-- first solution
solution2 :: Int
solution2 = sumEvenFibs 4000000

sumEvenFibs :: Int -> Int
sumEvenFibs n = sum $ filter even $ takeWhile (<n) fibs

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


-- alternately, define the sequence of even Fibonacci numbers
evenFibs :: [Int]
evenFibs = 0 : 2 : zipWith (\a b -> a + 4 * b) evenFibs (tail evenFibs)

sumEvenFibs' :: Int -> Int
sumEvenFibs' n = sum $ takeWhile (<n) evenFibs


-- a nonrecursive solution appropriate for very large n
-- using matricies
sumEvenFibs'' :: Int -> Int
sumEvenFibs'' n = (a + b - 1) `div` 2
    where n2 = n `div` 2
          (a, b) = foldr f (0,1)
                   . takeWhile ((<= n2) . fst)
                   . iterate times2E $ (1, 4)
          f x y | fst z <= n2 = z
                | otherwise   = y
            where z = x `addE` y

addE :: (Int, Int) -> (Int, Int) -> (Int, Int)
addE (a, b) (c, d) = (a*d + b*c - 4*a*c, a*c + b*d)

times2E :: (Int, Int) -> (Int, Int)
times2E (a, b) = addE (a, b) (a, b)


---------------------------------
-- problem 3: What is the largest prime factor
-- of the number 600851475143?

solution3 :: Int
solution3 = gpf 600851475143

gpf :: Int -> Int
gpf = gpf' 2

gpf' :: Int -> Int -> Int
gpf' 2 n
    | n `mod` 2 == 0 = gpf' 2 (n `div` 2)
    | otherwise      = gpf' 3 n
gpf' x n
    | n `mod` x == 0 = gpf' 2 (n `div` x)
    | test           = gpf' (x + 2) n
    | otherwise      = n
    where test = x < (floor . sqrt $ fromIntegral n)


-- another solution



---------------------------------
-- problem 4: Find the largest palindrome made
-- from the product of two 3-digit numbers.

-- first solution
solution4 :: Int
solution4 = head $ filter prodTwo3DigNums $ descPalindromes 999999

isPalindrome :: Int -> Bool
isPalindrome n = show n == reverse (show n)

descPalindromes :: Int -> [Int]
descPalindromes n = filter isPalindrome [n,n-1..0]

prodTwo3DigNums :: Int -> Bool
prodTwo3DigNums x        = threeDigitDivs /= []
    where m              = floor . sqrt $ fromIntegral x
          n              = x `div` 999
          threeDigitDivs = filter (\v -> 0 == x `mod` v) [m,m-1..n]

-- a much shorter solution
solution4' = maximum [ x | y <- [100..999], z <- [y..999]
                     , let x = y * z
                     , let s = show x
                     , s == reverse s
                     ]


---------------------------------
-- problem 5: What is the smallest positive number that is
-- evenly divisible by all of the numbers from 1 to 20?

-- first solution
solution5 :: Int
solution5 = divTo 20

divTo :: Int -> Int
divTo n = foldl1 lcm [1..n]


---------------------------------
-- problem 6: Find the difference between the sum of the squares of
-- the first one hundred natural numbers and the square of the sum

-- first solution
solution6 :: Int
solution6 = diffSquares 100

diffSquares :: Int -> Int
diffSquares n = squareSum n - sumSquares n

squareSum :: Int -> Int
squareSum n = (sum $ [1..n])^2

sumSquares :: Int -> Int
sumSquares n = sum $ map (^2) [1..n]


---------------------------------
-- problem 7: What is the 10001st prime number?

-- first solution
solution7 :: Int
solution7 = primeNum 10001

primeNum :: Int -> Int
primeNum n = primes !! n-1


-- a nice definition of primes
primes = 2 : filter ((== 1) . length . primeFactors) [3,5..]

primeFactors n = factor n primes
    where
        factor n (p:ps) 
            | p * p > n      = [n]
            | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
            | otherwise      = factor n ps


---------------------------------
-- problem 8: Find the greatest product of five
-- consecutive digits in the 1000-digit number.

-- first solution
solution8 :: Int
solution8 = greatestProduct $ stringToInts
    ("73167176531330624919225119674426574742355349194934" ++
    "96983520312774506326239578318016984801869478851843" ++
    "85861560789112949495459501737958331952853208805511" ++
    "12540698747158523863050715693290963295227443043557" ++
    "66896648950445244523161731856403098711121722383113" ++
    "62229893423380308135336276614282806444486645238749" ++
    "30358907296290491560440772390713810515859307960866" ++
    "70172427121883998797908792274921901699720888093776" ++
    "65727333001053367881220235421809751254540594752243" ++
    "52584907711670556013604839586446706324415722155397" ++
    "53697817977846174064955149290862569321978468622482" ++
    "83972241375657056057490261407972968652414535100474" ++
    "82166370484403199890008895243450658541227588666881" ++
    "16427171479924442928230863465674813919123162824586" ++
    "17866458359124566529476545682848912883142607690042" ++
    "24219022671055626321111109370544217506941658960408" ++
    "07198403850962455444362981230987879927244284909188" ++
    "84580156166097919133875499200524063689912560717606" ++
    "05886116467109405077541002256983155200055935729725" ++
    "71636269561882670428252483600823257530420752963450")

greatestProduct :: [Int] -> Int
greatestProduct = greatestProduct' 0

greatestProduct' :: Int -> [Int] -> Int
greatestProduct' p ( x1 : x2 : x3 : x4 : [] ) = p
greatestProduct' p ( x1 : x2 : x3 : x4 : x5 : xs )
    | p < p'    = greatestProduct' p' ( x2 : x3 : x4 : x5 : xs )
    | otherwise = greatestProduct' p  ( x2 : x3 : x4 : x5 : xs )
    where p'    = x1 * x2 * x3 * x4 * x5

stringToInts :: [Char] -> [Int]
stringToInts cs = map digitToInt cs


---------------------------------
-- problem 9: There exists exactly one Pythagorean triplet
-- for which a + b + c == 1000. Find the product abc.

solution9 :: Int
solution9 = pyTriple 1000


-- a + b + c == 1000 <=> c = 1000 - a - b,
-- which gives us a^2 + b^2 == (1000 - a - b)^2
pyTriple :: Int -> Int
pyTriple n = head $
    do a <- [1..n]
       b <- [1..n]
       guard (a < b)
       guard (a^2 + b^2 == (n - a - b)^2)
       return $ a * b * (n - a - b)


-- this can be optimized in a few different ways


---------------------------------
-- problem 10: Find the sum of all the primes below two million.

solution10 :: Int
solution10 = sumPrimes 2000000


-- a simple version, depends on how nice the primes def is
sumPrimes :: Int -> Int
sumPrimes m = sum $ takeWhile (<m) primes


---------------------------------
