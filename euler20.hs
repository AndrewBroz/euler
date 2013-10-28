----------------------------------
-- Project Euler problems 11-20 --
----------------------------------

import Control.Monad
import Data.List
import System.IO
import Data.Char

----------------------------------
-- problem 11:


solution11 = getResult11 "11.txt"
-- 70600674


getResult11 :: FilePath -> IO Int
getResult11 fileName = do
    contents <- readFile fileName
    return . hvdMax' $ toGrid contents

toGrid :: String -> [[Int]]
toGrid = map (map read . words) . lines


-- first solution
hvdMax :: [[Int]] -> Int
hvdMax grid = maximum [ h, v, d ]
    where h = hMax 0 grid
          v = vMax 0 grid
          d = dMax 0 grid

hMax, vMax, dMax :: Int -> [[Int]] -> Int
hMax p [] = p
hMax p ( xs : xss )
    | p < p'    = hMax p' xss
    | otherwise = hMax p  xss
    where p'    = gp4 0 xs
vMax p ( as : bs : cs : [] ) = p
vMax p ( as : bs : cs : ds : nss )
    | p < p'    = vMax p' ( bs : cs : ds : nss )
    | otherwise = vMax p  ( bs : cs : ds : nss )
    where p'    = vMaxH 0  [ as, bs, cs, ds ]
dMax p ( as : bs : cs : [] ) = p
dMax p ( as : bs : cs : ds : nss )
    | p < p'    = dMax p' (bs:cs:ds:nss)
    | otherwise = dMax p  ( bs : cs : ds : nss )
    where p'    = dMaxH 0  [ as, bs, cs, ds ]


gp4 :: Int -> [Int] -> Int
gp4 p ( x1 : x2 : x3 : [] ) = p
gp4 p ( x1 : x2 : x3 : x4 : xs )
    | p < p'    = gp4 p' ( x2 : x3 : x4 : xs )
    | otherwise = gp4 p  ( x2 : x3 : x4 : xs )
    where p'    = x1 * x2 * x3 * x4

vMaxH, dMaxH :: Int -> [[Int]] -> Int
vMaxH p [ [], [] , [], [] ] = p
vMaxH p [ a:as, b:bs, c:cs, d:ds ]
    | p < p'    = vMaxH p' [ as, bs, cs, ds ]
    | otherwise = vMaxH p  [ as, bs, cs, ds ]
    where p'    = a * b * c * d
dMaxH p [ a1 : a2 : a3 : []
        , b1 : b2 : b3 : []
        , c1 : c2 : c3 : []
        , d1 : d2 : d3 : []
        ] = p
dMaxH p [ a1 : a2 : a3 : a4 : as
        , b1 : b2 : b3 : b4 : bs
        , c1 : c2 : c3 : c4 : cs
        , d1 : d2 : d3 : d4 : ds
        ]
    | p < p'    = dMaxH p' [ a2 : a3 : a4 : as
                           , b2 : b3 : b4 : bs
                           , c2 : c3 : c4 : cs
                           , d2 : d3 : d4 : ds
                           ]
    | otherwise = dMaxH p  [ a2 : a3 : a4 : as
                           , b2 : b3 : b4 : bs
                           , c2 : c3 : c4 : cs
                           , d2 : d3 : d4 : ds
                           ]
    where p'    = max ( a1 * b2 * c3 * d4 ) ( a4 * b3 * c2 * d1 )


-- a much cleaner solution using transpose, zipWith and drop
myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum xs = maximum xs

takeBy :: Int -> [Int] -> [[Int]]
takeBy n = filter ((n==) . length) . map (take n) . tails

maxBy :: Int -> [Int] -> Int
maxBy n = myMaximum . map product . takeBy n

hMax', vMax', ldMax, rdMax :: Int -> [[Int]] -> Int
hMax' n = maximum . map (maxBy n)
vMax' n = hMax' n . transpose
ldMax n = hMax' n . transpose . zipWith drop [0..]
rdMax n = ldMax n . map reverse

hvdMax' :: [[Int]] -> Int
hvdMax' grid = maximum $ map (flip ($ 4) grid) [hMax',vMax',ldMax,rdMax]


----------------------------------
-- problem 12: What is the value of the first triangle number
-- to have over five hundred divisors?


solution12 = triFun 500
-- 76576500


-- pretty good solution
numFactors :: Int -> Int
numFactors n = if m * m == n then 2 * l - 1 else 2 * l
    where m  = floor . sqrt $ fromIntegral n
          l  = length $ [ x | x <- [1..m], rem n x == 0]

triNum :: Int -> Int
triNum n = ((n+1) * n) `div` 2

numTriFacs :: Int -> Int
numTriFacs n
    | even n = (numFactors $ div n 2) * (numFactors (n+1))
    | odd  n = (numFactors $ div (n+1) 2) * (numFactors n)

triFun :: Int -> Int
triFun m = triNum . head $ filter (\n -> m < numTriFacs n) [1..]


----------------------------------
-- problem 13: Get the first ten digits of the sum
-- of fifty 50 digit integers.


solution13 = getFirst10 "13.txt"
-- 2658544435


getFirst10 :: FilePath -> IO Int
getFirst10 fileName = do
    contents <- readFile fileName
    return . (10 `firstDigits`) . sum $ toIntegers contents

firstDigits :: Int -> Integer -> Int
firstDigits n = read . take n . show

toIntegers :: String -> [Integer]
toIntegers = map read . lines


----------------------------------
-- problem 14: Which starting number, under one million,
-- produces the longest Collatz sequence?


solution14 = ronJeremy 1000000 1 (1,1)
-- 837799


ronJeremy :: Int -> Int -> (Int,Int) -> Int
ronJeremy m count (n,l)
    | count >= m = n
    | l < l'     = ronJeremy m (count+1) (count,l')
    | otherwise  = ronJeremy m (count+1) (n,l)
    where l'     = collatzLength count

collatzLength :: Int -> Int
collatzLength = collatzLength' 0

collatzLength' :: Int -> Int -> Int
collatzLength' counter n
    | n < 1  = 0
    | n == 1 = counter + 1
    | even n = collatzLength' (counter + 1) (div n 2)
    | odd  n = collatzLength' (counter + 1) (3*n + 1)


----------------------------------
-- problem 15: How many down-right paths exist through a 20x20 grid?


solution15 = div (fac 40) $ fac 20 * fac 20
    where fac n = product [1..n]
-- 137846528820


----------------------------------
-- problem 16: What is the sum of the digits of the number 2^1000?


solution16 = sum . map digitToInt . show $ 2^1000
-- 1366


----------------------------------
-- problem 17:


solution17 = sum $ map lengthFind [1..1000]
-- 21274


to19 =
    map length ["","one","two","three","four","five"
               ,"six","seven","eight","nine","ten","eleven"
               ,"twelve","thirteen","fourteen","fifteen"
               ,"sixteen","seventeen","eighteen","nineteen"
               ]

tens =
    map length ["","","twenty","thirty","forty","fifty"
               ,"sixty","seventy","eighty","ninety"
               ]

lengthFind :: Int -> Int
lengthFind n
    | n == 1000 = 11
    | n < 20    = to19 !! n
    | n < 100   = tens !! t + to19 !! o
    | test      = to19 !! h + 7
    | n < 1000  = to19 !! h + 10 + lengthFind (n `mod` 100)
    | otherwise = 0
    where o     = n `mod` 10
          t     = (n - o) `mod` 100 `div` 10
          h     = (n - t - o) `mod` 1000 `div` 100
          test  = mod n 100 == 0


----------------------------------
-- problem 18: Find the maximum total from top to bottom of the triangle.


solution18 = getMaxSum "18.txt"


getMaxSum :: FilePath -> IO Int
getMaxSum fileName = do
    contents <- readFile fileName
    return . myMax . triVals . reverse $ toGrid contents

myMax :: [[Int]] -> Int
myMax [ns] = maximum ns
myMax _    = 0

-- takes an upside-down triangle as input
triVals :: [[Int]] -> [[Int]]
triVals [ns]        = [ns]
triVals (as:bs:nss) = triVals $ collapse as bs : nss

collapse :: [Int] -> [Int] -> [Int]
collapse _ []              = []
collapse (x1:x2:xs) (y:ys) = max (x1+y) (x2+y) : collapse (x2:xs) ys


----------------------------------
-- problem 19: How many Sundays fell on the first of the month
-- during the twentieth century (1 Jan 1901 to 31 Dec 2000)?


-- This solution is really verbose and silly for such a tiny problem.
-- BUT! It works. And is very very literal.

solution19 = length firstSundays
-- 171

type Weekday = Char
weekdays  = [ 'S', 'M', 'T', 'W', 'R', 'F', 'A' ] :: [Weekday]
-- 1 Jan 1901 was a Tuesday, so drop Sunday and Monday:
weekCycle = drop 2 weekdays ++ cycle weekdays :: [Weekday]

type SmallDate = (Int, Int)
stdDays :: [SmallDate]
stdDays   = [ (m,d) |
						m <- [1..12],
						d <- [1..31],
						(m `elem` [4, 6, 9, 11]) <= (d <= 30),
						(m == 2) <= (d <= 28)
						]
leapDays :: [SmallDate]
leapDays  = [ (m,d) |
						m <- [1..12],
						d <- [1..31],
						(m `elem` [4, 6, 9, 11]) <= (d <= 30),
						(m == 2) <= (d <= 29)
						]

type Year = Int
isLeap :: Year -> Bool
isLeap y  = y `mod` 400 == 0 || y `mod` 100 /= 0 && y `mod` 4 == 0
isntLeap  = not . isLeap
years			= [1901..2000] :: [Year]
leapYears = [ y | y <- years, isLeap y ] :: [Year]
stdYears  = [ y | y <- years, isntLeap y ] :: [Year]

type Date = (Year, SmallDate)
stdDates  = [ (y, sd) | y <- stdYears, sd <- stdDays ] :: [Date]
leapDates = [ (y, sd) | y <- leapYears, sd <- leapDays ] :: [Date]
dates     = sort $ stdDates ++ leapDates :: [Date]

type FullDate = (Date, Weekday)
fullDates = zip dates weekCycle :: [FullDate]

getWeekday :: FullDate -> Weekday
getWeekday (_,w) = w

getDay'' :: SmallDate -> Int
getDay'' (_,d) = d
getDay' :: Date -> Int
getDay' (_,sd) = getDay'' sd
getDay :: FullDate -> Int
getDay (dt,_) = getDay' dt

firstSundays :: [FullDate]
firstSundays = [ s | s <- fullDates, getDay s == 1, getWeekday s == 'S' ]


-- My original impulse was to write the following, but I got it wrong
-- the first time I tried it. Second time's a charm, eh?

solution19' = (sum . map (snd . year)) [1901..2000]
-- 171

nonLeap = [31,31,28,31,30,31,30,31,31,30,31,30]
leap = [31,31,29,31,30,31,30,31,31,30,31,30]

monthsOf y | isLeap y = leap
           | otherwise = nonLeap

year 1900 = (6,2)
year y = h (foldl f (g,0) (monthsOf y))
        where f (a,b) c = (a+c,b+(if mod (a+c) 7==0 then 1 else 0))
              g = (fst . year) (y-1)
              h (a,b) = (mod a 7,b)


-- Of course, the most clever thing to do is as little as possible:
import Data.Time
import System.Locale

solution19'' = length . filter (== "1 01") $ map (formatTime t "%u %d" ) [b..e]
-- 171

b = fromGregorian 1901 1 1
e = fromGregorian 2000 12 31
t = System.Locale.defaultTimeLocale


----------------------------------
-- problem 20: Find the sum of the digits in the number 100!


solution20 = sum . map digitToInt . show $ product [1..100]
-- 648


----------------------------------
