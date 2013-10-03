----------------------------------
-- Project Euler problems 21-30 --
----------------------------------


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
