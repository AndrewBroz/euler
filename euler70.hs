----------------------------------
-- Project Euler problems 61-70 --
----------------------------------

import Data.List

----------------------------------
-- problem 61:


----------------------------------
-- problem 62:


----------------------------------
-- problem 63:


----------------------------------
-- problem 64:


----------------------------------
-- problem 65:


----------------------------------
-- problem 66:


----------------------------------
-- problem 67: Find the maximum total from top to bottom of the triangle.


solution67 = getMaxSum "67.txt"


-- makes an upside-down triangle
toTriangle :: String -> [[Int]]
toTriangle = reverse . map (map read . words) . lines

getMaxSum :: FilePath -> IO Int
getMaxSum fileName = do
    contents <- readFile fileName
    return . extract . collapse $ toTriangle contents

extract :: [[Int]] -> Int
extract [[m]] = m
extract _     = 0

-- takes an upside-down triangle as input
collapse :: [[Int]] -> [[Int]]
collapse [[m]]       = [[m]]
collapse (as:bs:nss) = collapse $ collapse' as bs : nss

-- collapses three-value subtriangles
collapse' :: [Int] -> [Int] -> [Int]
collapse' _ []              = []
collapse' (x1:x2:xs) (y:ys) = max (x1+y) (x2+y) : collapse' (x2:xs) ys


-- a much more elegant solution by gauchopuro
triMax triangle = foldr1 reduce triangle
    where reduce a b = zipWith (+) a (zipWith max b (tail b))


----------------------------------
-- problem 68:


----------------------------------
-- problem 69:


----------------------------------
-- problem 70:


----------------------------------
