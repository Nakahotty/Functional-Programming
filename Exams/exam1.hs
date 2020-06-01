import Data.List(tails)

-- 1)
checkSequence :: [Int] -> Bool 
checkSequence (x1:x2:xs) 
    | xs == []         = True 
    | x2 `mod` x1 == 0 = False
    | otherwise        = x1 < x2 && checkSequence (x2:xs)

-- 2)
removeNb :: Int -> [(Int, Int)]
removeNb n = [(a,b) | a <- [1..n], b <- [1..n], (a < n && b < n) && ((a*b) == sum [x | x <- [1..n], x /= a && x /= b])]

-- 3)
type Point = (Double, Double)
line :: Point -> Point -> (Double -> Double)
line (x1,y1) (x2,y2) = y  
    where y x = y1 + (x-x1) * (y2-y1) / (x2-x1) 

liesOn :: (Double -> Double) -> (Point -> Bool)
liesOn f = y 
    where y (pointX, pointY) = pointY == f pointX
    
diagonal = line (0,0) (1,1)
onDiag   = liesOn diagonal

main :: IO()
main = do
    print $ checkSequence [2, 9, 15]
    print $ checkSequence [11, 14, 20, 27, 31]
    print $ checkSequence [11, 14, 28, 27, 31]
    print $ checkSequence [11, 14, 14, 27, 31]
    ------- 
    print $ checkSequence [2,2,3]

    print $ removeNb 26
    print $ removeNb 100
    print $ removeNb 101
    -------
    print $ removeNb 45
 
    print $ onDiag (5.5, 5.5)
    print $ onDiag (0.5, 0)
    -------
    print $ onDiag (3, 0)
    print $ onDiag (2, 2)
