import Data.List(nub)
import Data.Char(isLetter)

-- 6)
isImage :: [Int] -> [Int] -> Bool
isImage as@(a:_) bs@(b:_) = map (+ (a-b)) bs == as

-- 7)
isTriangle :: [[Int]] -> Bool
isTriangle [] = True
isTriangle mss = all (== 0) (tail (map head mss)) && isTriangle (tail (map tail mss))

matrix :: [[Int]]
matrix = [[1,2,3],
          [0,5,6],
          [0,0,9]]

-- Допълнителни
-- 1)
signCheck :: [Int] -> Bool
signCheck []  = True
signCheck [_] = True
signCheck (x1:x2:xs) = (x1 * x2 < 0) && signCheck (x2:xs)

-- 3)
countLetters :: String -> [(Char, Int)]
countLetters word = [(letter, count letter word) | letter <- letters]
    where 
        letters = filter isLetter (nub word)
        count x xs = length [y | y <- xs, y == x]

main :: IO()
main = do
    print (isImage [1,2,3] [4,5,6])
    print (isTriangle [[1,2,2],[0,5,6],[0,0,9]])
    print (signCheck [1,-2,3,-4])
    print (countLetters "This")