suma :: [Int] -> Int
suma []     = 0
suma (x:xs) = x + suma xs

elem' :: Int -> [Int] -> Bool
elem' x []     = False
elem' x (y:ys) = x == y || elem' x ys

doubleAll :: [Int] -> [Int]
doubleAll xs = [2*x | x <- xs]

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
    | x <= y    = x : (y:ys)
    | otherwise = y : (ins x ys)

sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = ins x (sort xs)

zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
zip' _ _ = []

main :: IO()
main = do
    print (elem' 17 [1,2,3,4,5])
    print (doubleAll [3,3,4])
    print (sort [2,34324,454,22,3])
    print (zip' [1,2,3] [4,5,6])