import Data.List

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs) = p x || any' p xs

all' :: (a->Bool) -> [a] ->  Bool
all' _ [] = True
all' f (x:xs) = f x && all' f xs

-- Map
map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- Filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if f x then x : filter' f xs else filter' f xs

-- Zip With (combo of zip and map)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

-- Digits of a number are ascending
isAscending :: Integer -> Bool
isAscending num = all' (\(x,y) -> x <= y) (zip digits (tail digits))
    where 
        numToList x = if x < 10 then [x] else numToList (x `div` 10) ++ [x `mod` 10]
        digits = numToList num 

-- 1) 
incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy xs n = map (+n) xs

multiplyAllBy :: [Int] -> Int -> [Int]
multiplyAllBy xs n = map (\x -> x * n) xs

filterSmallerBy :: [Int] -> Int -> [Int]
filterSmallerBy xs n = filter (\x -> x < n) xs

-- 2)
splitByParity :: [Int] -> ([Int], [Int])
splitByParity xs = (filter odd xs, filter even xs)  

-- 3)
partition' :: (a->Bool) -> [a] -> ([a],[a])
partition' f xs = (filter f xs, filter (not . f) xs)

-- 4)
splitByParity' :: [Int] -> ([Int], [Int])
splitByParity' xs = partition' odd xs

-- 5) 
quickSort :: [Int] -> [Int]
quickSort []     = []
quickSort (x:xs) = (quickSort smaller) ++ [x] ++ (quickSort larger)
    where 
        (smaller, larger) = partition (<x) xs 

main :: IO()
main = do
    -- print (any' odd [2,2,2])
    -- print (all' odd [1,3,6])
    -- print (map' (\x -> x*x) [2,4,5])
    -- print (filter' odd [1,2,3,4,5])
    -- print (zipWith' (+) [1,2,3] [1,1,1])
    -- print (length (filter' (\ x -> x <=5) [1,2,44,44]))
    -- print (filterSmallerBy [1,2,3] 3)
    -- print (splitByParity [1,2,3,4,5,6])    
    
    -- [1,2,3,4] zip with tail xs => [(1,2) (2,3) (3,4)]
    print (isAscending 234)

    print (partition' odd [1,2,3,4,5,6,7])
    print (splitByParity' [1,2,3,4,5,6,7])
    print (quickSort [3,4,1,2,5,4])