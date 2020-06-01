doubleAll :: [Int] -> [Int]
doubleAll xs = map f xs
    where
        f :: Int -> Int
        f x = 2 * x

double :: Int -> Int
double x = 2 * x

bigger :: Int -> Bool
bigger x = x > 10

-- !!! Apply a function on all elements
map' :: (Int -> Int) -> [Int] -> [Int]
map' f xs = [f x | x <- xs]

-- !!! Check a condition and return the elements that pass it
filter' :: (Int->Bool) -> [Int] -> [Int]
filter' f xs = [x | x <- xs, f x]

-- !!! Combination of zip and map
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' f _      _      = []    

-- Multiply the elements from the two lists
mult :: Int -> Int -> Int
mult a b = a * b

plus :: Int -> Int -> Int
plus a b = a + b

-- !!! Applying a function to elements from start to end by order
foldr1' :: (a->a->a) -> [a] -> a
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs) 

-- The same but it has a starting value
foldr' :: (a->a->a) -> a -> [a] -> a
foldr' f s [] = s
foldr' f s (x:xs) = f x (foldr' f s xs)

main :: IO()
main = do
    print (map' double [2,5,3])
    print (filter' bigger [2,344,33,12,2])
    print (zipWith' mult [1,2,3] [4,5,6])
    print (foldr1' mult [1..5])
    print (foldr1' plus [1..7]) -- Aritmetic progression
    print (foldr' mult 10 [1..5])