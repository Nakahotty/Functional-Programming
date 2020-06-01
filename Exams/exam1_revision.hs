import Data.List(inits, tails)

-- ПРЕГОВОР ЗА КОНТРОЛНО

------------ ТЕМА 5. - Функции от по висок ред (функции за работа със списъци) ------
addNum :: Int -> (Int -> Int)
addNum x = addX 
    where addX y = x + y

-- Вместо това ползваме ламбда функции
addNum' x = \ y -> x + y
add = \ x y -> x + y  

inside :: Int -> Int -> (Int -> Bool)
inside a b = \ x -> x >= a && x <= b 

twice :: (a->a) -> (a->a)
twice f = (f . f)

iter :: Int -> (a -> a) -> (a -> a)
iter 0 _ = id -- вградена функция за идентитет
iter n f = f . iter (n-1) f

plusOne :: Int -> Int 
plusOne x = x + 1

squared :: Int -> Int 
squared x = x * x

x2Plus1 :: Int -> Int 
x2Plus1 x = (plusOne . squared) x

-- СЕЧЕНИЕ НА ОПЕРАТОРИ
plusOne' = (+1)
plusX x = (+x)
lessThanFive = (<5)
greatherThanFive = (>5)

------------ ТЕМА 6. Функции от по висок ред -----------
zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys 
zip' _      _      = []
 
qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = qSort [y | y <-xs, y<=x] ++ [x] ++ qSort [y | y <- xs, y > x]

map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs 

filter' :: (a->Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if f x then x : filter' f xs else filter' f xs

hasTwo :: Int -> Bool
hasTwo x 
    | x == 0          = False
    | x `mod` 10 == 2 = True 
    | otherwise       = hasTwo (x `div` 10)

-- isAscending :: Integer -> Bool
-- isAscending num = all ordered (zip digits (tail digits)) 
--     where   
--         numToList x = if x < 10 then [x] else numToList (x `div` 10) ++ [x `mod` 10]
--         digits = numToList num 
--         ordered (x,y) = x <= y

isInteresting :: Int -> Bool
isInteresting num = num `mod` sumOfDigits num == 0
    where 
        sumOfDigits 0 = 0
        sumOfDigits x = (x `mod` 10) + sumOfDigits (x `div` 10)

sumNumbers :: Int -> Int -> Int 
sumNumbers a b = sum [x | x <- [a..b], hasSix x, mod x 4 == 1]
    where 
        hasSix 6 = True 
        hasSix x 
            | x < 10    = False 
            | otherwise = ((mod x 10) == 6 || hasSix (div x 10))    

dominates :: (Int->Int) -> (Int->Int) -> [Int] -> Bool
dominates f g xs = and [ abs (f x) >= abs (g x) | x <- xs]

sumUnique :: [[Int]] -> Int 
sumUnique lst = sum (concat (map clean lst))
    where 
        clean [] = []
        clean (x:xs) = 
            if elem x xs 
                then clean [y | y <- xs, y /= x]
                else x : clean xs

------------ ТЕМА 7. Foldr функции -----------
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x xs@(y:ys) = if x < y then x : xs else y : (insert x ys)

insertionSort :: [Int] -> [Int]
insertionSort xs = foldr insert [] xs

type Point = (Double, Double)

closestPoint :: [Point] -> (Point -> Point)
closestPoint ps (px, py) = foldr1 chooseCloser ps 
    where 
        chooseCloser p1 p2 = if (distance p1 < distance p2) then p1 else p2 
        distance (x,y) = sqrt((px - x)^2 + (py - y)^2)

------------ ТЕМА 8. База от данни -----------
type Product = (String, Int, Float)
type Shop = [Product]

p1,p2,p3,p4 :: Product
p1 = ("milk", 5, 1.20)
p2 = ("cheese", 20, 1.80)
p3 = ("bread", 10, 0.50)
p4 = ("chocolate", 3, 2.00)

shop :: Shop 
shop = [p1,p2,p3,p4]

-- 1) 
getPrice :: Product -> Float 
getPrice (_, _, price) = price 

-- 2)
getTotal :: Shop -> Float
getTotal [] = 0
getTotal ((_,quantity,price) : xs) = fromIntegral quantity * price + getTotal xs

-- 3)
buy :: String -> Int -> Shop -> Shop
buy _ _ [] = error "No such product"
buy name' quantity' (x@(name,quantity,price) : xs)
    | name == name' && quantity' < quantity  = (name, quantity - quantity', price) : xs 
    | name == name' && quantity' == quantity = xs 
    | name == name' && quantity' > quantity  = x : xs   
    | otherwise                              = x : buy name' quantity' xs 

-- 4)
getNeeded :: Int -> Shop -> [Product]
getNeeded needed xs = [x | x@(_, quantity, _) <- xs, quantity <= needed]

-- 5)
getAverage :: Shop -> Float
getAverage xs = sum prices/ fromIntegral (length prices) 
    where prices = [price | (_,_,price) <-xs ]

------------- УПРАЖНЕНИЯ ОТ НАЧАЛОТО --------------

myFactIter :: Int -> Int
myFactIter n = helper n 1 
    where 
        helper 0 acc = acc 
        helper n acc = helper (n-1) (acc*n)

isPrime :: Integer -> Bool
isPrime 1 = False 
isPrime n = helper 2 
    where  
        helper current 
            | current > n `div` 2      = True  --най големият делител на число е половината му
            | (n `mod` current) == 0   = False
            | otherwise                = helper (current + 1)

isPerfect :: Integer -> Bool
isPerfect n = n == (sumDivisors 1 2)
    where
        sumDivisors sum current
            | current > n `div` 2  = sum
            | n `mod` current == 0 = sumDivisors (sum + current) (current + 1)
            | otherwise            = sumDivisors sum (current + 1)

-- Упражнение 3)
reverseNumber :: Integer -> Integer
reverseNumber n = helper n 0
    where 
        helper rest acc 
            | rest < 10 = rest + acc * 10 
            | otherwise = helper (rest `div` 10 ) ((rest `mod` 10) + (acc * 10)) 

isPalindrome :: Integer -> Bool
isPalindrome num = num == reverseNumber num

addPair' :: (Int, Int) -> Int
addPair' (x, y) = x + y

divide :: Int -> Int -> (Int,Int)
divide x y = (x `div` y, x `mod` y)

type Vector = (Double, Double, Double)
v, p :: Vector
v = (1, 1, 1)
p = (2, 3, 5)

sumVectors :: Vector -> Vector -> Vector
sumVectors (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1 + z2)

type Rat = (Int, Int)
equalRat :: Rat -> Rat -> Bool
equalRat (a,b) (c,d) = a*d == b*c

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs 

drop' :: Int -> [a] -> [a]
drop' 0 xs  = xs 
drop' _ []  = []
drop' n (_:xs) = drop' (n-1) xs 

-- Упражнение 4)
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x [] = [x]
insertAt 0 x xs = x : xs 
insertAt n x (y:ys) = y : insertAt (n-1) x ys 

sublistBetween :: Int -> Int -> [a] -> [a]
sublistBetween start end xs = take (end-start) (drop start xs)

chunksOf :: Int -> [a] -> [[a]]
chunksOf size xs = if length xs <= size then [xs] else take size xs : chunksOf size (drop size xs)

isSorted :: [Int] -> Bool 
isSorted []         = True 
isSorted (x:[])     = True
isSorted (x1:x2:xs) = x1 <= x2 && isSorted (x2:xs)

isAscending :: Int -> Bool 
isAscending num = isSorted (digits num)
    where digits x = if x < 10 then [x] else digits (x `div` 10) ++ [x `mod` 10]

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs 
merge [] ys = ys 
merge first@(x:xs) second@(y:ys) 
    | x <= y    = x : merge xs second
    | otherwise = y : merge first ys 

isPrimeComprehension :: Integer -> Bool
isPrimeComprehension 1 = False
isPrimeComprehension n = (length [x | x <- [2..n-1], n `mod` x == 0] == 0)

primesInRange :: Integer -> Integer -> [Integer]
primesInRange a b = [x | x <- [a..b], isPrimeComprehension x]

prodSumDiv :: [Integer] -> Integer -> Integer
prodSumDiv xs k = product [x | x <-xs, (divisors x) `mod` k == 0]
    where divisors num = sum [d | d <- [1..num], num `mod` d == 0]

squares :: Double -> Double -> Double -> [(Double,Double)]
squares start end h = [(x, x*x) | x <- [start, start + h .. end]] 

-- Упражнение 5.
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y:ys) = if x == y then ys else y : removeFirst x ys

removeAll :: Eq a => a -> [a] -> [a]
removeAll _ [] = []
removeAll x (y:ys) = if x == y then removeAll x ys else y : removeAll x ys 

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (removeAll x xs)

prefix :: Eq a => [a] -> [a] -> Bool
prefix xs ys = xs == take (length xs) ys

countOccurences :: Eq a => [a] -> [a] -> Int
countOccurences subxs xs = helper xs 0 
    where
        helper [] cnt            = cnt 
        helper curr@(_:rest) cnt = if prefix subxs curr then helper rest (cnt + 1) else helper rest cnt

-- Упражнение 6.
incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy xs n = map (+n) xs 

filterSmallerThan :: [Int] -> Int -> [Int]
filterSmallerThan xs n = filter (<n) xs 

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f xs = (filter f xs, filter (not . f) xs)

quickSort :: [Int] -> [Int]
quickSort []     = []
quickSort (p:xs) = quickSort smaller ++ [p] ++ quickSort larger
    where (smaller, larger) = partition (<p) xs

-- Упражнение 7.

foldr' :: (a -> b -> b) -> b -> [a] -> b 
foldr' _ v [] = v 
foldr' f v (x:xs) = f x (foldr' f v xs)

foldr1' :: (a->a->a) -> [a] -> a -- когато сме сигурни че ще работим с непразни списъци
foldr1' f [x] = x 
foldr1' f (x:xs) = f x (foldr1' f xs)

concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs 

rev :: [a] -> [a]
rev lst = foldr (\ x xs -> xs ++ [x]) [] lst

main :: IO()    
main = do
    print $ incrementAllBy [1,2,3] 12
    print $ quickSort [1,32,14,34,235,1,23,21321,3]
    print $ foldr1' (+) [1,2,3,4]
    print $ foldr1' (||) [False, True, True]
    print $ rev [1,2,3,4,5]
    print $ map (add 4) [1,2,3,4] 