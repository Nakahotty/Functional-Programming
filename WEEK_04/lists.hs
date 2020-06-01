a :: [Int]
a = [1,2,3]

null' :: [a] -> Bool
null' [] = True
null' _  = False

head' :: [a] -> a
head' (x : _) = x

tail' :: [a] -> [a]
tail' (_ : xs) = xs

sum' :: [Int] -> Int
sum' xs = if null xs then 0 else head' xs + sum'(tail xs)

sum'' :: [Int] -> Int
sum'' [] = 0
sum'' (x:xs) = x + sum'' xs

length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

elem' :: Int -> [Int] -> Bool
elem' _ []     = False
elem' y (x:xs) = x == y || elem' y xs

-- Взима n броя елементи от началото
take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

-- Премахва n броя елементи от началото
drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (x:xs) = drop' (n-1) xs

minimum' :: [Int] -> Int
minimum' (x:xs) = helper xs x
    where 
        helper [] min     = min
        helper (x:xs) min = if x < min then helper xs x else helper xs min 

-- Конкатенация
append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

-- Обръщане на списък
reverse' :: [a] -> [a]
reverse' xs = helper [] xs 
    where
        helper :: [a] -> [a] -> [a]
        helper acc []     = acc
        helper acc (y:ys) = helper (y:acc) ys

-- Превръщане на два списък в списък с двойки
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (a:as) (b:bs) = (a,b) : zip' as bs

main :: IO()
main = do
    print (minimum' [3,4,-3,2])
    print (append [1,2,3] [4,5,6])
    print (reverse' [1,2,3])
    print (zip' [1,2,3,4] [5,6,7,9])