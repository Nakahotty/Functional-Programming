-- Конструкция на списък чрез определяне на неговия обхват
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = length [x | x <- [2..n-1], n `mod` x == 0] == 0

isPerfect :: Integer -> Bool
isPerfect n = sum ([x | x <- [1..n-1], n `mod` x == 0]) == n

primeIsInRange :: Integer -> Integer -> [Integer]
primeIsInRange a b = [x | x <- [a..b], isPrime x]

perfectsInRange :: Integer -> Integer -> [Integer]
perfectsInRange a b =  [x | x <- [a..b], isPerfect x]

prodSumDiv :: [Integer] -> Integer -> Integer
prodSumDiv xs k = product [x | x <- xs, (sumDiv x) `mod` k == 0]
    where 
        sumDiv num = sum ([y | y <- [1..num], num `mod` y == 0])



main :: IO()
main = do
    print [x | x <- [2..11], 12 `mod` x == 0]
    print (isPerfect 6)
    print (primeIsInRange 1 10)
    print (perfectsInRange 1 100)
    print (prodSumDiv [1..10] 3)