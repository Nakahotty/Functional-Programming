myFib :: Int -> Int
myFib n = if (n == 0 || n == 1) then 1 else myFib(n - 1) + myFib(n - 2)

myNot :: Bool -> Bool
myNot True = False
myNot False = True

-- 1)
countDigits :: Int -> Int
countDigits n 
    | n < 10    = 1
    | otherwise = 1 + countDigits(n `div` 10)

-- 2)
sumDigitsIter :: Int -> Int
sumDigitsIter n = helper n 0
    where 
        helper 0 sum = sum
        helper number sum = helper (number `div` 10) (sum + (number `mod` 10))  

-- 3)
countOccurences :: Integer -> Integer -> Integer
countOccurences n digit
    | n < 10 && n /= digit = 0
    | n < 10 && n == digit = 1
    | n `mod` 10 == digit  = 1 + countOccurences(n `div` 10) digit
    | otherwise            = countOccurences(n `div` 10) digit

-- 4)
isAscending :: Int -> Bool
isAscending n 
    | n < 10                             = True
    | (n `div` 10) `mod` 10 < n `mod` 10 = isAscending(n `div` 10)          
    | otherwise                          = False

-- 5)
isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = helper (n - 1)
    where
        helper curr
            | curr == 1           = True
            | (n `mod` curr) == 0 = False
            | otherwise           = helper (curr - 1)

main :: IO()
main = do
    print (countOccurences 12333 3)